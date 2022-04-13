import imp
from multiprocessing.sharedctypes import Value
from lark import Lark, Transformer, v_args, exceptions
from numpy import dstack
from parsimonious import rule
from smt.veriT.command import Assume, Step
from logic import context
from kernel import term as hol_term
from kernel import type as hol_type
from syntax import parser as hol_parser


# Grammar of SMT-LIB language
smt_decl_grammar = r"""
    ?term: "(declare-fun" CNAME "()" CNAME ")" -> mk_tm
        | "(declare-fun" CNAME "(" CNAME+ ")" CNAME ")" -> mk_fun

    %import common.CNAME
    %import common.INT
    %import common.DIGIT
    %import common.DECIMAL
    %import common.WS
    %import common.NUMBER
    %ignore WS
"""
@v_args(inline=True)
class DeclTransformer(Transformer):
    """A parser for declaration in SMT-LIB."""
    def __init__(self):
        pass

    def type_str(self, s):
        s = str(s)
        return s.lower() if s in ("Bool", "Real", "Int") else s

    def mk_tm(self, name, ty):
        "Make a term: name :: ty"
        return {name.value: self.type_str(ty)}

    def mk_fun(self, name, *args):
        """Make a function term, which type is arg1 -> ... argn"""
        T = " => ".join(self.type_str(t) for t in args)
        return {name.value: T}

decl_parser = Lark(smt_decl_grammar, start="term", parser="lalr", transformer=DeclTransformer())

def parse_decl(s):
    return decl_parser.parse(s)
# Grammar of Alethe proof
veriT_grammar = r"""
    ?proof_command : "(assume" CNAME proof_term ")" -> mk_assume
                    | "(step" CNAME clause ":rule" CNAME step_annotation? ")" -> mk_step
 
    ?clause : "(cl" proof_term* ")" -> mk_clause
    
    ?step_annotation : ":premises" "(" CNAME+ ")" -> mk_premises

    ?proof_term : term

    ?let_pair : "(" "?" CNAME term ")" -> mk_let_pair

    ?term :   "true" -> mk_true
            | "false" -> mk_false
            | "(not" term ")" -> mk_neg_tm
            | "(or" term+ ")" -> mk_disj_tm
            | "(and" term+ ")" -> mk_conj_tm
            | "(=>" term term ")" -> mk_impl_tm
            | "(=" term term ")" -> mk_eq_tm
            | "(!" term ":named" "@" CNAME ")" -> mk_annot_tm
            | "(let" "(" let_pair+ ")" term ")" -> mk_let_tm
            | "(distinct" term term+ ")" -> mk_distinct_tm
            | "(" term ")" -> mk_par_tm
            | "(" term+ ")" -> mk_app_tm
            | name

    ?name : "@" CNAME -> ret_annot_tm
            | "?" CNAME -> ret_let_tm
            | CNAME -> ret_tm

    %import common.CNAME
    %import common.INT
    %import common.DIGIT
    %import common.DECIMAL
    %import common.WS
    %import common.NUMBER
    %ignore WS
"""

@v_args(inline=True)
class ProofTransformer(Transformer):
    """A parser for alethe proof grammar.
    
    ctx: map symbols to higher-order terms
    """
    def __init__(self, ctx):
        # context.set_context("verit", vars=ctx)
        self.ctx = ctx 
        
        # map from annotation to the term
        self.annot_tm = dict()

        # map from local variables to term
        self.let_tm = dict()


    def ret_annot_tm(self, name):
        name = "@" + str(name)
        return self.annot_tm[name]

    def ret_let_tm(self, name):
        name = "?" + str(name)
        return self.let_tm[name]

    def mk_par_tm(self, tm):
        return tm

    def mk_app_tm(self, *tms):
        return tms[0](*tms[1:])

    def mk_let_pair(self, name, tm):
        name = "?" + str(name)
        
        self.let_tm[name] = tm

    def mk_let_tm(self, *tms):
        return tms[-1]

    def ret_tm(self, tm):
        # if str(tm) in self.annot_tm:
        #     return self.annot_tm[str(tm)]
        # if str(tm) in self.let_tm:
        #     return self.let_tm[str(tm)]
        if str(tm) not in self.ctx:
            raise ValueError(str(tm))      
        return hol_term.Const(str(tm), hol_type.TConst(self.ctx[str(tm)]))

    def mk_distinct_tm(self, *tms):
        neq_tm = []
        for i in range(len(tms)):
            for j in range(i+1, len(tms)):
                neq_tm.append(hol_term.Not(hol_term.Eq(tms[i], tms[j])))

        return hol_term.And(*neq_tm)

    def mk_true(self):
        return hol_term.true

    def mk_false(self):
        return hol_term.false

    def mk_neg_tm(self, tm):
        # if tm in self.annot_tm:
        #     tm = self.annot_tm[str(tm)]
        # print(str(tm))
        return hol_term.Not(tm)

    def mk_annot_tm(self, tm, name):
        name = "@" + str(name)
        self.annot_tm[name] = tm
        return tm

    def mk_disj_tm(self, *tms):
        return hol_term.Or(*tms)

    def mk_conj_tm(self, *tms):
        return hol_term.And(*tms)

    def mk_impl_tm(self, *tms):
        return hol_term.Implies(*tms)

    def mk_eq_tm(self, *tms):
        return hol_term.Eq(*tms)

    def mk_assume(self, id, tm):
        return Assume(id, tm)

    def mk_step(self, id, cl, rule_name, pm=None):
        if pm is not None:
            pm = [p.value for p in pm]
        return Step(id, rule_name, cl, pm)

    def mk_clause(self, *tm):
        if len(tm) == 0:
            return hol_term.false
        return hol_term.Or(*tm)

    def mk_premises(self, *pm):
        return pm

def proof_parser(ctx):
    return Lark(veriT_grammar, start="proof_command", parser="lalr", transformer=ProofTransformer(ctx=ctx))
