import z3
from z3.z3consts import *

from kernel.type import TFun, BoolType, NatType, IntType, RealType, STVar
from kernel.term import *
from kernel.thm import Thm
from kernel.proofterm import ProofTerm
from kernel import theory
from logic import basic, context, matcher
from logic.logic import apply_theorem, resolution

from collections import deque, namedtuple
from functools import reduce
import operator

# Z3 proof method name.
method = ('mp', 'mp~', 'asserted', 'trans', 'monotonicity', 'rewrite', 'and-elim', 'not-or-elim',
            'iff-true', 'iff-false', 'unit-resolution', 'commutativity', 'def-intro', 'apply-def', 'lambda',
            'def-axiom', 'iff~', 'nnf-pos', 'nnf-neg', 'sk')

Node = namedtuple('Node', ['term', 'tree'])

def Z3Term(proof):
    """Index all terms in z3 proof."""
    s = dict()
    id = 0
    def rec(term):
        nonlocal id
        if term not in s.keys():
            s[term] = id
            id += 1
        if term.decl().name() in method:
            for child in term.children():
                rec(child)
    rec(proof)
    return {value: key for key, value in s.items()}

def Z3TermGraph(proof):
    """Relation between proof terms."""
    r = dict()
    c = Z3Term(proof)
    s = {value: key for key, value in c.items()}
    for i in range(len(s)):
        r[i] = []
    def rec(proof):
        for child in proof.children():
            if proof in s.keys():
                r[s[proof]].append(s[child])
            if child.decl().name() in method:
                rec(child)

    rec(proof)
    return {key: list(dict.fromkeys(value).keys()) for key, value in r.items()}

def DepthFirstOrder(G):
    """Traverse graph in reversed DFS order."""
    reversePost = deque()
    marked = [False for i in range(len(G))]
    
    def dfs(G, v):
        marked[v] = True
        for w in G[v]:
            if not marked[w]:
                dfs(G, w)
        reversePost.append(v)

    for v in G.keys():
        if not marked[v]:
            dfs(G, v)
    
    return reversePost

def translate_type(T):
    """Translate z3 type into holpy type."""
    if T == Z3_BOOL_SORT:
        return BoolType
    elif T == Z3_INT_SORT:
        return NatType
    elif T == Z3_REAL_SORT:
        return RealType
    else:
        raise NotImplementedError

# def translate_term(term, *args, isNat=False):
#     """Convert a z3 term into holpy term. If term is a variable in nat, its type is NatType."""
#     assert all(isinstance(arg, Term) for arg in args)

#     kind = term.decl().kind() # kind of the term  
#     name = term.decl().name() # name of the term
#     sort = term.sort_kind() #sort of the term
    
#     if z3.is_int_value(term):
#         n = term.as_long()
#         if n < 0:
#             return Int(n)
#         return Nat(n) if isNat else Int(n)
#     elif z3.is_algebraic_value(term):
#         return Real(term.as_fraction())
#     elif name == 'true':
#         return true
#     elif name == 'false':
#         return false
#     elif z3.is_const(term): # note the difference of variable notion between SMT and HOL
#         return Var(name, NatType) if isNat else Var(name, translate_type(sort))
#     elif kind == Z3_OP_UMINUS: # -x
#         return -Var(term.arg(0).decl().name(), translate_type(term.sort_kind()))
#     elif kind == Z3_OP_NOT:
#         return Not(args[0]) # ¬x
#     elif kind == Z3_OP_ADD: # (+ x y z)
#         return sum(args)
#     elif kind == Z3_OP_SUB: # (- x y z)
#         return reduce(operator.sub, args[1:], args[0])
#     elif kind == Z3_OP_MUL: # (* x y z)
#         return reduce(operator.mul, args[1:], args[0])
#     elif kind == Z3_OP_DIV: # (/ x y z)
#         return reduce(operator.truediv, args[1:], args[0])
#     elif kind == Z3_OP_AND: # (and x y z)
#         return And(*args)
#     elif kind == Z3_OP_OR: # (or x y z)
#         return Or(*args)
#     elif kind == Z3_OP_IMPLIES: # (=> x y)
#         return Implies(*args) # note: z3 implies is binary.
#     elif kind == Z3_OP_EQ: # (= x y)
#         return Eq(*args)
#     elif kind == Z3_OP_GT: # (> x y)
#         return greater(translate_type(sort))(*args)
#     elif kind == Z3_OP_GE: # (≥ x y)
#         return greater_eq(translate_type(sort))(*args)
#     elif kind == Z3_OP_LT: # (< x y)
#         return less(translate_type(sort))(*args)
#     elif kind == Z3_OP_LE: # (≤ x y)
#         return less_eq(translate_type(sort))(*args)
#     elif kind == Z3_OP_UNINTERPRETED: # s(0)
#         types = [translate_type(term.arg(i).sort_kind()) for i in range(term.num_args())]
#         print(*types)
#         f = Var(name, TFun(types))
#         return f(*args)
#     else:
#         raise NotImplementedError

def sep_expr(term, typ):
    """Separate the z3 addition, subtraction or multiplication expression term."""
    assert z3.is_add(term) or z3.is_mul(term) or z3.is_sub(term)
    children = []
    def rec(term, typ=''):
        if typ == 'add':
            if z3.is_add(term):
                for c in term.children():
                    rec(c, 'add')
            else:
                children.append(term)
        elif typ == 'sub':
            if z3.is_sub(term):
                for c in term.children():
                    rec(c, 'sub')
            else:
                children.append(term)
        elif typ == 'mul':
            if z3.is_mul(term):
                for c in term.children():
                    rec(c, 'mul')
            else:
                children.append(term)
        
    rec(term, typ)
    return children


def translate(term, vars=[], isNat=True):

    if isinstance(term, z3.FuncDeclRef):
        arity = term.arity()
        rangeT = translate_type(term.range().kind())
        domainT = [translate_type(term.domain(i).kind()) for i in range(arity)]
        types = domainT + [rangeT]
        return Var(term.name(), TFun(*types))
    elif isinstance(term, z3.ExprRef):
        kind = term.decl().kind() # kind of the term  
        name = term.decl().name() # name of the term
        sort = term.sort_kind() #sort of the term

        if z3.is_int_value(term):
            n = term.as_long()
            if n < 0:
                return Int(n)
            return Nat(n) if isNat else Int(n)
        elif z3.is_algebraic_value(term):
            return Real(term.as_fraction())
        elif name == 'true':
            return true
        elif name == 'false':
            return false
        elif z3.is_const(term): # note the difference of variable notion between SMT and HOL
            return Var(name, NatType) if name in vars else Var(name, IntType)
        elif kind == Z3_OP_ADD:
            # bug: sum() occurs 0 in the start.
            children = sep_expr(term, 'add')
            # If term looks like x + (-1)*y convert it to x - y
            # So far assume the first item is positive in nat.
            if isNat:
                addition = translate(children[0], vars)
                for c in children[1:]:
                    if z3.is_mul(c): # -1 * y
                        c_children = sep_expr(c, 'mul')
                        if z3.is_int_value(c_children[0]) and c_children[0].as_long() < 0:
                            addition -= translate(reduce(operator.mul, c_children[2:], c_children[1]), vars)
                        else:
                            addition += translate(c, vars)
                    else:
                        addition += translate(c, vars)
                return addition
            return reduce(lambda x, y: x + y, [translate(term.arg(i), vars) for i in range(term.num_args())])
        elif kind == Z3_OP_SUB: # dual
            children = sep_expr(term, 'sub')
            if isNat:
                addition = translate(children[0], vars)
                for c in children[1:]:
                    if z3.is_mul(c): # -1 * y
                        c_children = sep_expr(c, 'mul')
                        if z3.is_int_value(c_children[0]) and c_children[0].as_long() < 0:
                            addition += translate(reduce(operator.mul, c_children[2:], c_children[1]), vars)
                        else:
                            addition -= translate(c, vars)
                    else:
                        addition -= translate(c, vars)
                return addition
            return reduce(lambda x, y: x + y, [translate(term.arg(i), vars) for i in range(term.num_args())])
        elif kind == Z3_OP_MUL:
            args = [translate(term.arg(i), vars) for i in range(term.num_args())]
            return reduce(operator.mul, args[1:], args[0])
        elif kind == Z3_OP_EQ:
            t_lhs = translate(term.arg(0), vars)
            t_rhs = translate(term.arg(1), vars)
            return Eq(t_lhs, t_rhs)
        elif kind == Z3_OP_NOT:
            return Not(translate(term.arg(0), vars))
        elif kind == Z3_OP_AND:
            args = [translate(term.arg(i), vars) for i in range(term.num_args())]
            return And(*args)
        elif kind == Z3_OP_OR:
            args = [translate(term.arg(i), vars) for i in range(term.num_args())]
            return Or(*args)
        elif kind == Z3_OP_UNINTERPRETED: # s(0)
            uf = translate(term.decl())
            args = [translate(term.arg(i), vars) for i in range(term.num_args())]
            return uf(*args)
        else:
            raise NotImplementedError
    else:
        raise NotImplementedError

def and_elim(pt, concl):
    context.set_context('logic_base')
    r = dict()
    def rec(pt):
        if pt.prop.is_conj():
            rec(apply_theorem('conjD1', pt))
            rec(apply_theorem('conjD2', pt))
        else:
            r[pt.prop] = pt
    rec(pt)
    return r[concl]

def monotonicity(*pts):
    ptl = pts[-1]
    lhs, rhs = ptl.arg1, ptl.arg
    assert lhs.head == rhs.head
    pf = ProofTerm.reflexive(lhs.head)

    new_pf = []
    if len(lhs.args) == 2:
        if lhs.arg1 == rhs.arg1:
            new_pf = [ProofTerm.reflexive(lhs.arg1)] + [pts[0]]
        elif lhs.arg == rhs.arg:
            new_pf = [pts[0]] + [ProofTerm.reflexive(lhs.arg)]
        else:
            new_pf = pts[:-1]
    else:
        new_pf = pts[:-1]

    for pt in new_pf:
        pf = pf.combination(pt)

    return pf

def schematic_rules(thms, lhs, rhs):
    """Rewrite by instantiating schematic theorems."""
    context.set_context('smt')
    for thm in thms:
        context.set_context('smt')
        pt = ProofTerm.theorem(thm)
        try:
            inst1 = matcher.first_order_match(pt.prop.lhs, lhs)
            inst2 = matcher.first_order_match(pt.prop.rhs, rhs, inst=inst1)
            return pt.substitution(inst1)
        except matcher.MatchException:
            continue
    return None

def rewrite(t):
    def norm_nat(t):
        """Use nat norm macro to normalize nat expression."""
        assert t.is_equals() and t.lhs.get_type() == NatType and t.rhs.get_type() == NatType
        context.set_context('nat')
        macro = theory.global_macros['nat_norm']
        return macro.get_proof_term(t, [])

    def equal_is_true(pt):
        """pt is ⊢ x = y, return: ⊢ (x = y) ↔ true"""
        context.set_context('logic_base')
        pt0 = apply_theorem('trueI') # ⊢ true
        pt1 = pt0.implies_intr(pt.prop) # ⊢ (x = y) → true
        pt2 = pt.implies_intr(pt0.prop) # ⊢ true → (x = y)
        return ProofTerm.equal_intr(pt1, pt2)

    if t.lhs == t.rhs:
        return ProofTerm.reflexive(t.lhs)

    pt = schematic_rules(['r001', 'r002', 'r038', 'r062'], t.lhs, t.rhs)  # rewrite by schematic theorems 
    if pt is None:
        if t.rhs == true and t.lhs.is_equals(): # prove ⊢ (x = y) ↔ true
            eq = t.lhs
            if eq.lhs.get_type() == NatType: # Maybe can reuse schematic theorems to prove ⊢ (x = y) in further
                pt_eq = norm_nat(eq)
                return equal_is_true(pt_eq)
            else:
                raise NotImplementedError
        elif t.is_equals(): # Equations that can't match with schematic theorems
            # Try nat norm macro:
            if t.lhs.get_type() == NatType:
                return norm_nat(t)
            else:
                raise NotImplementedError
        else:
            raise NotImplementedError
    else:
        return pt  


def convert_method(term, *args):
    name = term.decl().name()
    if name == 'asserted': # {P} ⊢ {P}
        return ProofTerm.assume(args[0])
    elif name == 'and-elim':
        arg1, arg2 = args
        return and_elim(arg1, arg2)
    elif name == 'monotonicity':
        return monotonicity(*args)
    elif name == 'trans':
        arg1, arg2, _ = args
        return arg1.transitive(arg2)
    elif name == 'mp':
        arg1, arg2, _ = args
        return ProofTerm.equal_elim(arg2, arg1)
    elif name == 'rewrite' or name == 'commutativity':
        arg1, = args
        return rewrite(arg1)
    elif name == 'unit-resolution':
        for arg in args:
            print('arg: ', arg)
        arg1, arg2, arg3 = args
        return resolution(arg1, arg2)
    else:
        raise NotImplementedError
    


def proofrec(proof):
    term = Z3Term(proof)
    net = Z3TermGraph(proof)
    order = DepthFirstOrder(net)
    r = dict()

    for i in order:
        name = term[i].decl().name()
        args = (r[j] for j in net[i])
        # print('term['+str(i)+']', term[i])
        if name not in method:
            r[i] = translate(term[i], ['A', 'B'])
        else:
            r[i] = convert_method(term[i], *args)
            basic.load_theory('nat')
            # print('r['+str(i)+']', r[i])
    return r[0]