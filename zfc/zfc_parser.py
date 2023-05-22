"""Parsing of ZFC terms."""

from lark import Lark, Transformer, v_args, exceptions

from kernel import type as hol_type
from kernel.type import BoolType, TFun
from kernel import term as hol_term
from kernel import theory
from zfc import zfc


class ZFCParserException(Exception):
    """Exceptions during parsing."""
    def __init__(self, str):
        self.str = str

class ZFCElaborateException(Exception):
    """Exceptions during elaboration."""
    def __init__(self, str):
        self.str = str


grammar = r"""
    ?atom: CNAME -> vname                                   // Constant, variable, or bound variable
        | "1" -> unit                                       // Unit
        | ("!"|"∀") CNAME ":" term ". " term -> all         // Forall quantification
        | ("?"|"∃") CNAME ":" term ". " term -> exists      // Exists quantification
        | "THE" CNAME ":" term ". " term -> the             // THE operator
        | "{" CNAME ":" term ". " term "}" -> collect_set   // Collect operator
        | atom "(" term ("," term)* ")" -> set_comb         // Function application (set-theoretic)
        | CNAME ":" term "->" term ":" term -> func_def     // Function definition
        | CNAME ":" term "," CNAME ":" term "->" term ":" term -> func_def2
        | "[" term "]" -> dummy_known                       // Dummy known struct
        | "(" term ")"                                      // Parenthesis

    ?comb: comb atom
        | atom
    
    ?times_expr: times_expr "*" comb -> times               // Multiplication: priority 70
        | times_expr "*" "[" term "]" comb -> times_str
        | comb

    ?inter: inter ("Int"|"∩") times_expr                    // Intersection: priority 70
        | times_expr

    ?plus_expr: plus_expr "+" inter -> plus                 // Addition: priority 65
        | plus_expr "-" inter -> minus                      // Subtraction: priority 65
        | inter

    ?union: union ("Un"|"∪") plus_expr                      // Union: priority 65
        | plus_expr

    ?comp_fun: union ("O"|"∘") comp_fun                     // Function composition: priority 60
        | union

    ?func_set: comp_fun ("=>"|"⇒") comp_fun -> func_set    // Function sets: priority 55
        | "(" comp_fun "," comp_fun ")" ("=>"|"⇒") comp_fun -> func_set2
        | comp_fun

    ?eq: eq "=" func_set | func_set                         // Equality: priority 50

    ?mem: mem ("Mem"|"∈") mem | eq                          // Membership: priority 50

    ?subset: subset ("Sub"|"⊆") subset | mem                // Subset: priority 50

    ?less_eq: less_eq ("<="|"≤") less_eq | subset           // Less-equal: priority 50

    ?less: less "<" less | less_eq                          // Less: priority 50

    ?greater_eq: greater_eq (">="|"≥") greater_eq | less    // greater-equal: priority 50

    ?greater: greater ">" greater | greater_eq              // greater: priority 50

    ?neg: ("~"|"¬") neg -> neg | greater                    // Negation: priority 40

    ?conj: neg ("&"|"∧") conj | neg                         // Conjunction: priority 35

    ?disj: conj ("|"|"∨") disj | conj                       // Disjunction: priority 30

    ?iff: disj ("<-->"|"⟷") iff | disj                     // Iff: priority 25

    ?imp: iff ("-->"|"⟶") imp | iff                        // Implies: priority 20

    ?term: imp

    ?arg_decl: CNAME "::" CNAME ("(" term ("," term)* ")")? ";" -> struct_arg_decl
        | CNAME ":" term ";" -> member_arg_decl

    ?arg_decls: ("arguments" arg_decl*)? -> arg_decls

    ?param_decl: ("const" | "func") CNAME ":" term ";"

    ?param_decls: ("parameters" param_decl*)? -> param_decls

    ?assum_decls: ("assumes" (term ";")*)? -> assum_decls

    ?inher_decl: term "::" CNAME ("(" term ("," term)* ")")? ";"

    ?inher_decls: ("inherits" inher_decl*)? -> inher_decls

    ?property_decl: CNAME ":" term ";"

    ?property_decls: ("properties" property_decl*)? -> property_decls

    ?structure: "struct" CNAME "(" "set" CNAME ")" "{" arg_decls param_decls inher_decls property_decls "}"

    ?fix_decls: ("fixes" arg_decl*)? -> fix_decls

    ?shows_decl: term "::" CNAME ("(" term ("," term)* ")")? -> struct_shows_decl
        | term -> prop_shows_decl

    ?param_inst: CNAME ":" term ";"

    ?param_insts: ("with" param_inst*)? -> param_insts

    ?theorem: "theorem" CNAME ":" fix_decls assum_decls "shows" shows_decl param_insts

    ?definition: "definition" CNAME ":" fix_decls assum_decls "defines" term

    ?morphism: "morphism" CNAME "(" CNAME ":" CNAME "->" CNAME ")" "{" inher_decls property_decls "}"

    ?zfc_item: structure | theorem | definition | morphism

    %import common.CNAME
    %import common.WS
    %import common.INT
    %import common.LETTER
    %import common.DIGIT

    %ignore WS        
"""

@v_args(inline=True)
class ZFCTransformer(Transformer):
    def __init__(self):
        pass

    def vname(self, s):
        s = str(s)
        if theory.thy.has_term_sig(s):
            # s is the name of a constant in the theory
            T = theory.thy.get_term_sig(s)
            res = hol_term.Const(s, T)
            argTs, retT = T.strip_type()
            for argT in argTs:
                if argT == zfc.structT:
                    res = res(zfc.dummyStruct)
            return res
        else:
            # s not found, either bound or free variable
            return hol_term.Var(s, zfc.setT)
        
    def dummy_known(self, t):
        return zfc.dummyKnownStruct(t)

    def unit(self):
        return zfc.unit(zfc.dummyStruct)

    def all(self, v, s, p):
        return zfc.ForallS(str(v), s, p)

    def exists(self, v, s, p):
        return zfc.ExistsS(str(v), s, p)

    def the(self, v, s, p):
        return zfc.TheS(str(v), s, p)
    
    def collect_set(self, v, s, p):
        return zfc.CollectS(str(v), s, p)

    def set_comb(self, func, *args):
        if len(args) == 1:
            return zfc.app(func, args[0])
        elif len(args) == 2:
            return zfc.app2(func, args[0], args[1])
        else:
            raise NotImplementedError
        
    def func_def(self, v, S, t, T):
        return zfc.Func(S, T, hol_term.Lambda(zfc.V(v), t))
    
    def func_def2(self, v, S1, w, S2, t, T):
        return zfc.Func2(S1, S2, T, hol_term.Lambda(zfc.V(v), zfc.V(w), t))
    
    def comb(self, fun, arg):
        if arg.head == zfc.dummyKnownStruct:
            assert fun.is_comb()
            return hol_term.Comb(fun.fun, arg)
        else:
            return hol_term.Comb(fun, arg)

    def times(self, lhs, rhs):
        return zfc.timesOp(zfc.dummyStruct, lhs, rhs)
    
    def times_str(self, lhs, struct, rhs):
        return zfc.timesOp(zfc.dummyKnownStruct(struct), lhs, rhs)

    def plus(self, lhs, rhs):
        return zfc.plusOp(zfc.dummyStruct, lhs, rhs)
    
    def minus(self, lhs, rhs):
        return zfc.minusOp(zfc.dummyStruct, lhs, rhs)
    
    def less_eq(self, lhs, rhs):
        return hol_term.Const("less_eq", None)(lhs, rhs)

    def less(self, lhs, rhs):
        return hol_term.Const("less", None)(lhs, rhs)

    def greater_eq(self, lhs, rhs):
        return hol_term.Const("greater_eq", None)(lhs, rhs)

    def greater(self, lhs, rhs):
        return hol_term.Const("greater", None)(lhs, rhs)

    def eq(self, lhs, rhs):
        return hol_term.Const("equals", TFun(zfc.setT, zfc.setT, BoolType))(lhs, rhs)

    def neg(self, t):
        return hol_term.Not(t)

    def conj(self, s, t):
        return hol_term.And(s, t)

    def disj(self, s, t):
        return hol_term.Or(s, t)

    def imp(self, s, t):
        return hol_term.Implies(s, t)

    def iff(self, s, t):
        return hol_term.Const("equals", TFun(BoolType, BoolType, BoolType))(s, t)

    def mem(self, x, A):
        return zfc.memSet(x, A)

    def subset(self, A, B):
        return zfc.subSet(A, B)

    def inter(self, A, B):
        return hol_term.Const("inter", None)(A, B)

    def union(self, A, B):
        return hol_term.Const("union", None)(A, B)

    def comp_fun(self, f, g):
        return zfc.composeFun(f, g)

    def func_set(self, S, T):
        return zfc.funcSet(S, T)
    
    def func_set2(self, S, T, U):
        return zfc.funcSet2(S, T, U)
    
    def struct_arg_decl(self, name, struct_name, *args):
        return (str(name), zfc.StructureProp(zfc.V(str(name)), struct_name, list(args)))
    
    def member_arg_decl(self, name, t):
        return (str(name), zfc.MembershipProp(zfc.V(str(name)), t))

    def arg_decls(self, *args):
        return list(args)

    def param_decl(self, name, t: hol_term.Term):
        return (str(name), t)

    def param_decls(self, *args):
        return list(args)
    
    def assum_decls(self, *args):
        return list(args)

    def inher_decl(self, t, struct_name, *args):
        return zfc.StructureProp(t, struct_name, list(args))

    def inher_decls(self, *args):
        return list(args)

    def property_decl(self, name, prop: hol_term.Term):
        return (str(name), prop)
    
    def property_decls(self, *args):
        return list(args)
    
    def fix_decls(self, *args):
        return list(args)
    
    def struct_shows_decl(self, t, struct_name, *args):
        return zfc.StructureProp(t, struct_name, list(args))
    
    def prop_shows_decl(self, t):
        return t

    def param_inst(self, name, t):
        return (str(name), t)
    
    def param_insts(self, *args):
        return list(args)
    
    def structure(self, name, carrier_set, args, params, inherits, props):
        return zfc.Structure(
            name=name,
            carrier_set=carrier_set,
            args=args,
            params=params,
            inherits=inherits,
            props=props)

    def theorem(self, name, fixes, assums, shows, insts):
        return zfc.StructureTheorem(
            name=name,
            fixes=fixes,
            assums=assums,
            shows=shows,
            insts=insts)

    def definition(self, name, fixes, assums, defines):
        return zfc.StructureDefinition(
            name=name,
            args=fixes,
            assums=assums,
            body=defines)
    
    def morphism(self, name, carrier_fun, source, target, inherits, props):
        return zfc.Morphism(
            name=name,
            carrier_fun=carrier_fun,
            source=source,
            target=target,
            inherits=inherits,
            props=props)


def get_parser_for(start):
    return Lark(grammar, start=start, parser="lalr", transformer=ZFCTransformer())

term_parser = get_parser_for("term")
structure_parser = get_parser_for("structure")
theorem_parser = get_parser_for("theorem")
definition_parser = get_parser_for("definition")
morphism_parser = get_parser_for("morphism")
zfc_item_parser = get_parser_for("zfc_item")

def parse_term(s: str) -> hol_term.Term:
    try:
        return term_parser.parse(s)
    except (exceptions.UnexpectedToken, exceptions.UnexpectedCharacters) as e:
        print("When parsing term:", s)
        raise e

def parse_structure(s: str) -> zfc.Structure:
    try:
        return structure_parser.parse(s)
    except (exceptions.UnexpectedToken, exceptions.UnexpectedCharacters) as e:
        print("When parsing structure:", s)
        raise e

def parse_theorem(s: str) -> zfc.StructureTheorem:
    try:
        return theorem_parser.parse(s)
    except (exceptions.UnexpectedToken, exceptions.UnexpectedCharacters) as e:
        print("When parsing theorem:", s)
        raise e

def parse_definition(s: str) -> zfc.StructureDefinition:
    try:
        return definition_parser.parse(s)
    except (exceptions.UnexpectedToken, exceptions.UnexpectedCharacters) as e:
        print("When parsing definition:", s)
        raise e

def parse_morphism(s: str) -> zfc.Morphism:
    try:
        return morphism_parser.parse(s)
    except (exceptions.UnexpectedToken, exceptions.UnexpectedCharacters) as e:
        print("When parsing morphism:", s)
        raise e

def parse_zfc_item(s: str):
    try:
        return zfc_item_parser.parse(s)
    except (exceptions.UnexpectedToken, exceptions.UnexpectedCharacters) as e:
        print("When parsing item:", s)
        raise e

def elaborate_term(t: hol_term.Term, ctx: zfc.ZFCContext):
    """Elaborate a term using context information."""
    if t.head.is_const("Func"):
        S, T, f = t.args
        v, body = f.dest_abs()
        ctx.push()
        ctx.add_soft_type(v, S)
        ctx.propagate()
        body2 = elaborate_term(body, ctx)
        ctx.pop()
        return zfc.Func(S, T, hol_term.Lambda(v, body2))
    elif t.head.is_const("Func2"):
        S1, S2, T, f = t.args
        v1, body1 = f.dest_abs()
        v2, body = body1.dest_abs()
        ctx.push()
        ctx.add_soft_type(v1, S1)
        ctx.add_soft_type(v2, S2)
        ctx.propagate()
        body2 = elaborate_term(body, ctx)
        ctx.pop()
        return zfc.Func2(S1, S2, T, hol_term.Lambda(v1, v2, body2))
    elif t.head.is_const("all"):
        v, body = t.arg.dest_abs()
        assert body.is_implies(), "elaborate_term: %s is not implies" % t
        mem, concl = body.args
        assert mem.head.is_const("member") and mem.arg1 == v, "elaborate_term: %s is not member" % mem
        S = mem.arg  # v Mem S
        ctx.push()
        ctx.add_soft_type(v, S)
        ctx.propagate()
        body2 = elaborate_term(concl, ctx)
        ctx.pop()
        return zfc.ForallS(v.name, S, body2)
    elif t.head.is_const("exists"):
        v, body = t.arg.dest_abs()
        assert body.is_conj(), "elaborate_term: %s is not conj" % t
        mem, concl = body.args
        assert mem.head.is_const("member") and mem.arg1 == v, "elaborate_term: %s is not member" % mem
        S = mem.arg  # v Mem S
        ctx.push()
        ctx.add_soft_type(v, S)
        ctx.propagate()
        body2 = elaborate_term(concl, ctx)
        ctx.pop()
        return zfc.ExistsS(v.name, S, body2)    
    elif t.head.is_const("The"):
        v, body = t.arg.dest_abs()
        assert body.is_conj(), "elaborate_term: %s is not conj" % t
        mem, concl = body.args
        assert mem.head.is_const("member") and mem.arg1 == v, "elaborate_term: %s is not member" % mem
        S = mem.arg  # v Mem S
        ctx.push()
        ctx.add_soft_type(v, S)
        ctx.propagate()
        body2 = elaborate_term(concl, ctx)
        ctx.pop()
        return zfc.TheS(v.name, S, body2)
    elif t.head.is_const("collectSet"):
        S, arg = t.args
        v, body = arg.dest_abs()
        ctx.push()
        ctx.add_soft_type(v, S)
        ctx.propagate()
        body2 = elaborate_term(body, ctx)
        ctx.pop()
        return zfc.CollectS(v.name, S, body2)
    elif t.head.is_const() and t.head.name in ctx.zfc_theory.constants:
        # This is the crucial case: elaboration for a constant with
        # structure argument. We first obtain information for the constant
        # from the ZFC theory.
        const_info: zfc.ZFCConstant = ctx.zfc_theory.constants[t.head.name]
        assert len(t.args) == 1 + const_info.arity

        # Current term for the structure argument
        struct: hol_term.Term = t.args[0]

        # Recursively elaborate the remaining arguments
        args = [elaborate_term(arg, ctx) for arg in t.args[1:]]

        # If no structure term is given, look for structures that satisfy
        # the struct_name in const_info, as well as containing all arguments.
        if struct == zfc.dummyStruct:
            ctx.push()
            for arg in args:
                ctx.add_term(arg)
            ctx.propagate()
            structs = ctx.get_structs(const_info.struct_name, *args)
            new_structs = []
            for derived_struct in structs:
                new_structs.append(zfc.build_struct_term(derived_struct.t, derived_struct))
            ctx.pop()
            if not new_structs:
                print(ctx)
                raise ZFCElaborateException("Unable to elaborate %s" % zfc.zfc_print(t))
            elif len(new_structs) > 1:
                print(ctx)
                print("Options are:", ', '.join(zfc.zfc_print(zfc.recover_carrier_set(struct))
                                                for struct in new_structs))
                raise ZFCElaborateException("Multiple options when elaborating %s" % zfc.zfc_print(t))
            else:
                return t.head(new_structs[0], *args)

        # If term is given for the structure, look for structures based on
        # that term only.
        elif struct.head == zfc.dummyKnownStruct:
            soft_type, = struct.args
            ctx.push()
            for arg in args:
                ctx.add_term(arg)
            ctx.add_term(soft_type)
            ctx.propagate()
            structs = ctx.get_structs_for_term(soft_type, const_info.struct_name)
            new_structs = []
            for derived_struct in structs:
                new_structs.append(zfc.build_struct_term(soft_type, derived_struct))
            for arg in args:
                if soft_type not in ctx.get_soft_types(arg):
                    print("Warning: %s does not have type %s in term %s" %
                            (zfc.zfc_print(arg), zfc.zfc_print(soft_type), zfc.zfc_print(t)))
            ctx.pop()
            if not new_structs:
                raise ZFCElaborateException("Term %s does not have required structure %s" %
                                            (zfc.zfc_print(soft_type), const_info.struct_name))
            else:
                return t.head(new_structs[0], *args)
        else:
            return t.head(struct, *args)
    elif t.is_comb():
        if t.head.is_const("app"):
            a, b = elaborate_term(t.arg1, ctx), elaborate_term(t.arg, ctx)
            ctx.push()
            ctx.add_term(a)
            ctx.add_term(b)
            ctx.propagate()
            found = False
            types_a = ctx.get_soft_types(a)
            types_b = ctx.get_soft_types(b)
            for type_a in types_a:
                for type_b in types_b:
                    if type_a.head.is_const("funcSet") and type_a.arg1 == type_b:
                        found = True
            if not found:
                print("Warning: type check failed for %s" % zfc.zfc_print(t))
            ctx.pop()
            return t.head(a, b)
        else:
            return t.head(*(elaborate_term(arg, ctx) for arg in t.args))
    else:
        return t

def elaborate_structure(struct: zfc.Structure, ctx: zfc.ZFCContext):
    """Elaborate given structure."""
    # For structures, we add to theory first, so it can be used
    # in the ensuing elaborations.
    ctx.zfc_theory.add_structure(struct)

    ctx.push()

    # Add carrier satisfies structure to context information
    carrier = zfc.V(struct.carrier_set)
    ctx.add_struct(carrier, zfc.StructureProp(carrier, struct.name, [zfc.V(arg) for arg, _ in struct.args]))

    # Add arguments to context information
    for arg, arg_prop in struct.args:
        ctx.add_def_argument(arg, arg_prop)

    # Add parameters to context information
    for name, t in struct.params:
        ctx.add_soft_type(zfc.V(name), t)

    # Add inherits to context information
    for inherit in struct.inherits:
        ctx.add_struct(inherit.t, inherit)
    ctx.propagate()

    # Elaborate each proposition
    for i, (name, prop) in enumerate(struct.props):
        prop2 = elaborate_term(prop, ctx)
        struct.props[i] = (name, prop2)
        ctx.add_prop(prop2)
        ctx.propagate()
    ctx.pop()

def elaborate_theorem(theorem: zfc.StructureTheorem, ctx: zfc.ZFCContext):
    """Elaborate given theorem"""
    ctx.push()

    # Add fixes to context information
    for arg, arg_prop in theorem.fixes:
        ctx.add_def_argument(arg, arg_prop)
    ctx.propagate()

    # Elaborate each assumption
    for i, assum in enumerate(theorem.assums):
        assum2 = elaborate_term(assum, ctx)
        theorem.assums[i] = assum2
        ctx.add_prop(assum2)
        ctx.propagate()

    # Elaborate shows
    if isinstance(theorem.shows, hol_term.Term):
        shows2 = elaborate_term(theorem.shows, ctx)
        theorem.shows = shows2
    elif isinstance(theorem.shows, zfc.StructureProp):
        t2 = elaborate_term(theorem.shows.t, ctx)
        theorem.shows = zfc.StructureProp(t2, theorem.shows.struct_name, theorem.shows.struct_args)

    # Elaborate each instantiation
    for i, (name, inst) in enumerate(theorem.insts):
        inst2 = elaborate_term(inst, ctx)
        theorem.insts[i] = (name, inst2)
    ctx.pop()

    # Add theorem to ZFC theory
    ctx.zfc_theory.add_theorem(theorem)

    # Also add appropriate function to the theory
    if isinstance(theorem.shows, zfc.StructureProp):
        t = theorem.shows.t
        if t.head.is_const():
            struct_name = t.head.name + "_" + theorem.shows.struct_name
            if not theory.thy.has_term_sig(struct_name):
                argT, _ = t.head.T.strip_type()
                theory.thy.add_term_sig(struct_name, hol_type.TFun(*(argT + [zfc.structT])))

def elaborate_definition(definition: zfc.StructureDefinition, ctx: zfc.ZFCContext):
    """Elaborate given definition"""
    ctx.push()

    # Add arguments to context information
    for arg, arg_prop in definition.args:
        ctx.add_def_argument(arg, arg_prop)
    ctx.propagate()

    # Elaborate each assumption
    for i, assum in enumerate(definition.assums):
        assum2 = elaborate_term(assum, ctx)
        definition.assums[i] = assum2
        ctx.add_prop(assum2)
        ctx.propagate()

    # Elaborate defined term
    body = definition.body
    body2 = elaborate_term(body, ctx)
    definition.body = body2
    ctx.pop()

    # Add definition to ZFC theory
    ctx.zfc_theory.add_definition(definition)

    # Add name of the definition to the HOL theory.
    # The type of the function is constructed according to the arguments.
    Ts = list()
    for arg, arg_prop in definition.args:
        if isinstance(arg_prop, zfc.StructureProp):
            Ts.append(zfc.structT)
        else:
            Ts.append(zfc.setT)
    Ts.append(definition.body.get_type())
    if not theory.thy.has_term_sig(definition.name):
        theory.thy.add_term_sig(definition.name, TFun(*Ts))

def elaborate_morphism(morphism: zfc.Morphism, ctx: zfc.ZFCContext):
    ctx.push()

    # Add function
    ctx.add_soft_type(zfc.V(morphism.carrier_fun), zfc.funcSet(zfc.V(morphism.source), zfc.V(morphism.target)))

    # Add inherits to context information
    for inherit in morphism.inherits:
        ctx.add_struct(inherit.t, inherit)
    ctx.propagate()

    # Elaborate each property
    for i, (name, prop) in enumerate(morphism.props):
        prop2 = elaborate_term(prop, ctx)
        morphism.props[i] = (name, prop2)
    ctx.pop()

    # Add morphism to ZFC theory
    ctx.zfc_theory.add_morphism(morphism)

    # Also add appropriate predicate to HOL theory
    if not theory.thy.has_term_sig(morphism.name):
        theory.thy.add_term_sig(morphism.name, TFun(zfc.setT, BoolType))

def load_zfc_item(s: str):
    item = parse_zfc_item(s)
    ctx = zfc.ZFCContext(zfc.zfc_theory)

    try:
        if isinstance(item, zfc.Structure):
            elaborate_structure(item, ctx)
        elif isinstance(item, zfc.StructureTheorem):
            elaborate_theorem(item, ctx)
        elif isinstance(item, zfc.StructureDefinition):
            elaborate_definition(item, ctx)
        elif isinstance(item, zfc.Morphism):
            elaborate_morphism(item, ctx)
        else:
            raise AssertionError("Unknown item type %s" + type(item))
    except ZFCElaborateException as e:
        if isinstance(item, zfc.Structure):
            ctx.zfc_theory.remove_structure(item)
        print("Elaboration failed:", e.str)
        return None
    except zfc.ZFCAlreadyExistsException as e:
        print(e)
        return None

    print('load successful')
    return item
