from typing import List, Tuple, Dict, Union

from kernel import type as hol_type
from kernel import term as hol_term
from kernel.type import TConst
from kernel.term import Term, Var, Const, Lambda

from logic import logic
from logic import basic
basic.load_theory("zfc")

from syntax.settings import settings


class ZFCAlreadyExistsException(Exception):
    """Item already exists."""
    def __init__(self, str):
        self.str = str


"""
Preliminary definitions

We define the type of sets, corresponding to sets in Zermelo-Fraenkel set theory.
We also axiomatize several constructors on sets.

"""
# Type of sets
setT = TConst("i")

# Variable of type set
def V(v: str):
    return Var(v, setT)

# Membership relation
memSet = Const("member", hol_type.TFun(setT, setT, hol_type.BoolType))

# Subset relation
subSet = Const("subset", hol_type.TFun(setT, setT, hol_type.BoolType))

# Set of functions between two sets
funcSet = Const("funcSet", hol_type.TFun(setT, setT, setT))

# Set of functions from A, B to C
funcSet2 = Const("funcSet2", hol_type.TFun(setT, setT, setT, setT))

# Collection of elements satisfying some property
collectSet = Const("collectSet", hol_type.TFun(setT, hol_type.TFun(setT, hol_type.BoolType), setT))

# Identity function
identityFun = Const("identity", hol_type.TFun(setT, setT))

# Compose function
composeFun = Const("compose", hol_type.TFun(setT, setT, setT))

# Derive a set-theoretic function from a lambda term
Func = Const("Func", hol_type.TFun(setT, setT, hol_type.TFun(setT, setT), setT))

# Derive a set-theoretic binary function from a lambda term
Func2 = Const("Func2", hol_type.TFun(setT, setT, setT, hol_type.TFun(setT, setT, setT), setT))

# Apply a set-theoretic function
app = Const("app", hol_type.TFun(setT, setT, setT))

# Apply a set-theoretic binary function
app2 = Const("app2", hol_type.TFun(setT, setT, setT, setT))

# Construct the set version of forall
def ForallS(v: str, s: Term, p: Term):
    return hol_term.Forall(V(v), hol_term.Implies(memSet(V(v), s), p))

# Construct the set version of exists
def ExistsS(v: str, s: Term, p: Term):
    return hol_term.Exists(V(v), hol_term.And(memSet(V(v), s), p))

# Construct the set version of THE
def TheS(v: str, s: Term, p: Term):
    return logic.mk_the(V(v), hol_term.And(memSet(V(v), s), p))

# Construct collection of elements satisfying some property
def CollectS(v: str, s: Term, p: Term):
    return collectSet(s, Lambda(V(v), p))

"""For structures, we define a special type ``struct``"""

# Type of structures
structT = TConst("struct")

# Dummy structure term
dummyStruct = Var("dummy", structT)

# Dummy with known term
dummyKnownStruct = Var("dummyK", hol_type.TFun(setT, structT))

# Arithmetic operations
unit = Const("unit", hol_type.TFun(structT, setT))
plusOp = Const("plusOp", hol_type.TFun(structT, setT, setT, setT))
minusOp = Const("minusOp", hol_type.TFun(structT, setT, setT, setT))
timesOp = Const("timesOp", hol_type.TFun(structT, setT, setT, setT))

# Variable of type struct
def structV(v: str):
    return Var(v, structT)

"""
Printing functions
"""

def priority(t: Term) -> int:
    if t.is_var():
        return 100
    elif t.head.is_const("funcSet") or t.head.is_const("funcSet2"):
        return 55
    elif t.head.is_const("app"):
        return 100
    elif t.head.is_const("subset") or t.head.is_const("member"):
        return 50
    elif t.head.is_const("all") or t.head.is_const("exists") or t.head.is_const("The"):
        return 10
    elif t.head.is_const("collectSet") or t.head.is_const("Func") or t.head.is_const("Func2"):
        return 10
    elif t.head.is_const("timesOp"):
        return 70
    elif t.head.is_const("unit"):
        return 100
    elif t.is_equals():
        return 50
    elif t.is_conj():
        return 35
    elif t.is_implies():
        return 20
    elif t.head.is_const("compose"):
        return 60
    else:
        return 100

def zfc_print(t: Union[Term, "StructureProp", "MembershipProp"]) -> str:
    if isinstance(t, StructureProp):
        return t.zfc_print()
    if isinstance(t, MembershipProp):
        return t.zfc_print()

    if t.is_var():
        return t.name
    elif t.head.is_const("Func"):
        S, T, f = t.args
        v, body = f.dest_abs()
        return "%s: %s -> %s : %s" % (v.name, zfc_print(S), zfc_print(body), zfc_print(T))
    elif t.head.is_const("Func2"):
        S1, S2, T, f = t.args
        v1, body1 = f.dest_abs()
        v2, body = body1.dest_abs()
        return "%s: %s, %s: %s -> %s : %s" % (v1.name, zfc_print(S1), v2.name, zfc_print(S2), zfc_print(body), zfc_print(T))
    elif t.head.is_const("funcSet"):
        a1, a2 = t.args
        fun_op = " ⇒ " if settings.unicode else " => "
        return "%s%s%s" % (zfc_print(a1), fun_op, zfc_print(a2))
    elif t.head.is_const("funcSet2"):
        a1, a2, b = t.args
        fun_op = " ⇒ " if settings.unicode else " => "
        return "(%s, %s)%s%s" % (zfc_print(a1), zfc_print(a2), fun_op, zfc_print(b))
    elif t.head.is_const("app"):
        f, a = t.args
        return "%s(%s)" % (zfc_print(f), zfc_print(a))
    elif t.head.is_const("subset"):
        a1, a2 = t.args
        fun_op = " ⊆ " if settings.unicode else " Sub "
        return "%s%s%s" % (zfc_print(a1), fun_op, zfc_print(a2))
    elif t.head.is_const("member"):
        a1, a2 = t.args
        fun_op = " ∈ " if settings.unicode else " Mem "
        return "%s%s%s" % (zfc_print(a1), fun_op, zfc_print(a2))
    elif t.head.is_const("all"):
        v, body = t.arg.dest_abs()
        mem, concl = body.args
        S = mem.arg
        all_op = "∀" if settings.unicode else "!"
        return "%s%s:%s. %s" % (all_op, v.name, zfc_print(S), zfc_print(concl))
    elif t.head.is_const("exists"):
        v, body = t.arg.dest_abs()
        mem, concl = body.args
        S = mem.arg
        exists_op = "∃" if settings.unicode else "?"
        return "%s%s:%s. %s" % (exists_op, v.name, zfc_print(S), zfc_print(concl))
    elif t.head.is_const("The"):
        v, body = t.arg.dest_abs()
        mem, concl = body.args
        S = mem.arg
        return "THE %s:%s. %s" % (v.name, zfc_print(S), zfc_print(concl))
    elif t.head.is_const("collectSet"):
        S, arg = t.args
        v, body = arg.dest_abs()
        return "{%s:%s. %s}" % (v.name, zfc_print(S), zfc_print(body))
    elif t.head.is_const("timesOp"):
        struct, a, b = t.args
        sa, sb = zfc_print(a), zfc_print(b)
        if priority(a) < priority(t):
            sa = "(" + sa + ")"
        if priority(b) <= priority(t):
            sb = "(" + sb + ")"
        if settings.show_struct and struct != dummyStruct:
            return "%s *[%s] %s" % (sa, zfc_print(recover_carrier_set(struct)), sb)
        else:
            return "%s * %s" % (sa, sb)
    elif t.head.is_const("unit"):
        struct, = t.args
        if settings.show_struct and struct != dummyStruct:
            return "1[%s]" % zfc_print(recover_carrier_set(struct))
        else:
            return "1"
    elif t.is_equals():
        return "%s = %s" % (zfc_print(t.lhs), zfc_print(t.rhs))
    elif t.is_conj():
        a1, a2 = t.args
        fun_op = " ∧ " if settings.unicode else " & "
        return "%s%s%s" % (zfc_print(a1), fun_op, zfc_print(a2))
    elif t.is_implies():
        a1, a2 = t.args
        fun_op = " ⟶ " if settings.unicode else " --> "
        return "%s%s%s" % (zfc_print(a1), fun_op, zfc_print(a2))
    elif t.head.is_const("compose"):
        a1, a2 = t.args
        fun_op = " ∘ " if settings.unicode else " O "
        return "%s%s%s" % (zfc_print(a1), fun_op, zfc_print(a2))
    elif t.head.is_const() and t.head.name in zfc_theory.constants:
        struct = t.args[0]
        args = t.args[1:]
        if settings.show_struct and struct != dummyStruct:
            return "%s [%s] %s" % (t.head.name, zfc_print(recover_carrier_set(struct)), " ".join(zfc_print(arg) for arg in args))
        else:
            return "%s %s" % (t.head.name, " ".join(zfc_print(arg) for arg in args))
    elif t.head.is_const():
        return "%s %s" % (t.head.name, " ".join(zfc_print(arg) for arg in t.args))
    else:
        print(t)
        raise NotImplementedError


"""
Structure definitions

Each structure definition contains the following parts:

* The set that the structure builds on (carrier set).

* Any additional arguments of the structure (e.g. $M$ in submonoid).

* Parameters of the structure (in addition to the base structures).

* Properties satisfied by the parameters (in addition to the properties in the base structures).

"""
class Structure:
    def __init__(self, name: str, carrier_set: str, args: List[Tuple[str, "StructureProp"]],
                 params: List[Tuple[str, Term]], inherits: List["StructureProp"],
                 props: List[Tuple[str, Term]]):
        self.name = name
        self.carrier_set = carrier_set
        self.args = args
        self.params = params
        self.inherits = inherits
        self.props = props
        
    def __str__(self):
        return "structure " + self.name
    
    def zfc_print(self) -> str:
        # Name and carrier set
        res = "struct %s (set %s) {\n" % (self.name, self.carrier_set)
        # Arguments
        if self.args:
            res += "   arguments\n"
            for _, struct_prop in self.args:
                res += "      %s;\n" % struct_prop
        # Parameters
        if self.params:
            res += "   parameters\n"
            for name, param in self.params:
                if param.is_var():
                    res += "      const %s : %s;\n" % (name, zfc_print(param))
                else:
                    res += "      func %s : %s;\n" % (name, zfc_print(param))
        # Inherits
        if self.inherits:
            res += "   inherits\n"
            for struct_prop in self.inherits:
                res += "      %s;\n" % struct_prop.zfc_print()
        # Properties
        if self.props:
            res += "   properties\n"
            for name, prop in self.props:
                res += "      %s: %s;\n" % (name, zfc_print(prop))
        res += "}"
        return res
    
    def show(self):
        print(self.zfc_print())

    def get_property(self, name: str) -> hol_term.Term:
        for n, prop in self.props:
            if n == name:
                return prop
        raise AssertionError("Property %s not found" % name)


structures: Dict[str, Structure] = dict()

"""
Declaring theorems

Each theorem contains the following parts:

* List of *fixes* that declares variables with structure assumptions

* List of *assumes* that states additional assumptions

* A conclusion, which may state that some variable satisfies some structure
  assumption, in which case definitions of parameters should be given.

"""
class StructureProp:
    """Pseudo-proposition stating the fact that some term satisfies some structure assumptions."""
    def __init__(self, t: Term, struct_name: str, struct_args: List[Term]):
        assert isinstance(t, Term) and isinstance(struct_name, str), "StructureProp"
        assert all(isinstance(arg, Term) for arg in struct_args), "StructureProp"
        self.t = t
        self.struct_name = struct_name
        self.struct_args = struct_args
        
    def __str__(self):
        if self.struct_args:
            return "%s :: %s(%s)" % (self.t, self.struct_name, ", ".join(str(arg) for arg in self.struct_args))
        else:
            return "%s :: %s" % (self.t, self.struct_name)

    def zfc_print(self):
        if self.struct_args:
            return "%s :: %s(%s)" % (zfc_print(self.t), self.struct_name, ", ".join(zfc_print(arg) for arg in self.struct_args))
        else:
            return "%s :: %s" % (zfc_print(self.t), self.struct_name)

    def __eq__(self, other):
        return isinstance(other, StructureProp) and self.t == other.t and \
            self.struct_name == other.struct_name and self.struct_args == other.struct_args

    def __hash__(self):
        return hash(("StructureProp", self.t, self.struct_name, tuple(self.struct_args)))


class MembershipProp:
    """Pseudo-proposition stating that some term is a member of some set."""
    def __init__(self, t: Term, s: Term):
        self.t = t
        self.s = s

    def __str__(self):
        return "%s: %s" % (self.t, self.s)

    def zfc_print(self):
        return "%s: %s" % (zfc_print(self.t), zfc_print(self.s))


class StructureTheorem:
    def __init__(self, name, fixes: List[Tuple[str, StructureProp]], assums: List[Term],
                 shows: Union[StructureProp, Term], insts: List[Tuple[str, Term]]):
        self.name = name
        self.fixes = fixes
        self.assums = assums
        self.shows = shows
        self.insts = insts
        
    def __str__(self):
        return "theorem " + self.name
    
    def zfc_print(self):
        res = "theorem %s:\n" % self.name
        if self.fixes:
            res += "   fixes\n"
            for _, struct_prop in self.fixes:
                res += "      " + str(struct_prop) + "\n"
        for assum in self.assums:
            res += "   assumes " + zfc_print(assum) + "\n"
        if self.insts:
            res += "   shows " + zfc_print(self.shows) + " with\n"
        else:
            res += "   shows " + zfc_print(self.shows) + "\n"
        for param_name, param in self.insts:
            res += "      %s = %s\n" % (param_name, zfc_print(param))
        return res

    def show(self):
        print(self.zfc_print())


"""
Definitions

Each definition contains the following parts:

* Any additional arguments on the definition (e.g. $M$ in inverse).

* Any additional assumptions on the definition (e.g. invertible condition in inverse).

* Body of the definition.

"""
class StructureDefinition:
    def __init__(self, name: str, args: List[Tuple[str, StructureProp | MembershipProp]],
                 assums: List[Term], body: Term):
        self.name = name
        self.args = args
        self.assums = assums
        self.body = body
        
    def __str__(self):
        return "definition " + self.name
    
    def zfc_print(self):
        res = "definition %s:\n" % self.name
        if self.args:
            res += "   fixes\n"
            for _, prop in self.args:
                res += "      " + zfc_print(prop) + "\n"
        if self.assums:
            res += "   assumes\n"
            for assum in self.assums:
                res += "      " + zfc_print(assum) + "\n"
        res += "   defines " + zfc_print(self.body)
        return res

    def show(self):
        print(self.zfc_print())


"""
Morphisms

We now turn to defining different classes of functions. A morphism is
given by a name, two structures, and a list of properties.

"""
class Morphism:
    def __init__(self, name: str, carrier_fun: str, source: str, target: str,
                 inherits: List[StructureProp], props: List[Tuple[str, Term]]):
        self.name = name
        self.carrier_fun = carrier_fun
        self.source = source
        self.target = target
        self.inherits = inherits
        self.props = props
        
    def __str__(self):
        return "morphism " + self.name
    
    def zfc_print(self):
        res = "morphism %s (%s : %s -> %s) {\n" % (self.name, self.carrier_fun, self.source, self.target)
        if self.inherits:
            res += "   inherits\n"
            for struct_prop in self.inherits:
                res += "      %s\n" % zfc_print(struct_prop)
        if self.props:
            res += "   properties\n"
            for name, prop in self.props:
                res += "      %s: %s\n" % (name, zfc_print(prop))
        res += "}"
        return res
    
    def show(self):
        print(self.zfc_print())


"""
Theory

Theory information includes the following:

* List of structures.

* List of morphisms.

* List of definitions.

* List of theorems.

* List of constants. This includes both parameters of structures, as well as
  definitions. For each constant, record the lowest-level structure it is
  defined for, as well as number of additional arguments.

"""
class ZFCConstant:
    def __init__(self, name: str, struct_name: str, arity: int):
        self.name = name
        self.struct_name = struct_name
        self.arity = arity

    def __str__(self):
        return "constant %s: base structure = %s, arity = %s" % (self.name, self.struct_name, self.arity)


class ZFCTheory:
    def __init__(self):
        # List of structures
        self.structs: Dict[str, Structure] = dict()

        # List of morphisms
        self.morphisms: Dict[str, Morphism] = dict()

        # List of definitions
        self.definitions: Dict[str, StructureDefinition] = dict()

        # List of theorems
        self.theorems: Dict[str, StructureTheorem] = dict()

        # List of constants.
        self.constants: Dict[str, ZFCConstant] = dict()

        # Add set structure
        self.structs["Set"] = Structure("Set", "M", args=[], params=[], inherits=[], props=[])

    def __str__(self):
        res = ""
        for _, struct in self.structs.items():
            res += str(struct) + "\n"
        for _, morphism in self.morphisms.items():
            res += str(morphism) + "\n"
        for _, definition in self.definitions.items():
            res += str(definition) + "\n"
        for _, theorem in self.theorems.items():
            res += str(theorem) + "\n"
        for _, constant in self.constants.items():
            res += str(constant) + "\n"
        return res

    def add_structure(self, struct: Structure):
        """Add a given structure to the theory."""
        if struct.name in self.structs:
            raise ZFCAlreadyExistsException("add_structure: structure %s already exists" % struct.name)
        self.structs[struct.name] = struct

        # Also add the associated constant.
        for param_name, param_type in struct.params:
            assert param_name not in self.constants
            carrier = V(struct.carrier_set)
            if param_type == carrier:
                self.constants[param_name] = ZFCConstant(param_name, struct.name, 0)
            elif param_type == funcSet2(carrier, carrier, carrier):
                self.constants[param_name] = ZFCConstant(param_name, struct.name, 2)
            else:
                raise NotImplementedError
            
    def remove_structure(self, struct: Structure):
        if struct.name in self.structs:
            del self.structs[struct.name]
        for param_name, param_type in struct.params:
            if param_name in self.constants:
                del self.constants[param_name]

    def add_morphism(self, morphism: Morphism):
        """Add a given morphism to the theory."""
        if morphism.name in self.morphisms:
            raise ZFCAlreadyExistsException("add_morphism: morphism %s already exists" % morphism.name)
        self.morphisms[morphism.name] = morphism

    def add_theorem(self, theorem: StructureTheorem):
        """Add a given theorem to the theory."""
        if theorem.name in self.theorems:
            raise ZFCAlreadyExistsException("add_theorem: theorem %s already exists" % theorem.name)
        self.theorems[theorem.name] = theorem

    def add_definition(self, definition: StructureDefinition):
        """Add a given definition to the theory."""
        if definition.name in self.definitions:
            raise ZFCAlreadyExistsException("add_definition: definition %s already exists" % definition.name)
        self.definitions[definition.name] = definition

        # Also add the associated constant.
        # We only consider the case where the first argument is a structure
        # and the remaining arguments are members of the structure.
        if not (len(definition.args) >= 1 and isinstance(definition.args[0][1], StructureProp)):
            raise NotImplementedError
        struct_name = definition.args[0][1].struct_name
        struct = definition.args[0][1].t
        for i in range(1, len(definition.args)):
            if not (isinstance(definition.args[i][1], MembershipProp) and definition.args[i][1].s == struct):
                raise NotImplementedError
        assert definition.name not in self.constants
        self.constants[definition.name] = ZFCConstant(definition.name, struct_name, len(definition.args) - 1)

    def get_all_structure_params(self, struct: Union[str, StructureProp]) -> List[str]:
        """Lookup all parameters of a structure.
        
        This includes recursively lookup parameters in inherited structures.
        
        """
        if isinstance(struct, StructureProp):
            return self.get_all_structure_params(struct.struct_name)
        if struct not in self.structs:
            raise AssertionError("get_all_structure_params: structure %s not found" % struct)
        res = []
        for name, _ in self.structs[struct].params:
            res.append(name)
        for inherits in self.structs[struct].inherits:
            res.extend(self.get_all_structure_params(inherits))
        return res
    
    def get_all_structure_props(self, struct: StructureProp) -> List[Term]:
        """From a proposition stating satisfaction of some structure,
        derive the consequences.        
        
        """
        if struct.struct_name not in self.structs:
            raise AssertionError("get_all_structure_props: structure %s not found" % struct)
        res = []
        for _, prop in self.structs[struct.struct_name].props:
            res.append(prop)
        for inherits in self.structs[struct.struct_name].inherits:
            res.extend(self.get_all_structure_props(inherits))
        return res


def build_struct_term(t: Term, struct: Union[str, StructureProp]) -> Term:
    """Given a term t and a structure information, return the corresponding
    structure term struct_t, whose carrier set is t.

    The naming rule is
    
    * For variables: <variable name>_<struct name>

    * For function application: <function name>_<struct name> (arguments)
    
    """
    if isinstance(struct, StructureProp):
        return build_struct_term(t, struct.struct_name)

    if t.is_var():
        assert t.T == setT, "build_struct_term: input type is not set."
        return Var(t.name + "_" + struct, structT)
    elif t.is_const():
        argT, resT = t.T.strip_type()
        assert resT == setT, "build_struct_term: input type is not set."
        return Const(t.name + "_" + struct, hol_type.TFun(*(argT + [structT])))
    elif t.is_comb():
        head, args = t.head, t.args
        return build_struct_term(head, struct)(*args)
    else:
        raise AssertionError("build_struct_term: unexpected term %s." % t)
    
def recover_carrier_set(t: Term) -> Term:
    """Given a structure term t, return the corresponding carrier set.
    This undoes the effect of build_struct_term.
    
    """
    if t.is_var():
        loc = t.name.rfind("_")
        assert t.T == structT, "recover_carrier_set for %s: input type is not struct." % t
        assert loc > 0, "recover_carrier_set: unexpected variable name %s" % t.name
        return Var(t.name[:loc], setT)
    elif t.head.is_var() and t.head.name == "dummyK":
        return t.arg
    elif t.is_const():
        loc = t.name.rfind("_")
        argT, outT = t.T.strip_type()
        assert outT == structT, "recover_carrier_set for %s: input type does not output struct." % t
        assert loc > 0, "recover_carrier_set: unexpected variable name %s" % t.name
        return Const(t.name[:loc], hol_type.TFun(*(argT + [setT])))
    elif t.is_comb():
        head, args = t.head, t.args
        return recover_carrier_set(head)(*args)
    else:
        raise AssertionError("recover_carrier_set: unexpected term.")

"""
Context

Context information includes the following:

* Current theory information.

* Membership of terms within sets (soft types information). 

* Known structures on sets.

* Subset relationships (used for inference).

"""
class ZFCContext:
    def __init__(self, zfc_theory: ZFCTheory):
        # Current theory
        self.zfc_theory = zfc_theory

        # Frame number
        self.frame = 0

        # Set of terms
        self.terms: Dict[Term, int] = dict()

        # Maps each term to the list of sets it is contained in
        self.soft_types: Dict[Term, List[Tuple[int, Term]]] = dict()

        # Maps each set to the list of structures it satisfy
        self.structs: Dict[Term, List[Tuple[int, StructureProp]]] = dict()

        # Subset relations
        self.subsets: Dict[Term, List[Tuple[int, Term]]] = dict()

    def __str__(self):
        res = "Terms:\n"
        res += "  " + ", ".join(str(t) + " (" + str(n) + ")" for t, n in self.terms.items()) + "\n"
        res += "Soft types:\n"
        for t in self.soft_types:
            if self.soft_types[t]:
                res += "  %s in " % t
                for n, s in self.soft_types[t]:
                    res += " %s (%s)" % (s, n)
                res += "\n"
        res += "Structures:\n"
        for t in self.structs:
            if self.structs[t]:
                res += "  %s is " % t
                for n, s in self.structs[t]:
                    res += " %s (%s)" % (s, n)
                res += "\n"
        res += "Subset relations:\n"
        for t in self.subsets:
            if self.subsets[t]:
                res += "  %s subsetof " % t
                for n, s in self.subsets[t]:
                    res += " %s (%s)" % (s, n)
                res += "\n"
        return res
    
    def push(self):
        """Increment frame number."""
        self.frame += 1

    def pop(self):
        """Decrement frame number, remove information from that frame."""
        self.frame -= 1
        self.terms = dict((t, n) for t, n in self.terms.items() if n <= self.frame)
        for t in self.soft_types:
            self.soft_types[t] = [(n, s) for n, s in self.soft_types[t] if n <= self.frame]
        for t in self.structs:
            self.structs[t] = [(n, s) for n, s in self.structs[t] if n <= self.frame]
        for t in self.subsets:
            self.subsets[t] = [(n, s) for n, s in self.subsets[t] if n <= self.frame]

    def add_term(self, t: Term):
        """Add term to context. This term will participate in type inference."""
        self.terms[t] = self.frame

    def add_soft_type(self, t: Term, s: Term):
        """Add soft type information at current frame."""
        if t not in self.soft_types:
            self.soft_types[t] = list()
        self.soft_types[t].append((self.frame, s))

    def add_struct(self, t: Term, struct: StructureProp):
        """Add struct information at current frame."""
        if t not in self.structs:
            self.structs[t] = list()
        self.structs[t].append((self.frame, struct))

    def add_subset(self, t: Term, s: Term):
        """Add subset information at current frame."""
        if t not in self.subsets:
            self.subsets[t] = list()
        self.subsets[t].append((self.frame, s))

    def get_soft_types(self, t: Term) -> List[Term]:
        """Return list of soft types of a term."""
        res = []
        if t in self.soft_types:
            for _, s in self.soft_types[t]:
                res.append(s)
        return res
    
    def get_joint_soft_types(self, t1: Term, t2: Term) -> List[Term]:
        """Return list of soft types satisfied by both t1 and t2."""
        res1 = self.get_soft_types(t1)
        res2 = self.get_soft_types(t2)
        return [t for t in res1 if t in res2]

    def get_structs(self, struct_name: str, *ts) -> List[StructureProp]:
        """Return all structure information with the given name, and whose
        carrier set contains all input terms.
        
        """
        res = []
        for s in self.structs:
            if all(s in self.get_soft_types(t) for t in ts):
                for _, struct_prop in self.structs[s]:
                    if struct_prop.struct_name == struct_name:
                        res.append(struct_prop)
        return res
    
    def get_structs_for_term(self, s: Term, struct_name: str) -> List[StructureProp]:
        """Return all structure information for the given term and with
        the given name, and whose carrier set contains all input terms.
        
        """
        assert isinstance(s, Term), "get_structs_for_term"
        res = []
        if s in self.structs:
            for _, struct_prop in self.structs[s]:
                if struct_prop.struct_name == struct_name:
                    res.append(struct_prop)
        return res

    def get_subsets(self, t: Term) -> List[Term]:
        """Return list of subsets for a term."""
        res = []
        if t in self.subsets:
            for _, s in self.subsets[t]:
                res.append(s)
        return res

    def add_prop(self, prop: Term):
        """Add the given proposition to the context."""
        # Subset
        if prop.head.is_const("subset"):
            t, s = prop.args
            self.add_subset(t, s)

    def add_def_argument(self, arg: str, arg_prop: Union[StructureProp, MembershipProp]):
        """Add argument of structure / definition / theorem to the context.
        
        The argument is one of two types:
        
        * StructureProp: indicating that a set/function satisfies some structure/morphism,
        
        * MembershipProp: indicating that a term belongs to some set.
        
        """
        self.add_term(V(arg))
        if isinstance(arg_prop, StructureProp):
            self.add_struct(V(arg), arg_prop)
            # Add propositions
            props = self.zfc_theory.get_all_structure_props(arg_prop)
            for prop in props:
                self.add_prop(prop)
        elif isinstance(arg_prop, MembershipProp):
            self.add_soft_type(V(arg), arg_prop.s)

    def propagate(self):
        updated = True
        while updated:
            updated = False
            for t in self.terms:
                # Propagation for unit
                if t.head.is_const("unit"):
                    struct, = t.args
                    soft_type = recover_carrier_set(struct)
                    if soft_type not in self.get_soft_types(t):
                        self.add_soft_type(t, soft_type)
                        updated = True
                # Propagation for timesOp
                if t.head.is_const("timesOp"):
                    _, a, b = t.args
                    soft_types = self.get_joint_soft_types(a, b)
                    for soft_type in soft_types:
                        if soft_type not in self.get_soft_types(t):
                            self.add_soft_type(t, soft_type)
                            updated = True
                # Propagation for function application
                if t.head.is_const("app"):
                    f, a = t.args
                    soft_types_f = self.get_soft_types(f)
                    soft_types_a = self.get_soft_types(a)
                    for soft_type_f in soft_types_f:
                        for soft_type_a in soft_types_a:
                            if soft_type_f.head.is_const("funcSet") and soft_type_f.arg1 == soft_type_a:
                                if soft_type_f.arg not in self.get_soft_types(t):
                                    self.add_soft_type(t, soft_type_f.arg)
                                    updated = True
                # Propagation for subsets
                soft_types = self.get_soft_types(t)
                for soft_type in soft_types:
                    super_types = self.get_subsets(soft_type)
                    for super_type in super_types:
                        if super_type not in self.get_soft_types(t):
                            self.add_soft_type(t, super_type)
                            updated = True
                # Propagate each rule
                for _, theorem in self.zfc_theory.theorems.items():
                    # Theorem can be used as a structure derivation rule, if it satisfies
                    # three conditions:
                    # 1. it has no assumptions
                    # 2. all fixes in the theorem are StructureProp
                    # 3. the conclusion of the theorem is StructureProp
                    if len(theorem.assums) == 0 and \
                            all(isinstance(arg_prop, StructureProp) for _, arg_prop in theorem.fixes) and \
                            isinstance(theorem.shows, StructureProp):
                        # List of instantiations of fixed variables
                        arg_insts = list()
                        arg_insts.append(hol_term.Inst())
                        # Loop through the list of fixes, updating arg_insts
                        # at each iteration
                        for arg_name, arg_prop in theorem.fixes:
                            new_arg_insts = list()
                            for inst in arg_insts:
                                new_args = [arg.subst(inst) for arg in arg_prop.struct_args]
                                for struct_prop in self.get_structs(arg_prop.struct_name):
                                    if struct_prop.struct_args == new_args:
                                        new_inst = inst.copy()
                                        new_inst.var_inst[arg_name] = struct_prop.t
                                        new_arg_insts.append(new_inst)
                            arg_insts = new_arg_insts
                        # Add the conclusion if the corresponding term exists and the
                        # conclusion is not already found
                        for inst in arg_insts:
                            t = theorem.shows.t.subst(inst)
                            if t in self.terms:
                                new_args = [arg.subst(inst) for arg in theorem.shows.struct_args]
                                new_struct = StructureProp(t, theorem.shows.struct_name, new_args)
                                if new_struct not in self.get_structs_for_term(t, theorem.shows.struct_name):
                                    self.add_struct(t, new_struct)
                                    updated = True

"""Global theory"""
zfc_theory: ZFCTheory
