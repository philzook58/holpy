from typing import Tuple, Union

class TyInst:
    """Indicates error in processing types."""

class Type:
    """Represents a type in higher-order logic.
    
    Types in HOL are formed by two kinds of constructors: STVar, TVar and TConst.

    STVar(name) represents a schematic type variable with the given name.
    TVar(name) represents a type variable with the given name. TConst(f, args)
    represents a type constant applied to a list of arguments.
    
    There are two fundamental type constants:
    
    - booleans, with name "bool" and no arguments.
    
    - functions, with name "fun" and two arguments: the domain and codomain
    types. TConst("fun", a, b) is printed as a => b. The => sign associates to
    the right.
    
    Further defined type constants include:
    
    - natural numbers, with name "nat" and no arguments.
    
    - lists, with name "list" and one argument.

    - product, with name "prod" and two arguments. TConst("prod", a, b) is
    printed as a * b.
    
    Examples:
    
    nat => bool: functions from natural numbers to booleans (predicates on
    natural numbers).

    nat => nat: functions from natural numbers to natural numbers.

    nat => nat => nat: or nat => (nat => nat), functions from two natural
    numbers to natural numbers.

    nat * nat => nat: functions from a pair of natural numbers to natural
    numbers.

    nat list: list of natural numbers.

    nat list list: list of lists of natural numbers.

    """
    # ty values for distinguishing between Type objects.

    def is_stvar(self) -> bool:
        """Return whether self is a schematic type variable."""

    def is_tvar(self) -> bool:
        """Return whether self is a type variable."""

    def is_tconst(self) -> bool:
        """Return whether self is given by a type constructor."""

    def is_fun(self) -> bool:
        """Whether self is of the form a => b."""
    
    def domain_type(self) -> Type:
        """Given a type of form a => b, return a."""
    
    def range_type(self) -> Type:
        """Given a type of form a => b, return b."""

    def strip_type(self) -> Tuple[list(Type), Type]:
        """Given a type of form a_1 => ... => a_n, b, return the pair
        [a_1, ... a_n], b.
        
        """

    def get_stvars(self) -> list(Type):
        """Return the list of schematic type variables."""

    def get_tvars(self) -> list(Type):
        """Return the list of type variables."""

    def get_tsubs(self) -> list(Type):
        """Return the list of schematic type variables and type variables
        appearing in self."""

    def size(self) -> int:
        """Return the size of the type."""

    def subst(self, tyinst: Union[TyInst, None] = None, **kwargs) -> Type:
        """Simultaneously substitute for the type variables using tyinst.
        
        Parameters
        ==========
        tyinst : TyInst
            Type instantiation to be substituted.

        """

    def match(self, T: Type) -> TyInst:
        """Type matching of self with T.
        
        Return the resulting instantiation, or raise TypeMatchException
        if matching fails.

        """

    def get_tvars(self) -> list(Type):
        """Return the list of type variables."""

    def is_numeral_type(self) -> bool: ...

    # # temporty methods
    # # Pyo3 does not support __eq__ in 0.19.3, 0.20.0 will support it.
    # def __eq__(self, other) -> bool:
    #     print("hhh")
    #     return self.cmp(other)

def STVar(name: str) -> Type:
    """Schematic type variable."""

def TVar(name: str) -> Type:
    """Type variable."""

def TConst(name: str, *args) -> Type:
    """Type constant, applied to a list of arguments."""

def TFun(*args) -> TConst:
    """Returns the function type arg1 => arg2 => ... => argn."""

class Term:
    """Represents a term in higher-order logic.
    
    There are six term constructors:
    
    SVar(name, T): schematic variable with given name and type.

    Var(name, T): variable with given name and type.

    Const(name, T): constant with given name and type.

    Comb(f, a): the function f applied to a, written as f a (or f(a)).

    Abs(x, T, body): abstraction. x is the suggested name of the bound
    variable, and T is the type of the bound variable. body is the body of
    the abstraction. This is written as %x::T. body, where the type T is
    usually omitted.

    Bound(n): bound variable with de Bruijn index n."""

    def is_svar(self) -> bool:
        """Return whether self is a schematic variable."""

    def is_var(self) -> bool:
        """Return whether self is a variable."""

    def is_const(self, name=None) -> bool:
        """Return whether the term is a constant.

        name : optional str
            If given, test whether the term has that name.

        """

    def is_comb(self) -> bool:
        """Return whether self is a combination."""

    def __call__(self, *args) -> Term:
        """Apply self (as a function) to a list of arguments."""

    def size(self) -> int:
        """Return the size of the term."""
    
    def get_type(self) -> Type:
        """Returns type of the term with minimal type checking."""

    def is_open(self) -> bool:
        """Whether t is an open term."""

def SVar(name: str, T: Type) -> Term:
    """Schematic variable."""

def Var(name: str, T: Type) -> Term:
    """Variable."""

def Const(name: str, T: Type) -> Term:
    """Constant."""

def Comb(f: Term, a: Term) -> Term:
    """Combination."""

def Abs(x: str, T: Type, body: Term) -> Term:
    """Abstraction."""

def Bound(n: int) -> Term:
    """Bound variable with de Bruijn index n."""