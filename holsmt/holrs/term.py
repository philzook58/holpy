from holrs._reexport import *
from holrs.other_python_file import *
from holrs import TermException

from typing import TYPE_CHECKING, Union

true = Const("true", BoolType)
false = Const("false", BoolType)

neg = Const("neg", TFun(BoolType, BoolType))
conj = Const("conj", TFun(BoolType, BoolType, BoolType))
disj = Const("disj", TFun(BoolType, BoolType, BoolType))
implies = Const("implies", TFun(BoolType, BoolType, BoolType))

def get_svars(t: Union[Term, list[Term]]) -> list[Term]:
    """Returns list of schematic variables in a term or a list of terms."""
    if isinstance(t, Term):
        return t.get_svars()
    elif isinstance(t, list):
        found = set()
        res = []
        for s in t:
            for svar in s.get_svars():
                if svar not in found:
                    res.append(svar)
                    found.add(svar)
        return res
    else:
        raise TypeError

def get_vars(t: Union[Term, list[Term]]) -> list[Term]:
    """Returns list of variables in a term or a list of terms."""
    if isinstance(t, Term):
        return t.get_vars()
    elif isinstance(t, list):
        found = set()
        res = []
        for s in t:
            for var in s.get_vars():
                if var not in found:
                    res.append(var)
                    found.add(var)
        return res
    else:
        raise TypeError

def get_stvars(t: Union[Term, list[Term]]) -> list[Term]:
    """Get the list of type variables for a term."""
    if isinstance(t, Term):
        return t.get_stvars()
    elif isinstance(t, list):
        found = set()
        res = []
        for s in t:
            for stvar in s.get_stvars():
                if stvar not in found:
                    res.append(stvar)
                    found.add(stvar)
        return res
    else:
        raise TypeError
    
def equals(T):
    """Returns the equals constant for the given type."""
    return Const("equals", TFun(T, T, BoolType))

# def Eq(s, t):
#     """Construct the term s = t."""
#     if isinstance(s, (int, Fraction)):
#         assert isinstance(t, Term), "Eq: one of the arguments must be a term."
#         s = Number(t.get_type(), s)
#     elif isinstance(t, (int, Fraction)):
#         t = Number(s.get_type(), t)

def Not(t):
    """Return negation of boolean term t."""
    # typecheck.checkinstance('Not', t, Term)
    return neg(t)

def forall(T):
    return Const("all", TFun(TFun(T, BoolType), BoolType))



# def Nat(n):
#     """Construct natural number with value n."""
#     return Number(NatType, n)

# def Int(n):
#     """Construct integer with value n."""
#     return Number(IntType, n)

# def Real(r):
#     """Construct real number with value r."""
#     return Number(RealType, r)



def BoolVars(s):
    """Create a list of variables of boolean type.

    s is a string containing space-separated names of variables.

    """
    nms = s.split(' ')
    return [Var(nm, BoolType) for nm in nms]

def NatVars(s):
    """Create a list of variables of nat type.

    s is a string containing space-separated names of variables.

    """
    nms = s.split(' ')
    return [Var(nm, NatType) for nm in nms]

def IntVars(s):
    """Create a list of variables of int type.

    s is a string containing space-separated names of variables.

    """
    nms = s.split(' ')
    return [Var(nm, IntType) for nm in nms]

def RealVars(s):
    """Create a list of variables of int type.

    s is a string containing space-separated names of variables.

    """
    nms = s.split(' ')
    return [Var(nm, RealType) for nm in nms]


def Lambda(*args):
    """Construct the term %x_1 ... x_n. body.
    
    The arguments are x_1, ..., x_n, body.

    Here x_1, ..., x_n must be variables (or schematic variable) and
    body is a term possibly depending on x_1, ..., x_n.
    
    """
    # typecheck.checkinstance('Lambda', args, [Term])
    if len(args) < 2:
        raise TermException("Lambda: must provide two terms.")
    body = args[-1]
    for x in reversed(args[:-1]):
        if not (x.is_var() or x.is_svar()):
            raise TermException("Lambda: x must be a variable. Got %s" % str(x))
        body = Abs(x.name, x.T, body.abstract_over(x))
    return body

def Or(*args):
    """Return the disjunction of the arguments."""
    # typecheck.checkinstance('Or', args, [Term])
    if not args:
        return false
    res = args[-1]
    for s in reversed(args[:-1]):
        res = disj(s, res)
    return res

def And(*args):
    """Return the conjunction of the arguments."""
    # typecheck.checkinstance('And', args, [Term])
    if not args:
        return true
    res = args[-1]
    for s in reversed(args[:-1]):
        res = conj(s, res)
    return res