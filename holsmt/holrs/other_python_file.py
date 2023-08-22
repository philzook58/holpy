from .holrs import TConst, Type, Const, TFun, Term

# pre-defined type constants
BoolType = TConst("bool")
"""A pre-defined constant representing the boolean type."""

NatType = TConst("nat")
"""A pre-defined constant representing the natural number type."""

IntType = TConst("int")
"""A pre-defined constant representing the integer type."""

RealType = TConst("real")
"""A pre-defined constant representing the real number type."""

# renamed method, funciton alias
Type.match = Type._match

true = Const("true", BoolType)
false = Const("false", BoolType)

neg = Const("neg", TFun(BoolType, BoolType))
conj = Const("conj", TFun(BoolType, BoolType, BoolType))
disj = Const("disj", TFun(BoolType, BoolType, BoolType))
implies = Const("implies", TFun(BoolType, BoolType, BoolType))

def equals(T: Type) -> Term:
    """Returns the equals constant for the given type."""
    return Const("equals", TFun(T, T, BoolType))

def plus(T):
    return Const('plus', TFun(T, T, T))

def minus(T):
    return Const('minus', TFun(T, T, T))

def uminus(T):
    return Const('uminus', TFun(T, T))

def times(T):
    return Const('times', TFun(T, T, T))

def divides(T):
    return Const('real_divide', TFun(T, T, T))

def of_nat(T):
    return Const('of_nat', TFun(NatType, T))

def of_int(T):
    return Const('of_int', TFun(IntType, T))

def nat_power(T):
    return Const('power', TFun(T, NatType, T))

def int_power(T):
    return Const('power', TFun(T, IntType, T))

def real_power(T):
    return Const('power', TFun(T, RealType, T))

def less_eq(T):
    return Const('less_eq', TFun(T, T, BoolType))

def less(T):
    return Const('less', TFun(T, T, BoolType))

def greater_eq(T):
    return Const('greater_eq', TFun(T, T, BoolType))

def greater(T):
    return Const('greater', TFun(T, T, BoolType))

# Binary bits 0 and 1
nat_zero = Const('zero', NatType)
nat_one = Const('one', NatType)
bit0 = Const("bit0", TFun(NatType, NatType))
bit1 = Const("bit1", TFun(NatType, NatType))