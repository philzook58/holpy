from .holrs import TConst, Type, TyInst

# pre-defined type constants
BoolType = TConst("bool")
"""A pre-defined constant representing the boolean type."""

NatType = TConst("nat")
"""A pre-defined constant representing the natural number type."""

IntType = TConst("int")
"""A pre-defined constant representing the integer type."""

RealType = TConst("real")
"""A pre-defined constant representing the real number type."""

# renamed method
Type.match = Type._match
