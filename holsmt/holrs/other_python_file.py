from .holrs import TConst, Type, TyInst

# pre-defined type constants
BoolType = TConst("bool")

NatType = TConst("nat")
IntType = TConst("int")
RealType = TConst("real")

# renamed method
Type.match = Type._match

# todo? Is there a better way to do this?
def __TyInst__str(tyinst):
    return ', '.join("'%s := %s" % (nm, T) for nm, T in tyinst.items())

TyInst.__str__ = __TyInst__str