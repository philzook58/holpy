"""Unit test for type"""

import unittest

from holrs import Type, TConst, TVar, STVar, TFun, print_address, TyInst, BoolType, NatType, IntType, RealType
from collections import UserDict

def basic_test(T: Type):
    display = str(T)
    print(display, "is_tvar:", T.is_tvar())
    print(display, "is_stvar:", T.is_stvar())
    print(display, "is_fun:", T.is_fun())
    print(display, "is_tconst:", T.is_tconst())
    print(display, "size:", T.size())
    # print(display, "domain_type:", T.domain_type())
    # print(display, "range_type:", T.range_type())
    # print(display, "strip_type:", T.strip_type())
    # print(display, "addr:", hex(id(T)))
    # print(print_address(T))


def main():
    Ta = TVar("a")
    Tb = TVar("b")
    Tc = TVar("c")
    STa = STVar("a")
    PyDict = {
        "a": "Ta",
        "b": "Tb",
    }

    # my_dict = UserDict(PyDict)

    # print(STa.subst({"a": "Ta", "b": "Tb"}))
    print(STa.subst(TyInst(a=Ta, b=Tb)))
    p = TFun(STVar("a"), STVar("b"))
    print(p.subst(a=STVar("b"), b=STVar("a")))
    print(p.match(TFun(NatType, BoolType)))
    tyinst = TyInst(a=STVar("b"), b=STVar("a"))
    print(tyinst)
    # test_data = [
    #         (STa, Tb),
    #         (STb, Ta),
    #         (TFun(STa, Tb), TFun(Tb, Tb)),
    #         (TFun(STa, STb), TFun(Tb, Ta)),
    #         (TConst("list", STa), TConst("list", Tb)),
    #     ]

    #     for T, res in test_data:
    #         self.assertEqual(T.subst(TyInst(a=Tb, b=Ta)), res)
    # basic_test(BoolType)

    # a = TVar("a")
    # basic_test(a)


    # aa = TVar("a")
    # print("aa", "addr:", hex(id(aa)))
    # # print(print_address(aa))

    # # aaa = TVar("a")
    # # print("aaa", "addr:", hex(id(aaa)))
    # # print(print_address(aaa))

    # # other = TVar("aa")
    # # print("other", "addr:", hex(id(other)))
    # # print(print_address(other))

    # b = STVar("b")
    # basic_test(b)

    # fun = TConst("fun", NatType, BoolType)
    # basic_test(fun)
    # # print(fun, "domain_type:", fun.domain_type())
    # # print(fun, "range_type:", fun.range_type())
    # # print(fun, "strip_type:", fun.strip_type())

    # other_fun = TConst("fun", a, b)
    # basic_test(other_fun)
    # # print(other_fun, "domain_type:", other_fun.domain_type())
    # # print(other_fun, "range_type:", other_fun.range_type())
    # # print(other_fun, "strip_type:", other_fun.strip_type())

    # other_fun_2 = TConst("fun", BoolType, NatType, a, b)
    # print(other_fun_2, "domain_type:", other_fun_2.domain_type())
    # # print(other_fun_2, "range_type:", other_fun_2.range_type())
    # # print(other_fun_2, "strip_type:", other_fun_2.strip_type())


if __name__ == "__main__":
    main()
# TVar basic test
# print("TVar basic test")
# a = TVar("a")
# print("a.is_tvar: {}".format(a.is_tvar()))
# print("a.is_stvar: {}".format(a.is_stvar()))
# print("a.is_fun: {}".format(a.is_fun()))
# print("a.is_tconst: {}".format(a.is_tconst()))
# print("a.size: {}".format(a.size()))

# v = TVar("1")
# vv = TVar("1")
# vvv = TVar("1")

# x = TVar("2")
# xx = TVar("3")

# print("v addr: {}".format(hex(id(v))))
# print("vv addr: {}".format(hex(id(vv))))
# print("vvv addr: {}".format(hex(id(vvv))))
# print(print_address(v))
# print(print_address(vv))
# print(print_address(vvv))

# f = TConst("fun", NatType, BoolType, IntType)
# print(f.is_fun())
# print(f.domain_type())
# print(f.range_type())
# print(f.strip_type())

# stvar = STVar("b")
# print(stvar.is_tvar())
# print(stvar.is_stvar())

# print(v)

# fun = TConst("fun", NatType, BoolType)
# print(fun.is_fun())
# print(fun)