# Author: Bohua Zhan

import unittest

from kernel import type as hol_type
from kernel.type import STVar, TVar, TFun, TyInst
from kernel import term
from kernel.term import SVar, Var, Const, Comb, Abs, Bound, And, Or, Lambda, Binary, Inst
from kernel.term import TermException, TypeCheckException

import fractions

Ta = TVar("a")
Tb = TVar("b")
STa = STVar("a")
STb = STVar("b")
Taa = TFun(Ta, Ta)        # 'a => 'a
Tab = TFun(Ta, Tb)        # 'a => 'b
Taab = TFun(Ta, Ta, Tb)   # 'a => 'a => 'b
a = Var("a", Ta)
b = Var("b", Tb)
c = Const("c", Ta)
f = Var("f", Tab)     # f: 'a => 'b
f2 = Var("f2", Taab)  # f2: 'a => 'a => 'b
g = Var("g", Taa)     # g: 'a => 'a
B0 = Bound(0)
B1 = Bound(1)

class TermTest(unittest.TestCase):
    def setUp(self):
        self.type_printer, self.term_printer = hol_type.type_printer, term.term_printer
        hol_type.type_printer, term.term_printer = None, None

    def tearDown(self):
        hol_type.type_printer, term.term_printer = self.type_printer, self.term_printer

    def testReprTerm(self):
        test_data = [
            (a, "Var(a, 'a)"),
            (f, "Var(f, 'a => 'b)"),
            (c, "Const(c, 'a)"),
            (f(a), "Comb(Var(f, 'a => 'b), Var(a, 'a))"),
            (f2(a,a), "Comb(Comb(Var(f2, 'a => 'a => 'b), Var(a, 'a)), Var(a, 'a))"),
            (f(g(a)), "Comb(Var(f, 'a => 'b), Comb(Var(g, 'a => 'a), Var(a, 'a)))"),
            (Abs("x", Ta, b), "Abs(x, 'a, Var(b, 'b))"),
            (Abs("x", Ta, B0), "Abs(x, 'a, Bound(0))"),
            (Abs("x", Ta, "y", Ta, B0), "Abs(x, 'a, Abs(y, 'a, Bound(0)))"),
            (Abs("x", Ta, "y", Ta, B1), "Abs(x, 'a, Abs(y, 'a, Bound(1)))"),
        ]

        for t, repr_t in test_data:
            self.assertEqual(repr(t), repr_t)

    def testPrintTerm(self):
        test_data = [
            (a, "a"),
            (f, "f"),
            (c, "c"),
            (f(a), "f a"),
            (f2(a,a), "f2 a a"),
            (f(g(a)), "f (g a)"),
            (Abs("x", Ta, b), "%x. b"),
            (Abs("x", Ta, B0), "%x. x"),
            (Abs("x", Ta, "y", Ta, B0), "%x. %y. y"),
            (Abs("x", Ta, "y", Ta, B1), "%x. %y. x"),
            (Abs("x", Ta, "y", Ta, f(B0)), "%x. %y. f y"),
            (Abs("x", Ta, "y", Ta, f2(B1,B0)), "%x. %y. f2 x y"),
        ]

        for t, str_t in test_data:
            self.assertEqual(str(t), str_t)

    def testEquals(self):
        test_data = [
            (Abs("x", Ta, b), Abs("y", Ta, b)),
            (Abs("x", Tb, f(B0)), Abs("y", Tb, f(B0))),
        ]

        for t1, t2 in test_data:
            self.assertEqual(t1, t2)

    def testGetType(self):
        test_data = [
            (a, Ta),
            (f, Tab),
            (c, Ta),
            (f(a), Tb),
            (f2(a,a), Tb),
            (f(g(a)), Tb),
            (Abs("x", Ta, b), Tab),
            (Abs("x", Ta, B0), Taa),
            (Abs("x", Ta, "y", Tb, B0), TFun(Ta,Tb,Tb)),
            (Abs("x", Ta, "y", Tb, B1), TFun(Ta,Tb,Ta)),
            (Abs("x", Ta, "y", Ta, f(B0)), TFun(Ta,Ta,Tb)),
            (Abs("x", Ta, "y", Ta, f2(B1,B0)), TFun(Ta,Ta,Tb)),
        ]

        for t, T in test_data:
            self.assertEqual(t.get_type(), T)

    def testIsOpen(self):
        test_data = [
            (a, False),
            (f(a), False),
            (B0, True),
            (f(B0), True),
            (Abs("x", Ta, B0), False),
            (Abs("x", Ta, B1), True),
            (Abs("x", Ta, "y", Ta, B0), False),
            (Abs("x", Ta, "y", Ta, B1), False),
            (Abs("x", Ta, "y", Ta, Bound(2)), True),
        ]

        for t, res in test_data:
            self.assertEqual(t.is_open(), res)

    def testSubstType(self):
        test_data = [
            (Var('a', STa), Var("a", Tb)),
            (Const("c", STa), Const("c", Tb)),
            (Var("f", TFun(STa,Tb))(Var("a", STa)), Var("f", TFun(Tb,Tb))(Var("a", Tb))),
            (Abs("x", STa, B0), Abs("x", Tb, B0)),
            (Abs("x", STa, Var('a', STa)), Abs("x", Tb, Var("a", Tb))),
        ]

        for t, res in test_data:
            self.assertEqual(t.subst_type(TyInst(a=Tb)), res)

    def testSubst(self):
        test_data = [
            (SVar('a', Ta), c),
            (c, c),
            (f(SVar('a', Ta)), f(c)),
            (Abs("x", Ta, B0), Abs("x", Ta, B0)),
            (Abs("x", Ta, SVar('a', Ta)), Abs("x", Ta, c)),
        ]

        for t, res in test_data:
            self.assertEqual(t.subst(Inst(a=c)), res)

    def testSubstFail(self):
        self.assertRaises(TermException, SVar('a', TVar('a')).subst, Inst(a=b))

    def testSubstBound(self):
        test_data = [
            (Abs("x", Ta, B0), c, c),
            (Abs("x", Ta, a), c, a),
            (Abs("x", Ta, "y", Tb, B0), c, Abs("y", Tb, B0)),
            (Abs("x", Ta, "y", Tb, B1), c, Abs("y", Tb, c)),
            (Abs("x", Ta, "y", Tb, f2(B1,B0)), c, Abs("y", Tb, f2(c,B0))),
            (Abs("x", Ta, B0), B0, B0),
        ]

        for t, s, res in test_data:
            self.assertEqual(t.subst_bound(s), res)

    def testSubstBoundFail(self):
        self.assertRaises(TermException, a.subst_bound, b)

    def testStripComb(self):
        self.assertEqual(f2.strip_comb(), (f2, []))
        self.assertEqual(f2(a).strip_comb(), (f2, [a]))
        self.assertEqual(f2(a,b).strip_comb(), (f2, [a, b]))

    def testBetaConv(self):
        test_data = [
            (Comb(Abs("x", Ta, B0), c), c),
            (Comb(Abs("x", Ta, a), c), a),
        ]

        for t, res in test_data:
            self.assertEqual(t.beta_conv(), res)

    def testBetaNorm(self):
        x = Var('x', Ta)
        y = Var('y', Ta)
        test_data = [
            (Lambda(x, x)(x), x),
            (Lambda(x, Lambda(y, y))(x, y), y),
            (Lambda(x, Lambda(y, x))(x, y), x),
        ]

        for t, res in test_data:
            self.assertEqual(t.beta_norm(), res)

    def testOccursVar(self):
        test_data = [
            (a, a, True),
            (a, b, False),
            (f(a), a, True),
            (f(a), b, False),
            (Abs("a", Ta, a), a, True),
            (Abs("a", Ta, b), b, True),
            (Abs("a", Ta, B0), a, False),
        ]

        for s, t, res in test_data:
            self.assertEqual(s.occurs_var(t), res)

    def testAbstractOver(self):
        test_data = [
            (a, a, B0),
            (Abs("b", Ta, a), a, Abs("b", Ta, B1)),
            (Abs("a", Ta, a), a, Abs("a", Ta, B1)),
            (f(a), a, f(B0)),
            (c, a, c),
        ]

        for s, t, res in test_data:
            self.assertEqual(s.abstract_over(t), res)

    def testAbstractOverFail(self):
        self.assertRaises(TermException, Comb(f,a).abstract_over, Comb(f,a))

    def testAbstractOverFail2(self):
        self.assertRaises(TermException, a.abstract_over, Var("a", Tb))

    def testCheckedGetType(self):
        test_data = [
            (a, Ta),
            (c, Ta),
            (f(a), Tb),
            (f2(a,a), Tb),
            (f(g(a)), Tb),
            (Comb(Abs("x", Ta, B0), a), Ta),
        ]

        for t, T in test_data:
            self.assertEqual(t.checked_get_type(), T)

    def testCheckedGetTypeFail(self):
        test_data = [
            Comb(a,a),
            Comb(f,f),
            f(a,a),
            f(b),
            Comb(Abs("x", Ta, B0), b),
        ]

        for t in test_data:
            self.assertRaises(TypeCheckException, t.checked_get_type)

    def testGetVars(self):
        test_data = [
            (a, [a]),
            (f(a), [f, a]),
            (f(c), [f]),
            ([a, f(c)], [a, f]),
        ]

        for t, res in test_data:
            self.assertEqual(term.get_vars(t), res)

    def testConj(self):
        test_data = [
            ([], term.true),
            ([a], a),
            ([a, b], term.conj(a, b)),
            ([a, b, a], term.conj(a, term.conj(b, a)))
        ]

        for ts, res in test_data:
            self.assertEqual(And(*ts), res)

    def testConjFail(self):
        self.assertRaises(TypeError, And, [a])

    def testStripConj(self):
        test_data = [
            (a, [a]),
            (And(a, b, a), [a, b, a])
        ]

        for t, res in test_data:
            self.assertEqual(t.strip_conj(), res)

    def testDisj(self):
        test_data = [
            ([], term.false),
            ([a], a),
            ([a, b], term.disj(a, b)),
            ([a, b, a], term.disj(a, term.disj(b, a)))
        ]

        for ts, res in test_data:
            self.assertEqual(Or(*ts), res)

    def testDisjFail(self):
        self.assertRaises(TypeError, Or, [a])

    def testStripDisj(self):
        test_data = [
            (a, [a]),
            (Or(a, b, a), [a, b, a])
        ]

        for t, res in test_data:
            self.assertEqual(t.strip_disj(), res)

    def testBinary(self):
        zero = term.nat_zero
        one = term.nat_one
        bit0 = term.bit0
        bit1 = term.bit1
        test_data = [
            (0, zero),
            (1, one),
            (2, bit0(one)),
            (3, bit1(one)),
            (4, bit0(bit0(one))),
            (5, bit1(bit0(one))),
            (6, bit0(bit1(one))),
            (7, bit1(bit1(one))),
            (19, bit1(bit1(bit0(bit0(one))))),
            (127, bit1(bit1(bit1(bit1(bit1(bit1(one))))))),
        ]

        for n, t in test_data:
            self.assertEqual(Binary(n), t)
            self.assertEqual(t.dest_binary(), n)

    def testBinaryLarge(self):
        test_data = [
            100, 10000, 100000, 111111, 999999, 10101010101, 12345678987654321,
        ]

        for n in test_data:
            self.assertEqual(Binary(n).dest_binary(), n)

    def testIsBinary(self):
        zero = term.nat_zero
        one = term.nat_one
        bit0 = term.bit0
        bit1 = term.bit1
        test_data = [
            (zero, True),
            (one, True),
            (bit0(one), True),
            (bit0, False),
            (bit1, False),
        ]

        for n, b in test_data:
            self.assertEqual(n.is_binary(), b)

    def testIsNumber(self):
        zero = term.nat_zero
        one = term.nat_one
        bit0 = term.bit0
        bit1 = term.bit1
        real_frac = term.Number(hol_type.RealType, 3) / term.Number(hol_type.RealType, 4)
        real_frac_b = term.Number(hol_type.RealType, fractions.Fraction(3, 4)) / term.Number(hol_type.RealType, fractions.Fraction(1, 2))
        nat_frac = term.Number(hol_type.NatType, 42) / term.Number(hol_type.NatType, 84)

        test_data = [
            (zero, True),
            (one, True),
            (term.Number(hol_type.NatType, 42), True),
            (term.Number(hol_type.NatType, 11112222333344445555), True),
            (term.Number(hol_type.RealType, fractions.Fraction(2, 4)), True),
            (term.Number(hol_type.RealType, fractions.Fraction(11112222333344445555, 2222333344445555)), True),
            (-term.Number(hol_type.NatType, 42), True),
            (-term.Number(hol_type.NatType, 11112222333344445555), True),
            (-term.Number(hol_type.RealType, fractions.Fraction(2, 4)), True),
            (-term.Number(hol_type.RealType, fractions.Fraction(11112222333344445555, 2222333344445555)), True),
            (-one, True),
            (real_frac, True),
            (real_frac_b, False),
            (-zero, False),
            (bit0, False),
            (bit1, False),
            (nat_frac, False),
        ]

        for t, expected_result in test_data:
            with self.subTest(t=t):
                self.assertEqual(t.is_number(), expected_result)


if __name__ == "__main__":
    unittest.main()
