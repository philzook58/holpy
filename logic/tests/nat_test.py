# Author: Bohua Zhan

import unittest

from kernel.thm import Thm
from logic import nat
from logic import basic
from logic.nat import zero, one, bit0, bit1

thy = basic.NatTheory

class NatTest(unittest.TestCase):
    def testPlus(self):
        self.assertEqual(nat.mk_plus(), zero)
        self.assertEqual(nat.mk_plus(zero), zero)
        self.assertEqual(nat.mk_plus(one), one)
        self.assertEqual(nat.mk_plus(zero, one), nat.plus(zero, one))
        self.assertEqual(nat.mk_plus(*([zero]*3)), nat.plus(nat.plus(zero, zero), zero))

    def testBinary(self):
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

        for n, binary in test_data:
            self.assertEqual(nat.to_binary(n), binary)
            self.assertEqual(nat.from_binary(binary), n)

    def testBinaryLarge(self):
        test_data = [
            100, 10000, 100000, 111111, 999999, 10101010101, 12345678987654321,
        ]

        for n in test_data:
            self.assertEqual(nat.from_binary(nat.to_binary(n)), n)

    def testIsBinary(self):
        test_data = [
            (zero, True),
            (nat.Suc(zero), True),
            (nat.Suc(one), False),
            (bit0(one), True),
            (bit0(nat.Suc(bit0(one))), False),
            (bit0, False),
            (bit1, False),
        ]

        for n, b in test_data:
            self.assertEqual(nat.is_binary(n), b)

    def testSucConv(self):
        test_data = [
            0, 1, 2, 3, 4, 5, 6, 7, 19, 127, 1000, 1001,
        ]

        cv = nat.Suc_conv()
        for n in test_data:
            t = nat.Suc(nat.to_binary(n))
            res_th = Thm.mk_equals(t, nat.to_binary(n + 1))
            self.assertEqual(cv(t), res_th)
            prf = cv.get_proof_term(t).export()
            self.assertEqual(thy.check_proof(prf), res_th)

    def testAddConv(self):
        test_data = [
            (0, 2),
            (2, 0),
            (1, 2),
            (2, 1),
            (2, 2),
            (2, 3),
            (3, 2),
            (3, 3),
            (5, 5),
            (10, 5),
            (12345,98765),
        ]

        cv = nat.add_conv()
        for m, n in test_data:
            t = nat.mk_plus(nat.to_binary(m), nat.to_binary(n))
            res_th = Thm.mk_equals(t, nat.to_binary(m + n))
            self.assertEqual(cv(t), res_th)
            prf = cv.get_proof_term(t).export()
            self.assertEqual(thy.check_proof(prf), res_th)


if __name__ == "__main__":
    unittest.main()
