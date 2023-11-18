import unittest

from data import bitvector
from logic import basic
from logic.tests.conv_test import test_conv

basic.load_theory('bitvector')


class BitvectorTest(unittest.TestCase):
    def testAddComm(self):
        test_data = [
            ("x + y", "y + x"),
            ("x + y + z", "z + (x + y)"),
        ]

        vars32 = {"x": "word32", "y": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_add_comm(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_add_comm(), vars=vars64, t=expr, t_res=res)

    def testAddAssoc(self):
        test_data = [
            ("x + y + z", "x + (y + z)"),
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_add_assoc(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_add_assoc(), vars=vars64, t=expr, t_res=res)

    def testDistribLeft(self):
        test_data = [
            ("x * (y + z)", "x * y + x * z"),
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_distrib_left(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_distrib_left(), vars=vars64, t=expr, t_res=res)

    def testMultiComm(self):
        test_data = [
            ("x * y", "y * x"),
        ]

        vars32 = {"x": "word32", "y": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_multi_comm(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_multi_comm(), vars=vars64, t=expr, t_res=res)

    def testDistribLeft2(self):
        test_data = [
            ("x * (y + z)", "y * x + z * x"),
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_distrib_left2(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_distrib_left2(), vars=vars64, t=expr, t_res=res)

    def testMultipleAssoc(self):
        test_data = [
            ("(x * y) * z", "(x * z) * y"),
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_swap_mult_r(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_swap_mult_r(), vars=vars64, t=expr, t_res=res) 

    def testNormMultAtom(self):
        test_data = [
            ("(x * y * f(x)) * x", "x * x * y * f(x)"),
            ("(x * y * f(x)) * y", "x * y * y * f(x)"),
            ("y * x", "x * y")
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_mult_atom(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_mult_atom(), vars=vars64, t=expr, t_res=res) \

    def testNormMultMonomial(self):
        test_data = [
            ("(x * y * f(x)) * (x * y * f(x))", "x * x * y * y * f(x) * f(x)"),
            ("(2 * x) * (3 * y)", "2 * x * y * 3")
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_mult_monomial(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_mult_monomial(), vars=vars64, t=expr, t_res=res)

    def testSwapAddR(self):
        test_data = [
            ("(x + y) + z", "(x + z) + y")
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_swap_add_r(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_swap_add_r(), vars=vars64, t=expr, t_res=res)

    def testNormAddMonomial(self):
        test_data = [
            # ("1 * y + 1 * x", "1 * x + 1 * y"),
            ("(x + z) + y", "(x + y) + z"),
            # ("0 + 2 * x", "2 * x"), #cant simplify 0
            # ("1 * x + 2 * x", "3 * x")
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_add_monomial(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_add_monomial(), vars=vars64, t=expr, t_res=res)

    def testNormAddPolynomial(self):
        test_data = [
            # ("1 * y + 1 * x", "1 * x + 1 * y"),
            # ("(x + z) + y", "(x + y) + z"),
            # ("0 + 2 * x", "2 * x"), #cant simplify 0
            ("0 + x", "x")
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_add_polynomial(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_add_polynomial(), vars=vars64, t=expr, t_res=res)        

    def testNormMultPolyMonomial(self):
        test_data = [
            # ("1 * y + 1 * x", "1 * x + 1 * y"),
            # ("(x + z) + y", "(x + y) + z"),
            # ("0 + 2 * x", "2 * x"), #cant simplify 0
            ("(x + y) * (x + y)", "x * x + x * y + x * y + y * y")
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_mult_polynomials(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_mult_polynomials(), vars=vars64, t=expr, t_res=res)

    def testNormFull(self):
        test_data = [
            ("1 * y + 1 * x", "1 * x + 1 * y"), #order error
            ("(x + z) + y", "(x + y) + z"),
            ("0 + 2 * x", "2 * x"), #cant simplify 0
            ("(x + y) * (x + y)", "x * x + x * y + x * y + y * y"),
            # ("0*(x+y)" ,"0"),
            ("0 + x + y", "x + y")
        ]

        vars32 = {"x": "word32", "y": "word32", "z": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_full(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64", "z": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_norm_full(), vars=vars64, t=expr, t_res=res)

if __name__ == "__main__":
    unittest.main()
