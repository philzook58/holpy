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

        vars32 = {"x": "word32", "y": "word32"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_add_assoc(), vars=vars32, t=expr, t_res=res)

        vars64 = {"x": "word64", "y": "word64"}
        for expr, res in test_data:
            test_conv(self, 'bitvector', bitvector.bv_add_assoc(), vars=vars64, t=expr, t_res=res)


if __name__ == "__main__":
    unittest.main()
