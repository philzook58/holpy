import unittest
import sympy

from integral.parser import parse_expr
from integral.sympywrapper import partial_fraction, convert_to_sympy

class SympywrapperTest(unittest.TestCase):
    def testPartialFraction(self):
        test_data = [
            ("1 / (x ^ 2 - 1)", "-(1 / (2 * x + 2)) + 1 / (2 * x - 2)"),
        ]

        for e, res in test_data:
            e = parse_expr(e)
            self.assertEqual(str(partial_fraction(e)), res)


if __name__ == "__main__":
    unittest.main()
