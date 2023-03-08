import unittest
import sympy

from integral.context import Context
from integral.parser import parse_expr
from integral.sympywrapper import partial_fraction, convert_to_sympy

class SympywrapperTest(unittest.TestCase):
    def testPartialFraction(self):
        test_data = [
            ("1 / (x ^ 2 - 1)", "-(1 / (2 * x + 2)) + 1 / (2 * x - 2)"),
            ("(x ^ 3 - 12 * x ^ 2 - 42) / (x - 3)", "-(123 / (x - 3)) + x ^ 2 - 9 * x - 27")
        ]

        for e, res in test_data:
            ctx = Context()
            ctx.add_condition("x > 4")
            e = parse_expr(e)
            self.assertEqual(str(partial_fraction(e, ctx)), res)


if __name__ == "__main__":
    unittest.main()
