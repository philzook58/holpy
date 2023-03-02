"""Unit test for conditions"""

import unittest

from integral import parser
from integral.conditions import Conditions


class ConditionsTest(unittest.TestCase):
    def testIsPositive(self):
        test_data = [
            # Constant
            ("1", [], True),

            # Variable
            ("a", ["a > 0"], True),

            # Function
            ("exp(a)", [], True),
            ("sqrt(a)", ["a > 0"], True),
            ("sqrt(a)", ["a >= 0"], False),

            # Power
            ("a ^ (1/2)", ["a > 0"], True),
            ("a ^ (-1/2)", ["a > 0"], True),
            ("x ^ a", ["x > 0"], True),
            ("x ^ a + 1", ["x > 0"], True),
            ("x ^ 2", ["x != 0"], True),
            ("cos(a) ^ 2", ["cos(a) != 0"], True),
            ("4 * x ^ 2 * cos(a) ^ 2 + (x ^ 2 - 1) ^ 2", ["cos(a) != 0", "x != 0"], True),

            # Plus
            ("m + 1", ["m >= 0"], True),
            ("1 + x ^ 2", [], True),

            # Polynomial
            ("x * (4 - x)", ["x > 0", "4 - x > 0"], True),

            # Integrals
            ("INT x:[1,oo]. 1 / x ^ 2", [], True),
            ("INT x:[0,oo]. exp(-1/2 * x ^ 2)", [], True),

            # Trigonometric
            ("sin(m * pi)", ["m > 0", "m < 1"], True),

            # Equality
            ("1/3 * u - 1/3", ["u = 1 + 3 * exp(-x)"], True),

            # Hyperbolic
            ("x ^ 4 + 2 * x ^ 2 * cosh(2 * a) + 1", [], True),
        ]

        for a, conds_str, res in test_data:
            e = parser.parse_expr(a)
            conds = Conditions()
            for s in conds_str:
                conds.add_condition(parser.parse_expr(s))
            self.assertEqual(conds.is_positive(e), res, msg="Failed with %s" % e)

    def testIsNonzero(self):
        test_data = [
            ("x + a", ["x != -a"], True),
        ]

        for a, conds_str, res in test_data:
            e = parser.parse_expr(a)
            conds = Conditions()
            for s in conds_str:
                conds.add_condition(parser.parse_expr(s))
            self.assertEqual(conds.is_nonzero(e), res, msg="Failed with %s" % e)

            
if __name__ == "__main__":
    unittest.main()
