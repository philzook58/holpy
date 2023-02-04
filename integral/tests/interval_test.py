"""Unit tests for the interval package."""

import unittest

from integral.expr import Var
from integral.parser import parse_interval, parse_expr
from integral.interval import Interval, get_bounds_for_expr


class IntervalTest(unittest.TestCase):
    def testParseInterval(self):
        test_data = [
            "(0, 1)", "(0, 1]", "[0, 1)", "[0, 1]",
            "(INT x:[1,2]. 3 * x, 5)",
        ]
        
        for s in test_data:
            self.assertEqual(str(parse_interval(s)), s)

    def testGetBounds(self):
        test_data = [
            # Basic arithmetic
            ("x - 4", "(0, 1)", "(-4, -3)"),
            ("(x + 1) * (x + 2)", "(0, 1)", "(2, 6)"),
            ("(x + 1) * (x + 2)", "(0, 1]", "(2, 6]"),
            ("x ^ 2", "(-oo, oo)", "[0, oo)"),
            ("1 / x ^ 2", "(-oo, oo)", "(0, oo)"),
            ("x + 1", "(-1, -1/2)", "(0, 1/2)"),

            # Square root
            ("sqrt(x)", "(1, 4)", "(1, 2)"),
            ("1 / sqrt(x)", "(1, 4)", "(1/2, 1)"),
            ("1 / sqrt(2 * x)", "(1, 4)", "(1/4 * sqrt(2), 1/2 * sqrt(2))"),
        ]

        for s, i1, i2 in test_data:
            s = parse_expr(s)
            var_range = {Var('x'): parse_interval(i1)}
            self.assertEqual(str(get_bounds_for_expr(s, var_range)), i2)


if __name__ == "__main__":
    unittest.main()