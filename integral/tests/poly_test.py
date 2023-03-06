"""Unit test for polynomial module."""

import unittest

from integral.poly import normalize, from_poly, to_poly
from integral.parser import parse_expr
from integral.context import Context


class PolynomialTest(unittest.TestCase):
    def testMultiply(self):
        test_data = [
            ("sqrt(2) - 1", "2 * sqrt(2) + 3", "sqrt(2) + 1"),
        ]

        ctx = Context()
        for e1, e2, res in test_data:
            e1 = parse_expr(e1)
            e2 = parse_expr(e2)
            self.assertEqual(str(from_poly(to_poly(e1, ctx) * to_poly(e2, ctx))), res)

    def testNormalize(self):
        test_data = [
            ("1/2 * pi", "pi / 2"),
            ("-1/2 * pi", "-(pi / 2)"),
            ("exp(2)", "exp(2)"),
            ("-1/2", "-1/2"),
            ("4 ^ (5/6)", "2 * 2 ^ (2/3)"),
            ("1/4 * (INT x:[0,oo]. x)", "1/4 * (INT x:[0,oo]. x)"),
            ("sqrt(-log(exp(-y)))", "sqrt(-log(exp(-y)))"),
        ]

        for e, res in test_data:
            e = parse_expr(e)
            ctx = Context()
            self.assertEqual(str(normalize(e, ctx)), res)
            self.assertEqual(normalize(e, ctx), parse_expr(res))


if __name__ == "__main__":
    unittest.main()
