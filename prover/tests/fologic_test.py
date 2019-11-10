"""Unit test for fologic."""

import unittest

from kernel.type import boolT, TFun, TVar
from logic import basic
from syntax import parser
from syntax import printer
from prover import fologic


class FOLogicTest(unittest.TestCase):
    def testHasBound0(self):
        test_data = [
            ("%y::'a. !x::'a. y = y", True),
            ("%x::'a. y = y", False)
        ]

        thy = basic.load_theory('logic_base')
        ctxt = {'vars': {'y': TVar('a')}}
        for fm, res in test_data:
            fm = parser.parse_term(thy, ctxt, fm)
            self.assertEqual(fologic.has_bound0(fm.body), res)

    def testSimplify(self):
        test_data = [
            # Three test cases Section 3.5 of HPLAR.
            ("true --> (p <--> (p <--> false))", "p <--> ~p"),
            ("?x. ?y::'a. ?z. P x --> Q z --> false", "?x. ?z. P x --> ~Q z"),
            ("(!x. !y. P x | (P y & false)) --> ?z::'a. q", "(!x. P x) --> q")
        ]

        thy = basic.load_theory('logic_base')
        ctxt = {'vars': {'p': boolT, 'q': boolT,
                         'P': TFun(TVar('a'), boolT), 'Q': TFun(TVar('a'), boolT)}}
        for fm, res in test_data:
            fm = parser.parse_term(thy, ctxt, fm)
            res = parser.parse_term(thy, ctxt, res)
            self.assertEqual(fologic.simplify(fm), res)

    def testNNF(self):
        test_data = [
            # Test case from Section 3.5 of HPLAR.
            ("(!x. P x) --> ((?y. Q y) <--> ?z. P z & Q z)",
             "(?x. ~P x) | (?y. Q y) & (?z. P z & Q z) | (!y. ~Q y) & (!z. ~P z | ~Q z)")
        ]

        thy = basic.load_theory('logic_base')
        ctxt = {'vars': {'P': TFun(TVar('a'), boolT), 'Q': TFun(TVar('a'), boolT)}}
        for fm, res in test_data:
            fm = parser.parse_term(thy, ctxt, fm)
            res = parser.parse_term(thy, ctxt, res)
            self.assertEqual(fologic.nnf(fm), res)


if __name__ == "__main__":
    unittest.main()
