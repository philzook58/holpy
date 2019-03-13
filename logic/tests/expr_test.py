# Author: Bohua Zhan

import unittest

from kernel.type import TFun
from kernel.term import Var
from kernel.thm import Thm
from logic import basic
from logic import nat
from logic import expr
from syntax import printer

natT = nat.natT
zero = nat.zero
one = nat.one
five = nat.to_binary(5)

thy = basic.loadTheory('expr')

class ExprTest(unittest.TestCase):
    def testProveEvalI(self):
        s = Var("s", TFun(natT, natT))
        t = expr.Plus(expr.V(one), expr.N(five))
        n = nat.plus(s(one), five)
        pt = expr.prove_avalI_macro().get_proof_term(thy, (s, t))
        th = Thm([], expr.avalI(s, t, n))
        self.assertEqual(thy.check_proof(pt.export()), th)


if __name__ == "__main__":
    unittest.main()
