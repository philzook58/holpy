# Author: Bohua Zhan

import unittest

from kernel.term import Var, Term, get_vars
from kernel.thm import Thm
from logic import basic
from imperative.expr import Var, true, eq, neq, neg, plus, minus, uminus, times, less, less_eq, zero, one
from imperative.com import Skip, Assign, Seq, Cond, While
from imperative.parser2 import cond_parser
from imperative import imp
from server.server import ProofState
from server import method
from syntax import printer

thy = basic.load_theory('hoare')

class ComTest(unittest.TestCase):
    def testPrintCom(self):
        x = Var('x')
        test_data = [
            (Skip(), ["skip"]),
            (Assign("x", plus(x,one)), ["x := x + 1"]),
            (Seq(Assign("x", plus(x,one)), Assign("x", plus(x,one))),
             ["x := x + 1;", "x := x + 1"]),
            (Cond(eq(x,one), Skip(), Assign("x", plus(x,one))),
             ["if (x == 1) then", "  skip", "else", "  x := x + 1"]),
            (While(eq(x,one), true, Assign("x", plus(x,one))),
             ["while (x == 1) {", "  [true]", "  x := x + 1", "}"])
        ]

        for com, s in test_data:
            lines, _ = com.print_com()
            self.assertEqual(lines, s)

    def testComputeWP(self):
        a = Var('a')
        b = Var('b')
        test_data = [
            (Skip(), "a <= m", "a <= m"),
            (Assign("m", plus(a,b)), "a <= m", "a <= a + b"),
            (Seq(Assign("m", plus(a,b)), Assign("n", minus(a,b))),
             "a <= m & n <= a", "a  <= a + b & a - b <= a"),
        ]

        for c, post, pre in test_data:
            post = cond_parser.parse(post)
            pre = cond_parser.parse(pre)
            self.assertEqual(c.compute_wp(post), pre)

    def testVCG(self):
        a = Var('a')
        b = Var('b')
        A = Var('A')
        B = Var('B')
        test_data = [
            (Seq(Assign("m", plus(a,b)), Assign("n", minus(a,b))),
             "0 <= b", "a <= m & n <= a",
             ["0 <= b --> a <= a + b & a - b <= a"]),

            (While(less(zero,a), less_eq(zero,a), Assign("a", minus(a,one))),
             "0 <= a", "a == 0",
             ["0 <= a --> 0 <= a",
              "0 <= a & 0 < a --> 0 <= a - 1",
              "0 <= a & ~0 < a --> a == 0"]),

            (While(neq(a,A), eq(b,times(a,B)), Seq(Assign('b',plus(b,B)), Assign('a',plus(a,one)))),
             "a == 0 & b == 0", "b == A * B",
             ["a == 0 & b == 0 --> b == a * B",
              "b == a * B & a != A --> b + B == (a + 1) * B",
              "b == a * B & ~a != A --> b == A * B"]),

            (Cond(neq(a,zero), Assign('a',zero), Skip()),
             "true", "a == 0",
             ["true --> if a != 0 then 0 == 0 else a == 0"]),
        ]

        for c, pre, post, expected_vcs in test_data:
            pre = cond_parser.parse(pre)
            post = cond_parser.parse(post)
            expected_vcs = [cond_parser.parse(vc) for vc in expected_vcs]
            c.pre = [pre]
            c.compute_wp(post)
            _, vc_dct = c.print_com()
            vcs = [v for k, v in sorted(vc_dct.items())]
            self.assertEqual(vcs, expected_vcs)

    def testVerify(self):
        a = Var('a')
        c = Cond(less_eq(zero,a), Assign('c',a), Assign('c', uminus(a)))
        pre = cond_parser.parse("true")
        post = cond_parser.parse("c == abs(a)")
        c.pre = [pre]
        c.compute_wp(post)

        _, vcs = c.print_com()
        vc = vcs[0]
        self.assertEqual(vc, cond_parser.parse("true --> if 0 <= a then a == abs(a) else -a == abs(a)"))

        goal = vc.convert_hol({"a": "int"})
        As, C = goal.strip_implies()
        state = ProofState.init_state(thy, get_vars(goal), As, C)
        state.rewrite_goal(1, "int_abs_def")
        method.apply_method(state, {
            'method_name': 'z3',
            'goal_id': "1"})
        self.assertEqual(state.check_proof(no_gaps=True), Thm([], goal))

    def testVerify2(self):
        m = Var('m')
        n = Var('n')
        c = Cond(less_eq(m,n), Assign('c',n), Assign('c',m))
        pre = cond_parser.parse("true")
        post = cond_parser.parse("c == max(m,n)")
        c.pre = [pre]
        c.compute_wp(post)

        _, vcs = c.print_com()
        vc = vcs[0]
        self.assertEqual(vc, cond_parser.parse("true --> if m <= n then n == max(m,n) else m == max(m,n)"))

        goal = vc.convert_hol({"m": "int", "n": "int"})
        As, C = goal.strip_implies()
        state = ProofState.init_state(thy, get_vars(goal), As, C)
        state.rewrite_goal(1, "int_max_def")
        method.apply_method(state, {
            'method_name': 'z3',
            'goal_id': "1"})
        self.assertEqual(state.check_proof(no_gaps=True), Thm([], goal))


if __name__ == "__main__":
    unittest.main()
