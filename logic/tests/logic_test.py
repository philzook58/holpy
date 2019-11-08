# Author: Bohua Zhan

import unittest

from kernel.type import TVar, TFun, boolT
from kernel import term
from kernel.term import Term, Var, Abs, Bound
from kernel.macro import global_macros
from kernel.thm import Thm
from kernel.proof import Proof
from kernel.report import ProofReport
from logic.proofterm import ProofTerm, ProofTermDeriv
from logic import logic
from logic import basic
from logic import matcher
from logic.tests.conv_test import test_conv
from data import nat
from syntax import parser

Ta = TVar("a")
a = Var("a", boolT)
b = Var("b", boolT)
c = Var("c", boolT)
d = Var("d", boolT)
x = Var("x", Ta)
y = Var("y", Ta)

def test_macro(self, thy_name, macro, *, ctxt=None, assms=None, res=None, args="", failed=None):
    thy = basic.load_theory(thy_name)
    ctxt = {'vars': dict((nm, parser.parse_type(thy, s))
                    for nm, s in ctxt.items()) if ctxt is not None else {}}
    macro = global_macros[macro]
    assms = [parser.parse_term(thy, ctxt, assm)
             for assm in assms] if assms is not None else []
    prev_ths = [Thm([assm], assm) for assm in assms]
    prevs = [ProofTerm.assume(assm) for assm in assms]
    args = parser.parse_args(thy, ctxt, macro.sig, args)

    if failed is not None:
        self.assertRaises(failed, macro.eval, thy, args, prev_ths)
        self.assertRaises(failed, macro.get_proof_term, thy, args, prevs)
        return

    res = parser.parse_term(thy, ctxt, res)

    # Check the eval function
    self.assertEqual(macro.eval(thy, args, prev_ths), Thm(assms, res))

    # Check the proof term
    pt = macro.get_proof_term(thy, args, prevs)
    prf = pt.export()
    self.assertEqual(thy.check_proof(prf), Thm(assms, res))

class LogicTest(unittest.TestCase):
    def testConj(self):
        test_data = [
            ([], logic.true),
            ([a], a),
            ([a, b], logic.conj(a, b)),
            ([a, b, a], logic.conj(a, logic.conj(b, a)))
        ]

        for ts, res in test_data:
            self.assertEqual(logic.mk_conj(*ts), res)

    def testConjFail(self):
        self.assertRaises(AssertionError, logic.mk_conj, [a])

    def testStripConj(self):
        test_data = [
            (a, [a]),
            (logic.mk_conj(a, b, a), [a, b, a])
        ]

        for t, res in test_data:
            self.assertEqual(logic.strip_conj(t), res)

    def testDisj(self):
        test_data = [
            ([], logic.false),
            ([a], a),
            ([a, b], logic.disj(a, b)),
            ([a, b, a], logic.disj(a, logic.disj(b, a)))
        ]

        for ts, res in test_data:
            self.assertEqual(logic.mk_disj(*ts), res)

    def testDisjFail(self):
        self.assertRaises(AssertionError, logic.mk_disj, [a])

    def testStripDisj(self):
        test_data = [
            (a, [a]),
            (logic.mk_disj(a, b, a), [a, b, a])
        ]

        for t, res in test_data:
            self.assertEqual(logic.strip_disj(t), res)

    def testGetForallName(self):
        test_data = [
            (Term.mk_all(x, Term.mk_all(y, Term.mk_equals(x, y))), ["x", "y"]),
        ]

        for t, res in test_data:
            self.assertEqual(logic.get_forall_names(t, []), res)

    def testNormBoolExpr(self):
        test_data = [
            ("true", "true"),
            ("false", "false"),
            ("~true", "false"),
            ("~false", "true"),
        ]

        for t, t_res in test_data:
            test_conv(self, 'logic', logic.norm_bool_expr(), t=t, t_res=t_res)

    def testNormConjAssoc(self):
        test_data = [
            ("A", "A"),
            ("A & B", "A & B"),
            ("(A & B) & (C & D)", "A & B & C & D"),
            ("((A & B) & C) & D", "A & B & C & D"),
        ]

        ctxt = {'A': 'bool', 'B': 'bool'}
        for t, t_res in test_data:
            test_conv(self, 'logic', logic.norm_conj_assoc(), ctxt=ctxt, t=t, t_res=t_res)

    def testBetaNorm(self):
        test_macro(
            self, 'logic_base', "beta_norm",
            ctxt={'f': "'a => 'b", 'x': "'a", 'y': "'b"},
            assms=["(%x. f x) x = y"],
            res="f x = y")

    def testApplyTheorem(self):
        test_macro(
            self, 'logic_base', "apply_theorem",
            ctxt={'A': 'bool', 'B': 'bool'},
            assms=["A & B"],
            args="conjD1",
            res="A"
        )

    def testIntro(self):
        thy = basic.load_theory('logic_base')
        macro = logic.intros_macro()

        Ta = TVar('a')
        x = Var('x', Ta)
        P = Var('P', TFun(Ta, boolT))
        C = Var('C', boolT)
        ex_P = logic.mk_exists(x, P(x))
        pt1 = ProofTerm.assume(ex_P)
        pt2 = ProofTerm.variable('x', Ta)
        pt3 = ProofTerm.assume(P(x))
        pt4 = ProofTerm.sorry(Thm([P(x)], C))
        pt4 = ProofTermDeriv('intros', thy, args=[ex_P], prevs=[pt1, pt2, pt3, pt4])
        prf = pt4.export()
        self.assertEqual(thy.check_proof(prf), Thm([ex_P], C))

    def testRewriteGoal(self):
        test_macro(
            self, 'nat', "rewrite_goal",
            args=("nat_plus_def_1, (0::nat) + 0 = 0"),
            res="(0::nat) + 0 = 0"
        )

    def testTrivialMacro(self):
        test_macro(
            self, 'logic_base', 'trivial',
            args='A --> B --> A',
            res='A --> B --> A'
        )

        test_macro(
            self, 'logic_base', 'trivial',
            args='A --> A --> B --> A',
            res='A --> A --> B --> A'
        )

    def testApplyFactMacro(self):
        test_macro(
            self, 'logic_base', 'apply_fact',
            ctxt={"P": "'a => bool", "Q": "'a => bool", "s": "'a"},
            assms=["!s. P s --> Q s", "P s"],
            res="Q s"
        )

    def testImpConjMacro(self):
        test_data = [
            ('A & (D & B) & C --> (A & D & C) & (A & B)', True),
            ('C & D --> A', False),
            ('A & B --> A & B & C', False),
            ('A & B & C --> A & B', True)
        ]

        ctxt = {'A': 'bool', 'B': 'bool', 'C': 'bool', 'D': 'bool'}
        for t, success in test_data:
            if success:
                test_macro(self, 'logic_base', 'imp_conj', ctxt=ctxt, args=t, res=t)
            else:
                test_macro(self, 'logic_base', 'imp_conj', ctxt=ctxt, args=t, failed=AssertionError)


if __name__ == "__main__":
    unittest.main()
