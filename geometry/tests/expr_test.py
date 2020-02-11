"""Unit test for expressions."""

import copy
import unittest

from geometry import expr
from geometry.expr import Fact, Rule, Line
from geometry import parser
from geometry.ruleset import ruleset, ruleset_reduced

from pstats import Stats
import cProfile

class ExprTest(unittest.TestCase):
    def testPrintExpr(self):
        test_data = [
            (Fact("coll", ["A", "B", "C"]), "coll(A,B,C)"),
        ]

        for f, s in test_data:
            self.assertEqual(str(f), s)

    def testPrintRule(self):
        test_data = [
            (Rule([Fact("coll", ["A", "B", "C"])], Fact("coll", ["A", "C", "B"])),
             "coll(A,C,B) :- coll(A,B,C)"),
        ]

        for r, s in test_data:
            self.assertEqual(str(r), s)

    def testGetArgTypeByFact(self):
        test_data = [
            ("coll(A, B, C)", expr.POINT),
            ("cong(A, B, C, D)", expr.SEG),
            ("para(A, B, C, D)", expr.PonL),
        ]
        for fact, r in test_data:
            self.assertEqual(parser.parse_fact(fact).get_arg_type(), r)

    def testGetCircle(self):
        test_data = [
            (["circle(None, A, B, C, D)"], ["A", "B", "C", "E"], None, "circle(None, A, B, C, D)"),
            (["circle(O, A, B, C, D)"], ["A", "B", "C", "E"], "O", "circle(O, A, B, C, D)"),
            (["circle(O, A, B, C, D)"], ["E", "F", "G", "H"], "P", "circle(P, E, F, G, H)"),
        ]
        for circles, points, center, concl in test_data:
            circles = [parser.parse_circle(circle) for circle in circles]
            concl = parser.parse_circle(concl)
            new_circle = expr.Prover(ruleset, circles=circles).get_circle(points, center=center)
            self.assertEqual({new_circle}, {concl})

    def testCombineCircles(self):
        test_data = [
            ("circle(O, A, B, C, D)", "circle(O, B, C, D, E, F)", "circle(O, A, B, C, D, E, F)"),
            ("circle(None, A, B, C)", "circle(O, A, B, C)", "circle(O, A, B, C)"),
        ]

        for this_circle, other_circle, combined in test_data:
            this_circle = parser.parse_circle(this_circle)
            other_circle = parser.parse_circle(other_circle)
            combined = parser.parse_circle(combined)
            this_circle.combine(other_circle)
            self.assertEqual(this_circle.center, combined.center)
            self.assertEqual(this_circle.args, combined.args)

    def testMatchFact(self):
        test_data = [
            ("coll(A,B,C)", "coll(P,Q,R)", {}, [{"A": "P", "B": "Q", "C": "R"}]),
            ("coll(A,B,C)", "coll(P,Q,R)", {"A": "P"}, [{"A": "P", "B": "Q", "C": "R"}]),
            ("coll(A,B,C)", "coll(P,Q,R)", {"A": "Q"}, []),
            ("coll(A,B,C)", "para(P,Q,R,S)", {}, []),
            ("coll(A,B,C,D,E,F)", "coll(P,Q,R,S,T,U)", {}, [{"A": "P", "B": "Q", "C": "R", "D": "S",
                                                             "E": "T", "F": "U"}]),
            ("cong(A, B, A, D)", "cong(P, Q, R, S)", {}, []),
            ("cong(A, B, C, D)", "cong(P, Q, P, S)", {}, [
                {"A": "P", "B": "Q", "C": "P", "D": "S"},
                {"A": "P", "B": "Q", "C": "S", "D": "P"},
                {"A": "Q", "B": "P", "C": "P", "D": "S"},
                {"A": "Q", "B": "P", "C": "S", "D": "P"}]),
            ("coll(A, B, A, D)", "coll(P, Q, P, S)", {}, [{"A": "P", "B": "Q", "D": "S"}]),
            ("cong(A, B, A, D)", "cong(P, Q, P, S)", {}, [{"A": "P", "B": "Q", "D": "S"}]),
            ("coll(A, B, C)", "coll(P, Q, R, T)", {"A": "Q", "B": "R"}, [{"A": "Q", "B": "R", "C": "T"}]),
            ("coll(A, B, C)", "coll(P, Q, R, T)", {"A": "P", "B": "Q"},
             [{"A": "P", "B": "Q", "C": "R"}, {"A": "P", "B": "Q", "C": "T"}]),
            ("contri(A, B, C, D, E, F)", "contri(P, Q, R, X, Y, Z)", {},
             [{"A": "P", "B": "Q", "C": "R", "D": "X", "E": "Y", "F": "Z"},
             {"A": "P", "B": "R", "C": "Q", "D": "X", "E": "Z", "F": "Y"},
             {"A": "Q", "B": "R", "C": "P", "D": "Y", "E": "Z", "F": "X"}]
             ),
        ]

        for pat, f, inst, res in test_data:
            pat = parser.parse_fact(pat)
            f = parser.parse_fact(f)
            insts = expr.Prover(ruleset).match_expr(pat, f, inst)
            insts = [p[0] for p in insts]
            self.assertEqual(insts, res)

    def testMatchFactLines(self):
        test_data = [
            ("perp(l, m)", "perp(P, Q, R, S)", {}, ["line(O, P, Q)"],
             [{"l": ("P", "Q"), "m": ("R", "S")}, {"l": ("R", "S"), "m": ("P", "Q")}]),
            ("perp(l, m)", "perp(P, Q, R, S)", {"l": ("Q", "P")}, ["line(O, P, Q)"],
             [{"l": ("Q", "P"), "m": ("R", "S")}]),
            ("perp(l, m)", "perp(P, Q, R, S)", {"l": ("Q", "P")}, [], [{"l": ("Q", "P"), "m": ("R", "S")}]),
            ("perp(l, m)", "perp(P, Q, R, S)", {"l": ("A", "P")}, ["line(O, P, Q)"], []),
            ("para(p, q)", "para(E, N, C, D)", {}, [],
             [{"p": ("E", "N"), "q": ("C", "D")}, {"p": ("C", "D"), "q": ("E", "N")}]),

            ("para(A, B, C, D)", "para(P, Q, R, S)", {'A': 'M', 'B': 'N'}, ["line(E, F, G, H)"], []),

            ("cong(A, B, C, D)", "cong(P, Q, R, S)", {}, [], [{"A": "P", "B": "Q", "C": "R", "D": "S"},
                                                              {"A": "P", "B": "Q", "C": "S", "D": "R"},
                                                              {"A": "Q", "B": "P", "C": "R", "D": "S"},
                                                              {"A": "Q", "B": "P", "C": "S", "D": "R"},]),

            ("perp(B, A, C, A)", "perp(P, Q, P, R)", {}, [], [{"A": "P", "B": "Q", "C": "R"}]),
            #
            ("cong(E, A, E, B)", "cong(A, Q, B, Q)", {"A": "A", "B": "B", "D": "P"}, [],
             [{"A": "A", "B": "B", "D": "P", "E": "Q"}]),
            ("perp(m, n)", "perp(A, C, B, E)", {"m": ("A", "C"), "l": ("B", "E")}, [],
             [{"l": ("B", "E"), "m": ("A", "C"), "n": ("B", "E")}]),
            ("eqangle(C, A, C, B, R, P, R, Q)", "eqangle(C, F, C, E, H, F, H, E)", {}, ["line(C, F, G)"],
             [{"A": "F", "B": "E", "C": "C", "P": "F", "Q": "E", "R": "H"}])
        ]

        for pat, f, inst, lines, res in test_data:
            pat = parser.parse_fact(pat)
            f = parser.parse_fact(f)
            lines = [parser.parse_line(line) for line in lines]
            insts = expr.Prover(ruleset, lines=lines).match_expr(pat, f, inst)
            insts = [p[0] for p in insts]
            self.assertEqual(insts, res)

    def testMatchFactCircles(self):
        test_data = [
            # ("cyclic(A, B, C, D)", "cyclic(P, Q, R, S)", {}, [], [{"A": "P", "B": "Q", "C": "R", "D": "S"}]),
            # ("cyclic(A, B, C, D)", "cyclic(P, Q, R, S)", {"A": "T"}, [], []),
            # ("cyclic(A, B, C, D)", "cyclic(P, Q, R, S)", {"A": "R"}, ["circle(None, P, Q, R, S)"],
            #  [{"A": "R", "B": "P", "C": "Q", "D": "S"}]),
            # ("cyclic(A, B, C, D)", "cyclic(P, Q, R, S)", {"A": "R", "B": "Q"}, ["circle(None, P, Q, R, S)"],
            #  [{"A": "R", "B": "Q", "C": "P", "D": "S"}]),
            # ("circle(A, B, C, D)", "circle(P, Q, R, S)", {}, [], [{"A": "P", "B": "Q", "C": "R", "D": "S"}]),
            # ("circle(A, B, C, D)", "circle(P, Q, R, S)", {"A": "P"}, [], [{"A": "P", "B": "Q", "C": "R", "D": "S"}]),
        ]

        for pat, f, inst, circles, res in test_data:
            pat = parser.parse_fact(pat)
            f = parser.parse_fact(f)
            circles = [parser.parse_circle(circle) for circle in circles]
            insts = expr.match_expr(pat, f, inst, circles=circles)
            self.assertEqual(insts, res)

    def testApplyRule(self):
        test_data = [
            # ("D44", ["midp(P, E, F)", "midp(Q, E, G)"], ["line(E, F)", "line(G, E)"], [], ["para(P, Q, F, G)"]),
            # ("D56", ["cong(D, A, D, B)", "cong(E, A, E, B)"],  [], [], ["perp(A, B, D, E)"]),
            # (ruleset["D5"], ["para(E, F, G, H)"], ["line(E, F)", "line(G, H)"], [],
            #  ["para(G, H, E, F)"]),
            # (ruleset["D44"], ["midp(P, E, F)", "midp(Q, E, G)"], ["line(E, F)", "line(G, E)"], [],
            #  ["para(P, Q, F, G)"]),
            # (ruleset["D45"], ["midp(N, B, D)", "para(E, N, C, D)", "coll(E, B, C)"],
            #  ["line(M, N, E)", "line(C, D)", "line(D, N, B)", "line(C, E, B)"], [],
            #  ["midp(E, B, C)"]),
            # (ruleset["D56"], ["cong(D, A, D, B)", "cong(E, A, E, B)"],
            #  [], [], ["perp(A, B, D, E)"]),
            # (ruleset["D13"], ["cong(E, P, P, F)", "cong(P, E, G, P)", "cong(P, E, H, P)"], [], [],
            #  ["cyclic(E, F, G, H)"]),
            # (ruleset["D43"], ["eqangle(E, F, E, G, C, D, C, B)", "cyclic(E, D, G, B, F, C)"],
            #  ["line(E, F)", "line(E, G)", "line(D, C)", "line(D, B)", "line(F, G)", "line(C, B)"],
            #  ["circle(None, E, D, G, B, F, C)"], ["cong(F, G, D, B)"]),
            # (ruleset["D42"], ["eqangle(A, F, B, C, A, C, B, E)"],
            #  ["line(E, A, C)", "line(F, B, C)", "line(H, A, F)", "line(H, B, E)", "line(G, A, B)", "line(G, C, H)"],
            #  [], ["cyclic(A, B, F, E)"]),
            # (ruleset["D9"], ["perp(B, E, A, C)", "perp(A, C, B, E)"], [], [], []),
            # ("D9", ["perp(G, F, D, E)", "perp(A, B, D, E)"], [], [], ["para(G, F, A, B)"]),
            # ("D9", ["perp(A, B, D, E)", "perp(D, E, G, F)"], [], [], ["para(A, B, G, F)"]),
            # (ruleset["D43"], ["eqangle(B, E, A, C, B, C, A, F)", "cyclic(B, A, E, F)"],
            #  ["line(E, A, C)", "line(F, B, C)", "line(H, A, F)", "line(H, B, E)", "line(G, A, B)", "line(G, C, H)"], [],
            #  []),
            # (ruleset["D42"], ["eqangle(B, E, A, C, A, F, B, C)"],
            #  ["line(E, A, C)", "line(F, B, C)", "line(H, A, F)", "line(H, B, E)", "line(G, A, B)", "line(G, C, H)"],
            #  [], ["cyclic(H, C, E, F)"]),
            # (ruleset["D76"], ["perp(B, E, A, C)", "perp(A, F, B, C)"],
            #  ["line(E, A, C)", "line(F, B, C)", "line(H, A, F)", "line(H, B, E)", "line(G, A, B)", "line(G, C, H)"], [],
            #  ["eqangle(B,E,A,C,A,F,B,C,B,C,A,F,A,C,B,E)"]),
            # (ruleset["D42"], ["eqangle(B, E, A, C, A, F, B, C, A, C, B, E, B, C, A, F)"],
            #   ["line(E, A, C)", "line(F, B, C)", "line(H, A, F)", "line(H, B, E)", "line(G, A, B)", "line(G, C, H)"],
            #   [], ["cyclic(E, C, F, H)", "cyclic(E, A, F, B)"]),
            # ("D61", ["simtri(B, A, D, C, A, D)", "cong(A, B, A, C)"], [], [], ["contri(B, A, D, C, A, D)"]),
            # ("D89", ["eqangle(A, F, A, B, A, E, D, E)", "eqangle(B, F, A, E, A, D, D, E)"], [], [],
            #  ["simtri(A, B, F, E, A, D)"]),
            # ("D58", ["eqangle(A, B, A, F, A, E, E, D)", "eqangle(B, F, F, A, A, D, D, E)"], [], [],
            #  ["simtri(A, B, F, E, A, D)"]),
            # ("D89", ["eqangle(A, F, A, B, D, E, E, A)", "eqangle(B, F, F, A, A, D, D, E)"], [], [],
            #  ["simtri(A, B, F, E, A, D)"]),
            # ("D40", ["para(A, B, D, E)"], ["line(E, A, F)"], [], ["eqangle(B, A, A, F, A, E, E, D)"])

        ]

        for rule, facts, lines, circles, concls in test_data:
            facts = [parser.parse_fact(fact) for fact in facts]
            concls = [parser.parse_fact(concl) for concl in concls]
            lines = [parser.parse_line(line) for line in lines]
            circles = [parser.parse_circle(circle) for circle in circles]
            hyps = copy.copy(facts)
            prover = expr.Prover(ruleset, hyps=facts, lines=lines, circles=circles)
            prover.apply_rule(rule, facts)
            self.assertEqual(set(prover.hyps) - set(hyps), set(concls))

    def testCombineFacts(self):
        test_data = [
            # para
            ("para(A, B, C, D)", "para(E, F, G, H)", [], [], False),
            ("para(A, B, C, D)", "para(E, F, G, H)", ["line(A, B, E, F)"], [], "para(A, B, C, D, G, H)"),
            ("para(A, B, C, D)", "para(C, D, E, F)", [], [], "para(A, B, C, D, E, F)"),
            ("para(A, B, C, D, E, F, G, H)", "para(C, D, P, Q, R, S)", ["line(E, F, R, S)"], [],
             "para(A, B, C, D, E, F, G, H, P, Q)"),

            # coll
            ("coll(A, B, C, D)", "coll(E, F, G, H)", [], [], False),
            ("coll(A, B, C, D)", "coll(A, D, P, Q)", [], [], "coll(D, P, B, C, A, Q)"),

            # eqangle
            ("eqangle(A, B, C, D, E, F, G, H)", "eqangle(P, Q, R, S, W, X, Y, Z)", [], [], False),
            ("eqangle(A, B, C, D, E, F, G, H)", "eqangle(P, Q, R, S, W, X, Y, Z)",
             ["line(A, B, P, Q)", "line(C, D, R, S)"], [], "eqangle(A, B, C, D, E, F, G, H, W, X, Y, Z)"),
            ("eqangle(A, B, C, D, E, F, G, H)", "eqangle(P, Q, R, S, W, X, Y, Z)",
             ["line(A, B, P, Q)"], [], False),
            ("eqangle(B, E, A, C, A, F, B, C)", "eqangle(A, C, B, E, A, F, B, C)", [], [],
             "eqangle(B, E, A, C, A, F, B, C, A, C, B, E)"),
            ("eqangle(A, C, B, E, A, F, B, C)", "eqangle(B, C, A, F, B, E, A, C)",
             ["line(F, B, C)", "line(H, A, F)", "line(H, B, E)", "line(G, A, B)", "line(G, C, H)"],
             [], False),
            ("eqangle(B, E, A, C, A, F, B, C)", "eqangle(A, C, B, E, A, F, B, C)",
             ["line(F, B, C)", "line(H, A, F)", "line(H, B, E)", "line(G, A, B)", "line(G, C, H)"],
             [], "eqangle(B, E, A, C, A, F, B, C, A, C, B, E)"),
            ("eqangle(B, A, B, E, F, A, F, E)", "eqangle(E, A, E, B, F, A, F, B)", [], [], False),

            # circle
            ("circle(O, A, B, C, D)", "circle(O, B, C, D, E)", [], [], "circle(O, A, B, C, D, E)"),

            # cyclic
            ("cyclic(A, B, C, D)", "cyclic(B, C, D, E)", [], [], "cyclic(A, B, C, D, E)"),

            # cong
            ("cong(A, B, C, D)", "cong(B, A, E, F)", [], [], "cong(A, B, C, D, E, F)"),
            ("cong(A, B, C, D)", "cong(P, Q, R, S)", [], [], False),
            ("cong(A, B, C, D, E, F)", "cong(F, E, A, B, P, Q)", [], [], "cong(A, B, C, D, E, F, P, Q)"),

            # perp
            ("perp(B, E, A, C)", "perp(A, C, B, E)", [], [], False),
        ]

        for fact, goal, lines, circles, concl in test_data:
            fact = parser.parse_fact(fact)
            goal = parser.parse_fact(goal)
            lines = [parser.parse_line(line) for line in lines]
            circles = [parser.parse_circle(circle) for circle in circles]
            res = expr.Prover(ruleset, lines=lines, circles=circles).combine_facts(fact, goal)
            if concl:
                concl = parser.parse_fact(concl)
                self.assertEqual(res, concl)
            else:
                self.assertEqual(res, None)

    def testCombineFactsList(self):
        test_data = [

        ]

        for fact, target, lines, circles, concl in test_data:
            fact = parser.parse_fact(fact)
            target = parser.parse_fact(target)
            lines = [parser.parse_line(line) for line in lines]
            circles = [parser.parse_circle(circle) for circle in circles]
            r = expr.combine_facts()
            concl = [parser.parse_fact(fact) for fact in concl]
            self.assertEqual(set(r), set(concl))

    def testApplyRuleHyps(self):
        test_data = [
            # ("D5", ["para(P, Q, R, S)"], [], ["para(P, Q, R, S)"]),

            # ("D45", ["midp(N, B, D)", "para(E, N, C, D)", "coll(E, B, C)"],
            #  ["line(M, N, E)", "line(C, D)", "line(D, N, B)", "line(C, E, B)"],
            #  ["midp(N, B, D)", "para(E, N, C, D)", "coll(E, B, C)", "midp(E, B, C)"]),
        ]

        for rule, hyps, lines, concls in test_data:
            hyps = [parser.parse_fact(fact) for fact in hyps]
            concls = [parser.parse_fact(concl) for concl in concls]
            lines = [parser.parse_line(line) for line in lines]
            expr.apply_rule_hyps(rule, hyps, lines=lines, ruleset=ruleset)
            self.assertEqual(set(hyps), set(concls))

    def testSearchStep(self):
        test_data = [
        ]
        for rules, hyps, lines, concls in test_data:
            hyps = [parser.parse_fact(fact) for fact in hyps]
            lines = [parser.parse_line(line) for line in lines]
            concls = [parser.parse_fact(concl) for concl in concls]
            expr.search_step(rules, hyps, lines=lines)
            self.assertEqual(set(hyps), set(concls))

    def testSearchFixpoint(self):
        test_data = [
        ]
        for rules, hyps, lines, circles, concl in test_data:
            hyps = [parser.parse_fact(fact) for fact in hyps]
            concl = parser.parse_fact(concl)
            lines = [parser.parse_line(line) for line in lines]
            circles = [parser.parse_circle(circle) for circle in circles]
            hyps = expr.search_fixpoint(ruleset, hyps, lines, circles, concl)
            fact = expr.find_goal(hyps, concl, lines, circles)
            self.assertIsNotNone(fact)

    def testPrintSearch(self):
        test_data = [
            # (ruleset, ["cong(D, A, D, B)", "cong(E, A, E, B)", "perp(G, F, D, E)", "coll(A, C, B)", "coll(A, G, E)",
            #            "coll(B, F, E)", "coll(D, C, E)"], [], [], "para(A, C, G, F)"),
            #
            # (ruleset, ["cong(A, B, B, C, C, D, D, A)"], [], [], "eqangle(A, B, B, D, B, D, A, D)"),
            #
            # (ruleset, ["eqangle(E, F, E, G, D, C, B, C)", "cyclic(E, D, G, B, F, C)"], [],
            #  ["circle(None, E, D, G, B, F, C)"], "cong(D, B, F, G)"),
            #
            # (ruleset, ["coll(E, A, C)", "perp(B, E, A, C)", "coll(F, B, C)", "perp(A, F, B, C)", "coll(H, A, F)",
            #            "coll(H, B, E)", "coll(G, A, B)", "coll(G, C, H)"], [], [], "perp(C, G, A, B)"),
            #
            # # Following 2 tests: testing if contri works.
            # (ruleset, ["para(B, E, C, F)", "cong(B, E, C, F)", "coll(B, M, C)", "coll(F, M, E)"],
            #             [], [], "cong(B, M, C, M)"),
            #
            # (ruleset, ["cong(A, B, A, C)", "cong(D, B, D, C)", "coll(A, D, F)"],
            #  [], [], "cong(B, F, C, F)"),
            #
            # # Following 4 tests: testing if simtri works.
            # (ruleset, ["para(D, E, B, F)", "para(E, F, A, B)", "coll(A, D, B)", "coll(B, F, C)", "coll(A, E, C)"], [], [],
            #  "simtri(A, D, E, E, F, C)"),
            #
            # (ruleset, ["para(F, D, A, B)", "para(F, E, A, C)", "coll(B, D, E, C)"], [], [], "simtri(A, B, C, F, D, E)"),
            #
            # (ruleset, ["perp(B, F, A, E)", "coll(A, F, E)", "coll(D, E, C)", "perp(A, B, A, D)", "perp(A, D, D, C)",
            #            "perp(A, B, B, C)", "perp(D, C, C, B)"],
            #  [], [], "simtri(A, B, F, E, A, D)"),
            #
            # (ruleset, ["perp(B, F, A, E)", "coll(A, F, E)", "coll(D, E, C)", "perp(A, B, A, D)", "perp(A, D, D, C)",
            #   "perp(A, B, B, C)", "perp(D, C, C, B)"],
            #  [], [], "simtri(A, B, F, E, A, D)"),
            #
            # # This example shows that the mechanism can be improved:
            # # Machine proof firstly obtain a circle from the eqangle hypothesis, then obtain another eqangle
            # # condition by the circle. But for human we can make use of eqangle(B, A, A, C, B, A, A, C) so that no
            # # circle required.
            # (ruleset, ["coll(A, B, D)", "coll(A, C, E)", "eqangle(C, B, A, C, A, D, D, E)",
            #            ], [], [], "simtri(A, C, B, A, D, E)"),

            # This is the Example 6.4. We are not able to add auxiliary point so far. So I add additional facts
            # in the hypothesis :midp(F, A, D) and coll(F, A, D).
            # A0 -> F
            (ruleset, ["para(A, B, C, D)", "midp(M, A, C)", "midp(F, A, D)", "midp(N, B, D)", "coll(M, N, E)",
                       "coll(A, M, C)", "coll(D, N, B)", "coll(C, E, B)", "coll(F, A, D)"
                       ], [], [], "midp(E, B, C)"),

            # Following tests proves some theorems in "Machine Proofs in Geometry".

            # Example 6.45 If L is the harmonic conjugate of the centroid G of a triangle ABC
            # with respect to the ends A, D of the median AD, show that LD = AD.
            # (ruleset, ["midp(D, B, C)", "midp(E, A, C)", "midp(F, A, B)", "coll(B, G, E)", "coll(A, G, D, L)",
            #            "coll(F, G, C)", "eqratio(L, D, D, G, A, L, A, G)", "coll(B, D, C)"], [], [], "cong(L, D, A, D)"),

            # Example 6.46 Show that the distances of a point on a median of triangle from
            # the sides including the median are inversely proportional to these sides.
            # (ruleset, ["coll(A, K, C)", "coll(C, J, B)", "coll(A, F, B)", "coll(C, N, F)",
            #            "midp(F, A, B)", "perp(K, N, A, C)", "perp(N, J, C, B)"], [], [], "eqratio(N, K, N, J, B, C, A, C)"),

            # Example 6.51
            # K1 -> P, K2 -> Q
            # (ruleset, ["midp(N, A, B)", "midp(M, A, C)", "coll(C, G, N)", "coll(B, G, M)",
            #            "eqratio(B, K, B, C, C, P, B, C)", "coll(N, Q, M)", "coll(A, Q, K)",
            #            "coll(Z, B, C)", "coll(Z, Q, G)"], [], [], "eqratio(B, Z, Z, C, B, P, P, C)"),

            # Example 6.60
            # H1 -> P, H2 -> Q, A1 -> R
            # (ruleset_reduced, ["midp(R, B, C)", "coll(H, B, C)", "para(A, R, H, N)", "coll(D, N, H)", "coll(B, A, D)", "coll(Q, A, P)",
            #            "midp(K, N, C)", "midp(L, B, D)", "midp(K, H, P)", "midp(L, H, Q)"], [], [], "midp(A, P, Q)"),
        ]
        # pr = cProfile.Profile()


        for ruleset_type, hyps, lines, circles, concl in test_data:
            # pr.enable()
            hyps = [parser.parse_fact(fact) for fact in hyps]
            concl = parser.parse_fact(concl)
            lines = [parser.parse_line(line) for line in lines]
            circles = [parser.parse_circle(circle) for circle in circles]
            prover = expr.Prover(ruleset_reduced, hyps, concl, lines, circles)
            print("--- Proof for", concl, "---")
            res = prover.search_fixpoint()
            assert res, "✘ Fixpoint reached without proving goal."
            prover.print_search(res)
            # p = Stats(pr)
            # p.strip_dirs()
            # p.sort_stats('cumtime')
            # p.print_stats()



    def testPrintSearchFailed(self):
        test_data = [
            # Following test shows how we handle rules that contain "not" fact.
            # The goal cannot be reached because "coll(E, F, G, H)" is in hypotheses.
            # The goal can be reached if remove "coll(E, F, G, H)" from hypotheses.
            (ruleset, ["eqangle(E, G, G, F, H, E, F, H)", "coll(E, F, G, H)"], [], [], "cyclic(E, F, G, H)"),
        ]
        for rules, hyps, lines, circles, concl in test_data:
            hyps = [parser.parse_fact(fact) for fact in hyps]
            concl = parser.parse_fact(concl)
            lines = [parser.parse_line(line) for line in lines]
            circles = [parser.parse_circle(circle) for circle in circles]
            prover = expr.Prover(ruleset, hyps, concl, lines, circles)
            print("--- Proof for", concl, "---")
            res = prover.search_fixpoint()
            assert not res, "✘ Goal has been proved."
            print("✔ Fixpoint reached without proving goal. ")

    def testPick(self):
        test_data = [
            ["para(A, B, C)", "perp(D, E, F)", "perp(P, Q, R)", "perp(R, S, T)", "coll(X, Y, Z)", "para(E, F, G, H)"],
        ]
        for hyps in test_data:
            hyps = [parser.parse_fact(fact) for fact in hyps]
            prover = expr.Prover(ruleset, hyps)
            print(prover.get_appliable_facts(ruleset["D10"], hyps))




if __name__ == "__main__":
    unittest.main()
