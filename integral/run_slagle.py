import unittest
import json
import cProfile
from integral import slagle
from integral import parser
from integral import rules
from integral import proof
import time

from logic import basic
from integral import proof
from integral import rules


test_cases = {
    "tongji7": [
        ("INT x:[2,3]. 2 * x + x ^ 2" , "34/3"),
        ("INT x:[0,1]. (3 * x + 1) ^ (-2)" , "1/4"),
        ("INT x:[0,1]. exp(6 * x)" , "-1/6 + 1/6 * exp(6)"),
        ("INT x:[-1,2]. x * exp(x)" , "2 * exp(-1) + exp(2)"),
        ("INT x:[0,pi/4]. sin(x)" , "1 + -1/2 * sqrt(2)"),
        ("INT x:[0,1]. 3*x^2 - x + 1" , "3/2"),
        ("INT x:[1,2]. x^2 + 1/x^4" , "21/8"),
        ("INT x:[pi/3, pi]. sin(2*x + pi/3)" , "-3/4"),
        ("INT x:[4, 9]. x ^ (1 / 3) * (x ^ (1 / 2) + 1)" , "-81/11 * 2 ^ (2/3) + 945/44 * 3 ^ (2/3)"),
        # ("INT x:[-1, 0]. (3 * x ^ 4 + 3 * x ^ 2 + 1) / (x ^ 2 + 1)" , "1 + 1/4 * pi"),
        # ("INT x:[4, exp(1) + 3]. (x ^ 3 - 12 * x ^ 2 - 42) / (x - 3)" , "-461/6 + -45 * exp(1) + -3/2 * exp(2) + 1/3 * exp(3)"),
        ("INT x:[0, pi / 2]. sin(x) * cos(x) ^ 3" , "1/4"),
        # ("INT x:[0, pi]. (1 - sin(x)^3)" , "-1/8 * sqrt(3) + 1/6 * pi"),
        ("INT x:[pi/6, pi/2]. cos(x) ^ 2" , "1/4 * pi"),
        ("INT x:[0, 1]. (1 - x^2) ^ (1/2)" , "1/2 * pi"),
        ("INT x:[0, sqrt(2)]. sqrt(2 - x^2)" , "2 * sqrt(2) + sqrt(2) * pi"),
        # "INT y,[-sqrt(2), sqrt(2)]. sqrt(8 - 2*y^2)" , "1 + -1/4 * pi", # auto.py line 161
        # ("INT x:[1/sqrt(2), 1]. sqrt(1 - x^2) / x ^ 2" , "1/6"), # hard
        # ("INT x:[-1, 1]. x / sqrt(5 - 4 * x)" , "2 + 2 * log(2) + -2 * log(3)"), # partial fraction
        # ("INT x:[1,4]. 1 / (1 + sqrt(x))" , "1 + -2 * log(2)"),
        # ("INT x:[3/4, 1]. 1 / (sqrt(1-x) - 1)" , "1 + -exp(-1/2)"),
        ("INT t:[0, 1]. t * exp(-t ^ 2 / 2)" , "-2 + 2 * sqrt(3)"),
        ("INT x:[1, exp(2)]. 1 / (x*sqrt(1+log(x)))" , "1 + -2 * exp(-1)"),
        # ("INT x:[-2, 0]. (x + 2)/(x^2 + 2*x + 2)" , "1/4 + 1/4 * exp(2)"), # timeout
        # ("INT x:[-pi/2,pi/2]. cos(x)^4" , "-4 + 8 * log(2)"), # timeout
        # ("INT x:[-pi/2, pi/2]. sqrt(cos(x) - cos(x)^3)" , "-1/2 + 1/4 * pi"),
        # ("INT x:[0, pi]. sqrt(1 + cos(2*x))" , "-1/4 * pi + 1/6 * pi ^ 3"),
        ("INT x:[0, 1].x * exp(-x)" , "-(2 * exp(-1)) + 1"),
        ("INT x:[1, exp(1)]. x * log(x)", "exp(2) / 4 + 1/4"),
        # ("INT x:[pi/4, pi/3]. x / sin(x)^2", ""),
        ("INT x:[1, 4]. log(x) / sqrt(x)", "8 * log(2) - 4"),
        # ("INT x:[0, 1]. x * atan(x)", ""),
        # ("INT x:[0, pi/2]. exp(2*x)*cos(x)", ""),
        # ("INT x:[0,pi]. (x * sin(x))^2", ""),
        # ("INT x:[1, exp(1)]. sin(log(x))", ""),
        ("INT x:[1/exp(1), exp(1)]. abs(log(x))", ""),
        ("INT x:[0, pi/2]. 1/(1 + cos(x))", "")
    ],

    "MIT/2013": {
        "Exercise 1" : "-2 * exp(1) * log(2) + 2 * log(2)",
        "Exercise 2" : "-2 + exp(1) + exp(3)",
        "Exercise 4" : "2500",
        "Exercise 5" : "sqrt(3) * pi",
        "Exercise 6" : "18 + cos(-3) + -cos(3)",
        # "Exercise 8" : "5/2",
        "Exercise 9" : "-1",
        "Exercise 10" : "-log(-1 + exp(1)) + log(-1 + exp(2))",
        "Exercise 11" : "1/8 * pi",
        # "Exercise 12" : "4", # sympy solveset bug,
        # "Exercise 13" : "1 + (-1/4) * pi", # auto.py line 302
        "Exercise 15" : "24 + -8 * exp(1)",
        "Exercise 17" : "exp(1)",
        "Exercise 18" : "1/2 + (-1/2) * log(2)",
        "Exercise 19" : "1/4 * pi",
        "Exercise 21" : "(1/2) * log(2) + (-1/4) * log(3)",
        "Exercise 22" : "3/2 + (1/3) * sqrt(3) * pi",
        # "Exercise 23" : "418/35", # 
    },
    
    "MIT/2014": {
        "Exercise 1": "2",
        "Exercise 5" : "-4 + 2 * exp(1)",
        # "Exercise 7" : "4 * sqrt(3) + 2/3 * pi", # inequality.py line 256
    },

    # "MIT/2019": {
    #     # "Exercise 2": "log(abs(exp(1) + sin(1)))",
    # },

    "MIT/2020": {
        "Exercise 3": "1/4 + 1/4 * exp(2)",
        # "Exercise 5": "1/2 * pi" # timeout
    },

    "UCDAVIS/usubstitution": {
        # "Exercise 1" : "209952",
        "Exercise 2" : "175099/11",
        "Exercise 3" : "74/21",
        "Exercise 4" : "-1/3 + 1/3 * 2 ^ (3/4)",
        "Exercise 5" : "-1/5 * exp(2) + 1/5 * exp(7)",
        "Exercise 6" : "4/3",
        "Exercise 7" : "0",
        "Exercise 10" : "3 * log(2)",
        "Exercise 11" : "1/5 + -1/5 * exp(-1)",
        "Exercise 12" : "1",
        "Exercise 13" : "-11/21",
        "Exercise 14" : "128/15",
        "Exercise 15" : "1/2 + -7/4 * log(3) + 7/4 * log(5)",
        "Exercise 16" : "-3/2 + -8 * log(2) + 8 * log(3)",
        # "Exercise 17" : "41/6", # ********
        # "Exercise 18" : "188/15", # auto.py line 302
    },
    
    "UCDAVIS/Exponentials": {
        "Exercise 1" : "-5 + 5 * exp(1)",
        "Exercise 2" : "5 + -3 * exp(1)",
        "Exercise 3" : "2",
        "Exercise 5" : "-149/98 + 3/2 * exp(2) + 1/49 * exp(7)",
        "Exercise 6" : "-(243/10) + 1/10 * (1 + 2 * exp(1)) ^ 5",
        "Exercise 7" : "-2 + -1/8 * exp(-8) + 1/8 * exp(8)",
        "Exercise 8" : "-2/3 + exp(1) + -1/3 * exp(3)",
        # "Exercise 10" : "130465/28 + -3645/4 * exp(-8) + -12150/7 * exp(-7) + -1350 * exp(-6) + -540 * exp(-5) + -225/2 * exp(-4) + -10 * exp(-3)",
    },

    "UCDAVIS/Trigonometric": {
        "Exercise 1" : "1/3",
        "Exercise 2" : "1/10 * log(2)", # auto.py line 302 # slagle HeuristicTrigonometricSubstitution
        "Exercise 3" : "5/4",
        "Exercise 4" : "1 + 1/2 * pi",
        "Exercise 5" : "3/4 * pi + 3/20 * sin(-pi)",
        # "Exercise 6" : "1 + 3/4 * pi + -2 * log(1/2)", # auto.py line 302
        # "Exercise 7" : "2/3 + -(0 ^ (1/2)) + 1/3 * 0 ^ (3/2)", # auto.py line 161
        "Exercise 8" : "2/5 * log(2) + -(1/5) * log(3)",
        # "Exercise 12" : "-16/3 + 4 * sqrt(2) * sqrt(3)", # inequality.py line 799
        # "Exercise 13" : "-1/4 + -1/2 * log(1/2)", # auto.py line 302
        "Exercise 15" : "-exp(4) + exp(5)",
        "Exercise 16" : "1/3 * cos(-1) + -1/3 * cos(1)",
        # "Exercise 17" : "1/2 * log(1/2) * log(2) + 1/8 * log(2) ^ 2", # proof.py line 518
        # "Exercise 19" : "-sin(exp(log(1/6) + log(pi))) + sin(exp(log(1/4) + log(pi)))", # proof.py line 397
        "Exercise 20" : "-1/9",
        "Exercise 21" : "-2 + 1/4 * pi ^ 2",
        "Exercise 22" : "1 + 1/2 * sqrt(2) * exp(1/2 * sqrt(2)) + -exp(1/2 * sqrt(2))",
        # "Exercise 25" : "-2 + 2 * sqrt(1 + sqrt(2))", # can't solve
        
        # Exercise 9 Can't Solve.
        # Exercise 10 Timeout!
        # Exercise 11 Timeout!
        # Exercise 14 Timeout!
        # Exercise 18 Timeout!
        # Exercise 23 Timeout!
        # Exercise 24 Timeout!
        # Exercise 26 Timeout!
        # Exercise 27 Timeout!
    },

    "UCDAVIS/Byparts": {
        "Exercise 1" : "1",
        "Exercise 2" : "1",
        "Exercise 3" : "1/4 + 1/4 * exp(2)",
        "Exercise 4" : "-1/9 + -1/18 * sqrt(2) + 1/24 * sqrt(2) * pi",
        "Exercise 5" : "1/16 + -5/16 * exp(-4)",
        "Exercise 7" : "1",
        "Exercise 8" : "-1 + 1/2 * pi",
        "Exercise 9" : "-5/27 * exp(3) + 26/27 * exp(6)",
        "Exercise 10" : "1/16 + 3/16 * exp(4) + 1/4 * exp(4) * log(5) + -1/4 * log(5)",
        "Exercise 11" : "-2 + exp(1)",
        "Exercise 12" : "6 + -2 * exp(1)",
        "Exercise 13" : "-8/5",
        # "Exercise 14" : "1/8", # proof.py line 397
        "Exercise 15" : "2 + -5 * exp(-1)",
        "Exercise 16" : "1/3",
        "Exercise 17" : "-1/2 + 1/32 * pi ^ 2 * sin(1/16 * pi ^ 2) + 1/2 * cos(1/16 * pi ^ 2)",
        "Exercise 18" : "-8/135 * sqrt(2) + 5/27 * sqrt(5)",
        # "Exercise 19" : "-5/36 + -1/2 * log(1/9) + 1/2 * log(1/6)", # proof.py line 397
        "Exercise 20" : "1/3 * cos(1) + -1/3 * exp(3) * cos(exp(3)) + -1/3 * sin(1) + 1/3 * sin(exp(3))", # inequality.py line 875

        # Exercise 6 Can't Solve.
        # Exercise 21 Timeout!
        # Exercise 22 Timeout!
        # Exercise 23 Timeout!
    },

    "UCDAVIS/LogAndArctangent": {
        "Exercise 1" : "3/2 + 3 * log(2)",
        "Exercise 2" : "7 * log(2) + 7 * log(3) + -7 * log(5)",
        "Exercise 4" : "log(2) + -(1/2) * log(5)",
        "Exercise 5" : "26/3 + -8 * log(3)", # not solved
        # "Exercise 6" : "-4/3 + (1/2)*pi", # split region
        "Exercise 7" : " 2 * log(2) + -log(3)", # not solved ** 
        "Exercise 8" : "-1 + 20 * log(2) + -10 * log(3)", # not solved **
        "Exercise 9" : "-atan(2) + atan(3)",
        "Exercise 12" : "1/2 * log(2)",
        "Exercise 13" : "1/4 * pi",   # **
        "Exercise 14" : "-(log(2)) + 1/2 * log (2 + 2 * exp(2))",
        "Exercise 15" : "-1/4 * pi + atan(exp(1))", # **
        "Exercise 16" : "-1/2 + 1/2 * exp(2) + log(2) + -1/2 * log (2 + 2 * exp(2))",
        # "Exercise 17" : "-1/2 + log(2)", # proof.py line 397
        "Exercise 20" : "1 + 3/2 * log(2) + -1/2 * log(4 + 4 * exp(2))",
        # "Exercise 21" : "-1 + -1/4 * pi + exp(1) + atan(exp(-1))",

        # Exercise 3 Timeout!
        # Exercise 6 Timeout!
        # Exercise 10 Timeout!
        # Exercise 11 Timeout!
        # Exercise 18 Timeout!
        # Exercise 19 Timeout!
        # Exercise 22 Timeout!
    },

    "UCDAVIS/PartialFraction": {
        "Exercise 1" : "-(1/4) * log(3) + 1/4 * log(5)",
        "Exercise 2" : "-(5/2) * log(2) + 1/2 * log(5)",
        "Exercise 3" : "9/5 * log(2) + 7/5 * log(3) + -(7/5) * log(7)",
        "Exercise 4" : "1 + -(15/4) * log(3) + 15/8 * log(5)",
        "Exercise 5" : "46 / 3 + -(17/3) * log(2) + -3 * log(3) + 13/3 * log(5)",
        "Exercise 6" : "log(2) + log(3) + -(1/2) * log(5)",
        "Exercise 7" : "7/8 + -(5/2) * log(2) + 5/4 * log(3)",
        "Exercise 8" : "127/64 + -(15/4) * log(2) + 31/8 * log(3)",
        # "Exercise 9" : "-3/8 + log(2)",  # proof 430
        # "Exercise 10" : "-1/8 + -log(12) + 4/3 * log(15) + -3/4 * log(16) + 5/12 * log(24)",  # proof 430
        # "Exercise 11" : "53/90 + -20/27 * log(27) + 20/27 * log(54) + -7/27 * log(108) + 7/27 * log(135)",
        # "Exercise 13" : "1 + -16/3 * log(3) + 9/2 * log(4) + 5/6 * log(6) + -7/6 * log(24) + 7/6 * log(30)",
        # "Exercise 14" : "-1/4 * log(abs(-4 + 4 * exp(1))) + 1/4 * log(abs(-4 + 4 * exp(2))) + 1/4 * log(abs(12 + 4 * exp(1))) + -1/4 * log(abs(12 + 4 * exp(2)))",
        # "Exercise 15" : "log(2) + log(abs(exp(1))) + -log(abs(1 + exp(1)))",
        # "Exercise 16" : "1/4 * pi + -atan(2) + 9/2 * log(2) + -3/2 * log(5)",
        # Exercise 12 Timeout!
        # Exercise 17 Timeout!
        # Exercise 18 Timeout!
        # Exercise 19 Timeout!
        # Exercise 20 Timeout!
    }
}

file_names = [
    "tongji7",
    # "MIT/2013",
    # "MIT/2014",
    # "MIT/2020",
    # "UCDAVIS/usubstitution",
    # "UCDAVIS/Exponentials",
    # "UCDAVIS/Trigonometric",
    # "UCDAVIS/Byparts",
    # "UCDAVIS/LogAndArctangent",
    # "UCDAVIS/PartialFraction",
]

class RunSlagle(unittest.TestCase):
    def testRunSlagle(self):
        for file in file_names:
            for problem, answer in test_cases[file]:
                s = slagle.Slagle(problem)
                try:
                    steps = s.export_step()
                    assert steps
                    f = slagle.export_calc(problem, steps, "tongji", "tongji7")
                    print(f)
                except (AssertionError, TimeoutError) as e:
                    print(e)
                    continue








if __name__ == "__main__":
    import sys, getopt
    opts, args = getopt.getopt(sys.argv[1:], 'pf:c:')

    profile = False
    test_file = None
    test_case = None
    for opt, arg in opts:
        if opt == '-p':
            profile = True
        if opt == '-f':
            test_file = arg
        if opt == '-c':
            test_case = arg

    sys.argv = sys.argv[:1]  # remove own arguments for unittest
    unittest.main()