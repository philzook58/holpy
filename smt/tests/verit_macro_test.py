import unittest

from logic.tests.logic_test import test_macro
from smt.veriT.verit_macro import *


class VeriTMacroTest(unittest.TestCase):
    def test_not_symm(self):
        test_macro(
            self, 'verit', "verit_not_symm",
            vars={'x': 'nat', 'y': 'nat'},
            assms=["~(x = y)"],
            args="~(y = x)",
            res="~(y = x)"
        )


if __name__ == "__main__":
    unittest.main()
