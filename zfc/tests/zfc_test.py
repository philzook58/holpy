import unittest

from kernel.term import *
from zfc.zfc import *
from zfc import zfc

from syntax.settings import settings
settings.unicode = True

class ZFCTest(unittest.TestCase):
    def testPrint(self):
        test_data = [
            (ForallS("a", V("M"), ForallS("b", V("M"), Eq(V("a"), V("b")))),
             "∀a. a ∈ M ⟶ (∀b. b ∈ M ⟶ a = b)"),

            (subSet(V("a"), V("b")),
             "a ⊆ b"),

            (identityFun(V("S")),
             "identity S"),

            (composeFun(V("f"), V("g")),
             "compose f g")
        ]

        for t, s in test_data:
            self.assertEqual(str(t), s)
        
    def testZFCPrint(self):
        zfc.zfc_theory = ZFCTheory()

        test_data = [
            (ForallS("a", V("M"), ForallS("b", V("M"), Eq(V("a"), V("b")))),
             "∀a:M. ∀b:M. a = b"),

            (subSet(V("a"), V("b")),
             "a ⊆ b"),

            (identityFun(V("S")),
             "identity S"),

            (composeFun(V("f"), V("g")),
             "f ∘ g")
        ]

        for t, s in test_data:
            self.assertEqual(zfc_print(t), s)


if __name__ == "__main__":
    unittest.main()
