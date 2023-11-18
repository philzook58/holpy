import unittest

from kernel.term import *
from zfc import zfc
from zfc.zfc import *
from zfc.zfc_parser import *

from syntax.settings import settings
settings.unicode = True

class ParserTest(unittest.TestCase):
    def testParseTerm(self):
        a, b, f, g, S, T = V("a"), V("b"), V("f"), V("g"), V("S"), V("T")
        S_Monoid = structV("S_Monoid")

        test_data = [
            # Set-theoretic functions
            ("a ∈ S", memSet(a, S)),
            ("S ⊆ T", subSet(S, T)),
            ("identity S", identityFun(S)),
            ("f ∘ g", composeFun(f, g)),

            # Logical operators
            ("a ∈ S ∧ b ∈ S", And(memSet(a, S), memSet(b, S))),
            ("a ∈ S ∨ b ∈ S", Or(memSet(a, S), memSet(b, S))),
            ("a ∈ S ⟶ b ∈ S", Implies(memSet(a, S), memSet(b, S))),
            ("a ∈ S ⟷ b ∈ S", Eq(memSet(a, S), memSet(b, S))),
            ("¬a ∈ S", Not(memSet(a, S))),

            # Quantifiers
            ("∀a:S. ∀b:S. a = b", ForallS("a", S, ForallS("b", S, Eq(a, b)))),
            ("∀a:S. ∃b:S. a = b", ForallS("a", S, ExistsS("b", S, Eq(a, b)))),
            ("THE a:S. a ∈ T", TheS("a", S, memSet(a, T))),
            ("{a : S. a ∈ T}", CollectS("a", S, memSet(a, T))),

            # Function application
            ("f(a)", app(f, a)),
            ("f(a, b)", app2(f, a, b)),

            # Function types
            ("S ⇒ T", funcSet(S, T)),
            ("(S, S) ⇒ T", funcSet2(S, S, T)),

            # Function definition
            ("a:S -> (a * a):S", Func(S, S, Lambda(a, timesOp(S_Monoid, a, a)))),
            ("a:S, b:S -> (a * b):S", Func2(S, S, S, Lambda(a, b, timesOp(S_Monoid, a, b)))),

            # Quantifiers with context change
            ("∀a:S. ∀b:S. a * a = b", ForallS("a", S, ForallS("b", S, Eq(timesOp(S_Monoid, a, a), b)))),
        ]

        zfc_theory = ZFCTheory()
        # We add some simple versions of structures
        zfc_theory.add_structure(Structure(
            name="Monoid", carrier_set="M", args=[], params=[
                ("unit", V("M")),
                ("timesOp", funcSet2(V("M"), V("M"), V("M")))
            ], inherits=[], props=[]))

        for s, expected_t in test_data:
            ctx = ZFCContext(zfc_theory)
            ctx.add_struct(S, StructureProp(S, "Monoid", []))
            t = parse_term(s)
            # print(str(t))
            elaborate_t = elaborate_term(t, ctx)
            # print(str(elaborate_t))
            # print(str(expected_t))
            self.assertEqual(elaborate_t, expected_t)

    def testParseStructure(self):
        """
        Examples from this test case are taken from:

        Ballarin, C. (2020). Exploring the structure of an algebra text with locales.
        J. Autom. Reason. 64(6): 1093-1121.

        """
        zfc.zfc_theory = ZFCTheory()

        ## Monoid

        # Definition of monoid structure over $M$.
        load_zfc_item("""
        struct Monoid (set M) {
            parameters
                const unit : M;
                func timesOp : (M, M) ⇒ M;
            properties
                associative: ∀a:M. ∀b:M. ∀c:M. (a * b) * c = a * (b * c);
                left_unit:   ∀a:M. 1 * a = a;
                right_unit:  ∀a:M. a * 1 = a;
        }
        """)

        # Definition of submonoid: $N$ is a submonoid of $M$.
        load_zfc_item("""
        struct Submonoid (set N) {
            arguments
                M :: Monoid;
            properties
                subset:       N ⊆ M;
                unit_closed:  1 ∈ N;
                times_closed: ∀a:N. ∀b:N. a * b ∈ N;
        }
        """)

        # The first property to show is that a submonoid is itself a monoid.
        load_zfc_item("""
        theorem submonoid_is_monoid:
            fixes
                M :: Monoid;
                N :: Submonoid(M);
            shows N :: Monoid with
                unit: 1;
                timesOp: a:N, b:N -> (a * b):N;
        """)

        # Next, show transitivity of submonoid: if $N$ is a submonoid of $M$, and
        # $K$ is a submonoid of $N$, then $K$ is a submonoid of $M$.
        load_zfc_item("""
        theorem submonoid_transitive:
            fixes
                N :: Submonoid(M);
                K :: Submonoid(N);
            shows K :: Submonoid(M)
        """)

        ## Transformations

        # An application of monoids: given any set $S$, the set of functions
        # from $S$ to $S$ forms a monoid.
        load_zfc_item("""
        theorem transformation_is_monoid:
            fixes
                S :: Set;
            shows S => S :: Monoid with
                unit: identity S;
                timesOp: f:(S => S), g:(S => S) -> (compose f g):(S => S);
        """)

        # Define $M$ as a submonoid of the monoid of transformations of $S$.
        # This should automatically derive $M$ to be a monoid.
        load_zfc_item("""
        struct TransformationMonoid (set M) {
            arguments
                S :: Set;
            inherits
                M :: Submonoid(S => S);
        }
        """)

        ## Invertible elements in a monoid

        # Definition of invertible (here we need to derive that composition
        # and unit refers to those in $M$):
        load_zfc_item("""
        definition invertible:
            fixes
                M :: Monoid;
                u : M;
            defines ∃v:M. u * v = 1 ∧ v * u = 1
        """)
        
        # Definition of inverse of an element:
        load_zfc_item("""
        definition inverse:
            fixes
                M :: Monoid;
                u : M;
            assumes
                invertible u;
            defines THE v:M. u * v = 1 ∧ v * u = 1
        """)

        # If an element is invertible in a submonoid $N$ of $M$, then it is also
        # invertible in $M$. Moreover its inverse (in $M$) also lies in $N$.
        load_zfc_item("""
        theorem submonoid_invertible:
            fixes
                M :: Monoid;
                N :: Submonoid(M);
                u : N;
            assumes
                invertible [M] u;
            shows invertible [N] u
        """)

        load_zfc_item("""
        theorem submonoid_inverse_closed:
            fixes
                M :: Monoid;
                N :: Submonoid(M);
                u : N;
            assumes
                invertible [M] u;
            shows inverse [N] u ∈ N        
        """)

        ## Groups

        # We now define groups as an extension of monoid.
        load_zfc_item("""
        struct Group (set G) {
            inherits
                G :: Monoid;
            properties
                invertible: ∀u:G. invertible u;
        }
        """)

        # Definition of subgroup
        load_zfc_item("""
        struct Subgroup (set H) {
            arguments
                G :: Group;
            inherits
                H :: Submonoid(G);
                H :: Group;
        }
        """)

        load_zfc_item("""
        theorem subgroup_transitive:
            fixes
                M :: Group;
                N :: Subgroup(M);
                K :: Subgroup(N);
            shows K :: Subgroup(M)
        """)

        ## Morphisms

        load_zfc_item("""
        morphism Injective (f : M -> N) {
            properties
                inj: ∀x:M. ∀y:M. f(x) = f(y) ⟶ x = y;
        }
        """)

        load_zfc_item("""
        morphism Surjective (f : M -> N) {
            properties
                surj: ∀y:N. ∃x:M. app f x = y;
        }
        """)

        load_zfc_item("""
        morphism Bijective (f : M -> N) {
            inherits
                f :: Injective;
                f :: Surjective;
        }
        """)

        # We are now ready to define morphisms and isomorphisms on monoids
        load_zfc_item("""
        morphism MonoidMorphism (f : M -> N) {
            inherits
                M :: Monoid;
                N :: Monoid;
            properties
                comp: ∀x:M. ∀y:M. f(x) * f(y) = f(x * y);
        }
        """)

        load_zfc_item("""
        morphism MonoidIsomorphism (f : M -> N) {
            inherits
                f :: MonoidMorphism;
                f :: Bijective;
        }
        """)

        ## Units in a monoid

        # We first define units in a monoid as a set, then show that they automatically
        # inherits a group structure.
        load_zfc_item("""
        definition units:
            fixes
                M :: Monoid;
            defines {u:M. invertible u}
        """)

        load_zfc_item("""
        theorem units_is_subgroup:
            fixes
                M :: Monoid;
            shows units :: Subgroup(M)
        """)

        ## Units in transformation

        # Units in the transformation monoid correspond to bijective functions:
        load_zfc_item("""
        theorem invertible_is_bijective:
           fixes
                S :: Set;
                f : S => S;
           shows invertible [S => S] f ⟷ Bijective f
        """)

        # Hence, the set of unit elements are the bijective functions
        load_zfc_item("""
        theorem units_bijective:
            fixes
                S :: Set;
            shows units [S => S] = {f: S => S. Bijective f}
        """)

        # Hence, we obtain that the units (bijective functions) form a group.
        load_zfc_item("""
        theorem units_is_group:
            fixes
                S :: Set;
            shows units [S => S] :: Group
        """)

        print(zfc.zfc_theory)


if __name__ == "__main__":
    unittest.main()
