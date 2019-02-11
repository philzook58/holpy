# Author: Bohua Zhan

from kernel.term import Term
from kernel.macro import MacroSig, ProofMacro, global_macros
from kernel.proof import Proof
from kernel.thm import Thm
from logic import logic, matcher
from logic.conv import beta_conv, top_conv, rewr_conv
from logic.proofterm import ProofTerm, ProofTermMacro

"""Standard macros in logic."""

class arg_combination_macro(ProofTermMacro):
    """Given theorem x = y and term f, return f x = f y."""

    def __init__(self):
        self.level = 1
        self.sig = MacroSig.TERM
        self.has_theory = False
        self.use_goal = False

    def __call__(self, f, th):
        assert th.concl.is_equals(), "arg_combination"
        return Thm.combination(Thm.reflexive(f), th)

    def get_proof_term(self, f, pt):
        assert pt.th.concl.is_equals(), "arg_combination"
        return ProofTerm.combination(ProofTerm.reflexive(f), pt)

class fun_combination_macro(ProofTermMacro):
    """Given theorem f = g and term x, return f x = g x."""

    def __init__(self):
        self.level = 1
        self.sig = MacroSig.TERM
        self.has_theory = False
        self.use_goal = False

    def __call__(self, x, th):
        assert th.concl.is_equals(), "fun_combination"
        return Thm.combination(th, Thm.reflexive(x))

    def get_proof_term(self, x, pt):
        assert pt.th.concl.is_equals(), "fun_combination"
        return ProofTerm.combination(pt, ProofTerm.reflexive(x))

class beta_norm_macro(ProofTermMacro):
    """Given theorem th, return the normalization of th."""

    def __init__(self):
        self.level = 1
        self.sig = MacroSig.NONE
        self.has_theory = False
        self.use_goal = False

    def __call__(self, th):
        cv = top_conv(beta_conv())
        eq_th = cv(th.concl)
        return Thm(th.assums, eq_th.concl.arg)

    def get_proof_term(self, pt):
        cv = top_conv(beta_conv())
        return ProofTerm.equal_elim(cv.get_proof_term(pt.th.concl), pt)

class apply_theorem_macro(ProofTermMacro):
    """Apply existing theorem in the theory to a list of current
    results in the proof.

    If with_inst is set, the signature is (th_name, concl), where
    th_name is the name of the theorem, concl is the expected conclusion.

    If with_inst is not set, the signature is th_name, where th_name
    is the name of the theorem.

    """
    def __init__(self, *, with_inst=False):
        self.level = 1
        self.with_inst = with_inst
        self.sig = MacroSig.STRING_INSTSP if with_inst else MacroSig.STRING
        self.has_theory = True
        self.use_goal = False

    def __call__(self, thy, args, *prevs):
        tyinst, inst = dict(), dict()
        if self.with_inst:
            name, tyinst, inst = args
        else:
            name = args
        th = thy.get_theorem(name)

        if not self.with_inst:
            As, _ = th.concl.strip_implies()
            for idx, prev_th in enumerate(prevs):
                matcher.first_order_match_incr(As[idx], prev_th.concl, (tyinst, inst))

        As, C = logic.subst_norm(th.concl, (tyinst, inst)).strip_implies()
        new_concl = Term.mk_implies(*(As[len(prevs):] + [C]))

        prev_assums = sum([prev.assums for prev in prevs], ())
        return Thm(th.assums + prev_assums, new_concl)

    def get_proof_term(self, thy, args, *pts):
        tyinst, inst = dict(), dict()
        if self.with_inst:
            name, tyinst, inst = args
        else:
            name = args
        th = thy.get_theorem(name)

        if not self.with_inst:
            As, _ = th.concl.strip_implies()
            for idx, pt in enumerate(pts):
                matcher.first_order_match_incr(As[idx], pt.th.concl, (tyinst, inst))

        pt = ProofTerm.substitution(inst,
                ProofTerm.subst_type(tyinst, ProofTerm.theorem(thy, name)))
        cv = top_conv(beta_conv())
        pt2 = cv.get_proof_term(pt.th.concl)
        pt3 = ProofTerm.equal_elim(pt2, pt)
        for pt in pts:
            pt3 = ProofTerm.implies_elim(pt3, pt)

        return pt3

class rewrite_goal_macro(ProofTermMacro):
    """Apply an existing equality theorem to rewrite a goal.

    The signature is (name, goal), where name is the name of the
    equality theorem. Goal is the statement of the goal.

    Rewrite the goal using the equality theorem. The result must
    be equal to prev.
    
    backward - whether to apply the given equality in the backward
    direction.

    """
    def __init__(self, *, backward=False):
        self.level = 1
        self.backward = backward
        self.sig = MacroSig.STRING_TERM
        self.has_theory = True
        self.use_goal = True

    def __call__(self, thy, args, th):
        assert isinstance(args, tuple) and len(args) == 2 and \
               isinstance(args[0], str) and isinstance(args[1], Term), "rewrite_goal_macro: signature"

        # Simply produce the goal
        _, goal = args
        return Thm(th.assums, goal)

    def get_proof_term(self, thy, args, pt):
        assert isinstance(args, tuple) and len(args) == 2 and \
               isinstance(args[0], str) and isinstance(args[1], Term), "rewrite_goal_macro: signature"

        name, goal = args
        eq_pt = ProofTerm.theorem(thy, name)
        if self.backward:
            eq_pt = ProofTerm.symmetric(eq_pt)
        cv = top_conv(rewr_conv(eq_pt))
        pt2 = cv.get_proof_term(goal)  # goal = th.concl
        pt3 = ProofTerm.symmetric(pt2)  # th.concl = goal
        return ProofTerm.equal_elim(pt3, pt)

global_macros.update({
    "arg_combination": arg_combination_macro(),
    "fun_combination": fun_combination_macro(),
    "beta_norm": beta_norm_macro(),
    "apply_theorem": apply_theorem_macro(),
    "apply_theorem_for": apply_theorem_macro(with_inst=True),
    "rewrite_goal": rewrite_goal_macro(),
    "rewrite_back_goal": rewrite_goal_macro(backward=True)
})
