# Author: Bohua Zhan

from kernel.type import Type
from kernel.term import Var, Term
from kernel.thm import Thm, InvalidDerivationException
from logic.proofterm import ProofTerm
from logic import matcher

class ConvException(Exception):
    pass

class Conv():
    """A conversion is a function for rewriting a term.

    A Conv object has two main methods:
    __call__ - function to obtain the equality from the term.
    get_proof_term - function to obtain the proof term for the equality.

    """
    def __call__(self, t):
        return self.get_proof_term(t).th

    def get_proof_term(self, t):
        raise NotImplementedError

    def apply_to_pt(self, pt):
        """Apply to the given proof term."""
        eq_pt = self.get_proof_term(pt.th.concl)
        return ProofTerm.equal_elim(eq_pt, pt)

class all_conv(Conv):
    """Returns the trivial equality t = t."""
    def __call__(self, t):
        return Thm.reflexive(t)

    def get_proof_term(self, t):
        return ProofTerm.reflexive(t)

class no_conv(Conv):
    """Always fails."""
    def __call__(self, t):
        raise ConvException()

    def get_proof_term(self, t):
        raise ConvException()

class combination_conv(Conv):
    """Apply cv1 to the function and cv2 to the argument."""
    def __init__(self, cv1, cv2):
        assert isinstance(cv1, Conv) and isinstance(cv2, Conv), "combination_conv: argument"
        self.cv1 = cv1
        self.cv2 = cv2

    def __call__(self, t):
        if t.ty != Term.COMB:
            raise ConvException()
        return Thm.combination(self.cv1(t.fun), self.cv2(t.arg))

    def get_proof_term(self, t):
        if t.ty != Term.COMB:
            raise ConvException()
        pt1 = self.cv1.get_proof_term(t.fun)
        pt2 = self.cv2.get_proof_term(t.arg)

        # Obtain some savings if one of pt1 and pt2 is reflexivity:
        if pt1.th.is_reflexive():
            return ProofTerm.arg_combination(pt1.th.concl.arg, pt2)
        elif pt2.th.is_reflexive():
            return ProofTerm.fun_combination(pt2.th.concl.arg, pt1)
        else:
            return ProofTerm.combination(pt1, pt2)

class then_conv(Conv):
    """Applies cv1, followed by cv2."""
    def __init__(self, cv1, cv2):
        assert isinstance(cv1, Conv) and isinstance(cv2, Conv), "then_conv: argument"
        self.cv1 = cv1
        self.cv2 = cv2

    def __call__(self, t):
        th1 = self.cv1(t)
        (_, t2) = th1.concl.dest_binop()
        th2 = self.cv2(t2)
        return Thm.transitive(th1, th2)

    def get_proof_term(self, t):
        pt1 = self.cv1.get_proof_term(t)
        (_, t2) = pt1.th.concl.dest_binop()
        pt2 = self.cv2.get_proof_term(t2)
        
        # Obtain some savings if one of pt1 and pt2 is reflexivity:
        if pt1.th.is_reflexive():
            return pt2
        elif pt2.th.is_reflexive():
            return pt1
        else:
            return ProofTerm.transitive(pt1, pt2)

class else_conv(Conv):
    """Applies cv1, if fails, apply cv2."""
    def __init__(self, cv1, cv2):
        assert isinstance(cv1, Conv) and isinstance(cv2, Conv), "else_conv: argument"
        self.cv1 = cv1
        self.cv2 = cv2

    def __call__(self, t):
        try:
            return self.cv1(t)
        except ConvException:
            return self.cv2(t)

    def get_proof_term(self, t):
        try:
            return self.cv1.get_proof_term(t)
        except ConvException:
            return self.cv2.get_proof_term(t)

class beta_conv(Conv):
    """Applies beta-conversion."""
    def __call__(self, t):
        try:
            return Thm.beta_conv(t)
        except InvalidDerivationException:
            raise ConvException()

    def get_proof_term(self, t):
        try:
            return ProofTerm.beta_conv(t)
        except InvalidDerivationException:
            raise ConvException()

class abs_conv(Conv):
    """Applies conversion to the body of abstraction."""
    def __init__(self, cv):
        assert isinstance(cv, Conv)
        self.cv = cv
    
    def __call__(self, t):
        if t.ty != Term.ABS:
            raise ConvException()
        
        # Find a new variable x and substitute for body
        v = Var(t.var_name, t.var_T)
        t2 = t.subst_bound(v)
        return Thm.abstraction(v, self.cv(t2))

    def get_proof_term(self, t):
        if t.ty != Term.ABS:
            raise ConvException()

        # Find a new variable x and substitute for body
        v = Var(t.var_name, t.var_T)
        t2 = t.subst_bound(v)
        return ProofTerm.abstraction(self.cv.get_proof_term(t2), v)

def try_conv(cv):
    return else_conv(cv, all_conv())

def comb_conv(cv):
    return combination_conv(cv, cv)

def arg_conv(cv):
    return combination_conv(all_conv(), cv)

def fun_conv(cv):
    return combination_conv(cv, all_conv())

def arg1_conv(cv):
    return fun_conv(arg_conv(cv))

def fun2_conv(cv):
    return fun_conv(fun_conv(cv))

def binop_conv(cv):
    return combination_conv(arg_conv(cv), cv)

def every_conv(*args):
    if len(args) == 0:
        return all_conv()
    elif len(args) == 1:
        return args[0]
    else:
        return then_conv(args[0], every_conv(*args[1:]))

class sub_conv(Conv):
    def __init__(self, cv):
        self.cv = cv

    def get_proof_term(self, t):
        if t.ty == Term.COMB:
            return comb_conv(self.cv).get_proof_term(t)
        elif t.ty == Term.ABS:
            return abs_conv(self.cv).get_proof_term(t)
        else:
            return ProofTerm.reflexive(t)

class bottom_conv(Conv):
    """Applies cv repeatedly in the bottom-up manner."""
    def __init__(self, cv):
        assert isinstance(cv, Conv), "bottom_conv: argument"
        self.cv = cv

    def get_proof_term(self, t):
        return then_conv(sub_conv(self), try_conv(self.cv)).get_proof_term(t)

class top_conv(Conv):
    """Applies cv repeatedly in the top-down manner."""
    def __init__(self, cv):
        assert isinstance(cv, Conv), "top_conv: argument"
        self.cv = cv

    def get_proof_term(self, t):
        return then_conv(try_conv(self.cv), sub_conv(self)).get_proof_term(t)

class rewr_conv(Conv):
    """Rewrite using the given equality theorem.
    
    match_vars -- whether variables in pat should be matched.
    
    """
    def __init__(self, pt, match_vars=True):
        assert isinstance(pt, ProofTerm), "rewr_conv: argument"
        self.pt = pt
        self.th = pt.th
        self.match_vars = match_vars

        # Deconstruct th into assumptions and conclusion
        self.As, self.C = self.th.concl.strip_implies()
        assert Term.is_equals(self.C), "rewr_conv: theorem is not an equality."
        self.pat = self.C.arg1

    def get_proof_term(self, t):
        tyinst, inst = dict(), dict()

        if self.match_vars:
            try:
                matcher.first_order_match_incr(self.pat, t, (tyinst, inst))
            except matcher.MatchException:
                raise ConvException()
        elif self.pat != t:
            raise ConvException()

        pt = ProofTerm.substitution(inst, ProofTerm.subst_type(tyinst, self.pt))
        As, _ = pt.th.concl.strip_implies()
        for A in As:
            pt = ProofTerm.implies_elim(pt, ProofTerm.assume(A))
        return pt


def rewr_conv_thm(thy, th_name):
    """Rewrite using the theorem with the given name."""
    return rewr_conv(ProofTerm.theorem(thy, th_name))

def rewr_conv_thm_sym(thy, th_name):
    """Rewrite in the reverse direction using the theorem with
    the given name.

    """
    return rewr_conv(ProofTerm.symmetric(ProofTerm.theorem(thy, th_name)))
