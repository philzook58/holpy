# Author: Bohua Zhan

from kernel.term import Term, Comb, Abs, Bound

class MatchException(Exception):
    pass

"""Matching between two terms.

By default, all variables in the pattern can be instantiated.

"""
def first_order_match_incr(pat, t, inst):
    """First-order matching of pat with t, where inst is the current
    partial instantiation. This is updated by the function.
    
    """
    if pat.ty == Term.VAR:
        if pat.name in inst:
            if t != inst[pat.name]:
                raise MatchException()
        else:
            if Term.is_open(t):
                raise MatchException()
            else:
                inst[pat.name] = t
    elif pat.ty != t.ty:
        raise MatchException()
    elif pat.ty == Term.CONST:
        if pat == t:
            return None
        else:
            raise MatchException()
    elif pat.ty == Term.COMB:
        if pat.fun.ty == Term.VAR and pat.arg == Bound(0):
            if pat.fun.name not in inst:
                inst[pat.fun.name] = Abs("x", pat.fun.T.domain_type(), t)
            else:
                inst_name = inst[pat.fun.name]
                if inst_name.ty == Term.ABS and inst_name.body == t:
                    pass
                elif inst_name.ty == Term.VAR and t == Comb(inst_name, Bound(0)):
                    pass
                else:
                    raise MatchException()
        else:
            first_order_match_incr(pat.fun, t.fun, inst)
            first_order_match_incr(pat.arg, t.arg, inst)
    elif pat.ty == Term.ABS:
        first_order_match_incr(pat.body, t.body, inst)
    elif pat.ty == Term.BOUND:
        if pat.n == t.n:
            return None
        else:
            raise MatchException()
    else:
        raise TypeError()

def first_order_match(pat, t):
    """First-order matching of pat with t. Return the instantiation
    or throws MatchException.

    """
    inst = dict()
    first_order_match_incr(pat, t, inst)
    return inst
