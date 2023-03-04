"""Module for reasoning about conditions."""

from typing import Dict, List

from integral.expr import Expr, eval_expr, match
from integral.conditions import Conditions
from integral.context import Context, Identity


def subject_of(cond: Expr) -> Expr:
    """Return the subject of a condition.
    
    This is usually the left side of an inequality, or the (only)
    argument of a predicate.
    
    """
    if cond.is_equals() or cond.is_not_equals():
        return cond.args[0]
    if cond.is_greater() or cond.is_greater_eq():
        return cond.args[0]
    if cond.is_less() or cond.is_less_eq():
        return cond.args[0]
    if cond.is_fun() and cond.func_name == 'isInt':
        return cond.args[0]
    raise TypeError

# Tolerance for floating-point rounding errors
tol = 1e-15

# Comparison of floating-point numbers up to rounding error
def approx_equal(a: Expr, b: Expr) -> bool:
    a, b = eval_expr(a), eval_expr(b)
    return abs(a - b) < tol

def approx_not_equal(a: Expr, b: Expr) -> bool:
    a, b = eval_expr(a), eval_expr(b)
    return abs(a - b) > tol

def approx_greater(a: Expr, b: Expr) -> bool:
    a, b = eval_expr(a), eval_expr(b)
    return a - b > tol

def approx_greater_eq(a: Expr, b: Expr) -> bool:
    a, b = eval_expr(a), eval_expr(b)
    return a - b > -tol

def approx_less(a: Expr, b: Expr) -> bool:
    a, b = eval_expr(a), eval_expr(b)
    return b - a > tol

def approx_less_eq(a: Expr, b: Expr) -> bool:
    a, b = eval_expr(a), eval_expr(b)
    return b - a > -tol

def approx_integer(a: Expr) -> bool:
    a = eval_expr(a)
    return abs(round(a) - a) < tol

def init_all_conds(conds: Conditions) -> Dict[Expr, List[Expr]]:
    """Initialize all_conds from a condition object."""
    all_conds: Dict[Expr, List[Expr]] = dict()
    for cond in conds.data:
        x = subject_of(cond)
        if x not in all_conds:
            all_conds[x] = list()
        all_conds[x].append(cond)
    return all_conds

def check_cond(cond: Expr, all_conds: Dict[Expr, List[Expr]]) -> bool:
    """Determine whether cond is implied by the existing set of conditions.
    
    The following checks are performed:

    - If subject of cond is a constant, and the right side is also constant,
      compare using eval_expr.

    - If subject of cond appears in all_conds, try to use the conditions
      available to verify cond.

    """
    x = subject_of(cond)

    # If subject of cond is a constant
    if x.is_constant():
        if cond.is_equals() and cond.args[1].is_constant():
            return approx_equal(x, cond.args[1])
        elif cond.is_not_equals() and cond.args[1].is_constant():
            return approx_not_equal(x, cond.args[1])
        elif cond.is_greater() and cond.args[1].is_constant():
            return approx_greater(x, cond.args[1])
        elif cond.is_greater_eq() and cond.args[1].is_constant():
            return approx_greater_eq(x, cond.args[1])
        elif cond.is_less() and cond.args[1].is_constant():
            return approx_less(x, cond.args[1])
        elif cond.is_less_eq() and cond.args[1].is_constant():
            return approx_less_eq(x, cond.args[1])
        elif cond.is_fun() and cond.func_name == 'isInt':
            return approx_integer(x)

    # If subject of cond appears in all_conds
    if x in all_conds:
        if cond in all_conds[x]:
            return True
        for fact in all_conds[x]:
            if cond.is_greater_eq() and cond.args[1].is_constant():
                # x >= b --> b >= a --> x >= a
                if (fact.is_greater() or fact.is_greater_eq()) and fact.args[1].is_constant():
                    if approx_greater_eq(fact.args[1], cond.args[1]):
                        return True
            if cond.is_greater() and cond.args[1].is_constant():
                # x >= b --> b > a --> x > a
                if fact.is_greater_eq() and fact.args[1].is_constant():
                    if approx_greater(fact.args[1], cond.args[1]):
                        return True
                # x > b --> b >= a --> x > a
                if fact.is_greater() and fact.args[1].is_constant():
                    if approx_greater_eq(fact.args[1], cond.args[1]):
                        return True
            if cond.is_less_eq() and cond.args[1].is_constant():
                # x <= b --> b <= a --> x <= a
                if (fact.is_less() or fact.is_less_eq()) and fact.args[1].is_constant():
                    if approx_less_eq(fact.args[1], cond.args[1]):
                        return True
            if cond.is_less() and cond.args[1].is_constant():
                # x <= b --> b < a --> x < a
                if fact.is_less_eq() and fact.args[1].is_constant():
                    if approx_less(fact.args[1], cond.args[1]):
                        return True

    # Not found
    return False

def saturate_expr(e: Expr, ineq: Identity, all_conds: Dict[Expr, List[Expr]]):
    """Use the rule ineq to saturate facts about e, add to all_conds."""
    pat = subject_of(ineq.expr)
    inst = match(e, pat)
    if inst is not None:
        res = ineq.expr.inst_pat(inst)
        if check_cond(res, all_conds):
            return  # already in all_conds

        conds = [cond.inst_pat(inst) for cond in ineq.conds.data]
        if all(check_cond(cond, all_conds) for cond in conds):
            if e not in all_conds:
                all_conds[e] = list()
            all_conds[e].append(res)
    return

def saturate_once(e: Expr, ineqs: List[Identity], all_conds: Dict[Expr, List[Expr]]):
    """Perform one round of saturation"""
    all_subs = e.find_all_subexpr()
    for sube, _ in all_subs:
        for ineq in ineqs:
            saturate_expr(sube, ineq, all_conds)

def all_conds_size(all_conds: Dict[Expr, List[Expr]]) -> int:
    """Return number of facts in all_conds."""
    res = 0
    for _, conds in all_conds.items():
        res += len(conds)
    return res        

def saturate(e: Expr, ineqs: List[Identity], all_conds: Dict[Expr, List[Expr]], *, 
             round_limit: int = 5, size_limit: int = 1000):
    """Saturate up to given number of rounds and size limits.
    
    If number of rounds and size limits have been reached without
    saturation, assertion is thrown to alert possible problems.
    
    """
    i = 0
    while True:
        prev_size = all_conds_size(all_conds)
        saturate_once(e, ineqs, all_conds)
        i += 1
        next_size = all_conds_size(all_conds)
        if prev_size == next_size:
            return
        if next_size > size_limit or i > round_limit:
            raise AssertionError("saturate: limit reached")

def print_all_conds(all_conds: Dict[Expr, List[Expr]]):
    for x, conds in all_conds.items():
        print("%s: %s\n" % (x, ', '.join(str(cond) for cond in conds)))

def check_condition(e: Expr, ctx: Context) -> bool:
    """Check whether e holds under the given context."""
    conds = ctx.get_conds()
    all_conds = init_all_conds(conds)
    ineqs = ctx.get_inequalities()
    saturate(subject_of(e), ineqs, all_conds)
    return check_cond(e, all_conds)
