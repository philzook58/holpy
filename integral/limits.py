"""Computing limits."""

from fractions import Fraction
from typing import Optional, Union

from integral import conditions
from integral import expr
from integral.conditions import Conditions, is_const, is_negative, is_positive
from integral.expr import NEG_INF, POS_INF, Const, Expr


"""Return value of comparison."""
LESS, EQUAL, GREATER, UNKNOWN = -1, 0, 1, 2


class Asymptote:
    """Represents the asymptotic growth of an expression as x -> oo."""
    pass


class Unknown(Asymptote):
    def __init__(self):
        pass

    def __str__(self):
        return "Unknown()"

    def __repr__(self):
        return "Unknown()"

    def __eq__(self, other):
        return isinstance(other, Unknown)


class PolyLog(Asymptote):
    """Approaches a polynomial in x, with possible logarithm factors.
    
    order: List[Union[int, Fraction, Expr]] - exponent of x, log(x),
    log(log(x)), etc, each order is an expression independent of x.
    
    """
    def __init__(self, *order):
        self.order = []
        for n in order:
            if isinstance(n, (int, Fraction)):
                self.order.append(Const(n))
            elif isinstance(n, Expr):
                self.order.append(n)
            else:
                raise AssertionError("PolyLog")

    def __str__(self):
        return "PolyLog(%s)" % (','.join(str(n) for n in self.order))

    def __repr__(self):
        return str(self)

    def __eq__(self, other):
        return isinstance(other, PolyLog) and self.order == other.order

    def get_order(self, i: int) -> Expr:
        if i < len(self.order):
            return self.order[i]
        else:
            return Const(0)


class Exp(Asymptote):
    """Approaches an exponential in x.
    
    order: Asymptote - exponent of the exponential, where base is
    assumed to be e.
    
    """
    def __init__(self, order: Asymptote):
        self.order = order

    def __str__(self):
        return "Exp(%s)" % self.order

    def __repr__(self):
        return str(self)

    def __eq__(self, other):
        return isinstance(other, Exp) and self.order == other.order


def asymp_compare(a: Asymptote, b: Asymptote) -> int:
    """Returns the maximum of two asymptotes."""
    if isinstance(a, Unknown) or isinstance(b, Unknown):
        return UNKNOWN
    elif isinstance(a, Exp) and isinstance(b, Exp):
        return asymp_compare(a.order, b.order)
    elif isinstance(a, Exp) and isinstance(b, PolyLog):
        return GREATER
    elif isinstance(a, PolyLog) and isinstance(b, Exp):
        return LESS
    elif isinstance(a, PolyLog) and isinstance(b, PolyLog):
        l = max(len(a.order), len(b.order))
        for i in range(l):
            ai, bi = a.get_order(i), b.get_order(i)
            if ai.is_const() and bi.is_const():
                if ai.val < bi.val:
                    return LESS
                elif ai.val > bi.val:
                    return GREATER
            if ai != bi:
                return UNKNOWN
        return EQUAL
    else:
        raise NotImplementedError
    
def asymp_add(a: Asymptote, b: Asymptote) -> Asymptote:
    """Return the sum of two asymptotes."""
    if isinstance(a, Unknown) or isinstance(b, Unknown):
        return Unknown()

    cmp = asymp_compare(a, b)
    if cmp == LESS:
        return b
    elif cmp == GREATER or cmp == EQUAL:
        return a
    else:
        return Unknown()

def asymp_add_inv(a: Asymptote, b: Asymptote) -> Asymptote:
    """Return the sum of two decaying asumptotes."""
    if isinstance(a, Unknown) or isinstance(b, Unknown):
        return Unknown()

    cmp = asymp_compare(a, b)
    if cmp == GREATER:
        return b
    elif cmp == LESS or cmp == EQUAL:
        return a
    else:
        return Unknown()    

def asymp_mult(a: Asymptote, b: Asymptote) -> Asymptote:
    """Return the product of two asymptotes."""
    if isinstance(a, Unknown) or isinstance(b, Unknown):
        return Unknown()
    elif isinstance(a, Exp) and isinstance(b, Exp):
        s = asymp_add(a.order, b.order)
        if isinstance(s, Unknown):
            return Unknown()
        else:
            return Exp(s)
    elif isinstance(a, Exp) and isinstance(b, PolyLog):
        return a
    elif isinstance(a, PolyLog) and isinstance(b, Exp):
        return b
    elif isinstance(a, PolyLog) and isinstance(b, PolyLog):
        l = max(len(a.order), len(b.order))
        s_order = []
        for i in range(l):
            ai, bi = a.get_order(i), b.get_order(i)
            s_order.append((ai + bi).normalize())
        return PolyLog(*s_order)
    else:
        raise NotImplementedError

def asymp_div(a: Asymptote, b: Asymptote) -> Asymptote:
    """Return the quotient of two asymptotes.
    
    Assume a > b according to asymp_compare. Otherwise throw Exception.
    
    """
    if asymp_compare(a, b) != GREATER:
        raise AssertionError("asymp_div")

    if isinstance(a, Exp) and isinstance(b, Exp):
        return a
    elif isinstance(a, Exp) and isinstance(b, PolyLog):
        return a
    elif isinstance(a, PolyLog) and isinstance(b, PolyLog):
        l = max(len(a.order), len(b.order))
        s_order = []
        for i in range(l):
            ai, bi = a.get_order(i), b.get_order(i)
            s_order.append((ai - bi).normalize())
        return PolyLog(*s_order)
    else:
        raise NotImplementedError

def asymp_power(a: Asymptote, b: Expr) -> Asymptote:
    """Raise an asymptotic limit to a constant power."""
    if isinstance(a, Unknown):
        return Unknown()
    elif isinstance(a, Exp):
        # This just multiplies an exponent by a constant, which is not
        # kept track of in this framework.
        return a
    elif isinstance(a, PolyLog):
        # Multiplies all orders in a by the given constant.
        return PolyLog(*((e * b).normalize() for e in a.order))
    else:
        raise NotImplementedError

def exp_asymp(a: Asymptote) -> Asymptote:
    """Exponential of an asymptote."""
    if isinstance(a, Unknown):
        return Unknown()
    else:
        return Exp(a)


"""Side of approaching the limit."""
AT_CONST, FROM_ABOVE, FROM_BELOW, TWO_SIDED = range(4)

def opp_side(side: int) -> int:
    """Return the opposite of the given side."""
    if side == AT_CONST:
        return AT_CONST
    elif side == FROM_ABOVE:
        return FROM_BELOW
    elif side == FROM_BELOW:
        return FROM_ABOVE
    elif side == TWO_SIDED:
        return TWO_SIDED
    else:
        raise NotImplementedError


class Limit:
    """Represents the limit of an expression as x -> oo.
    
    e: Optional[Expr] - limit of the expression, can be oo, -oo, a finite
    value, or None to indicate the limit is unknown.

    asymp: Asymptote - asymptotic growth/decay rate of the limit,
    valid only when the limit is oo, -oo or 0.

    side: int - one of FROM_ABOVE, FROM_BELOW and TWO_SIDED. This is used
    only when e is a finite value. TWO_SIDED is the most general case and
    can be used when the side is unknown.

    """
    def __init__(self, e: Optional[Union[int, Fraction, Expr]], *,
                 asymp: Asymptote = Unknown(), side: int = TWO_SIDED):
        if isinstance(e, (int, Fraction)):
            self.e = Const(e)
        elif isinstance(e, Expr) or e is None:
            self.e = e
        else:
            raise AssertionError("Limit")
        self.asymp = asymp
        self.side = side
    
    def __str__(self):
        return "Limit(%s,%s,%s)" % (self.e, self.asymp, self.side)

    def __repr__(self):
        return str(self)

    def __eq__(self, other):
        return isinstance(other, Limit) and self.e == other.e and self.asymp == other.asymp and \
            self.side == other.side


def limit_add(a: Limit, b: Limit) -> Limit:
    """Addition between two limits."""
    if a.e is None or b.e is None:
        return Limit(None)
    elif a.e == POS_INF and b.e == POS_INF:
        return Limit(POS_INF, asymp=asymp_add(a.asymp, b.asymp))
    elif a.e == POS_INF and b.e == NEG_INF:
        cmp = asymp_compare(a.asymp, b.asymp)
        if cmp == UNKNOWN:
            return Limit(None)
        elif cmp == LESS:
            return Limit(NEG_INF, asymp=b.asymp)
        elif cmp == GREATER:
            return Limit(POS_INF, asymp=a.asymp)
        else:  # EQUAL case, oo - oo = unknown
            return Limit(None)
    elif a.e == NEG_INF and b.e == POS_INF:
        return limit_add(b, a)
    elif a.e == NEG_INF and b.e == NEG_INF:
        return Limit(NEG_INF, asymp=asymp_add(a.asymp, b.asymp))
    elif a.e == POS_INF:
        return Limit(POS_INF, asymp=a.asymp)
    elif b.e == POS_INF:
        return Limit(POS_INF, asymp=b.asymp)
    elif a.e == NEG_INF:
        return Limit(NEG_INF, asymp=a.asymp)
    elif b.e == NEG_INF:
        return Limit(NEG_INF, asymp=b.asymp)
    else:
        res_e = (a.e + b.e).normalize()
        if a.side == TWO_SIDED or b.side == TWO_SIDED:
            return Limit(res_e, asymp=asymp_add_inv(a.asymp, b.asymp))
        elif a.side == AT_CONST:
            return Limit(res_e, asymp=b.asymp, side=b.side)
        elif b.side == AT_CONST:
            return Limit(res_e, asymp=a.asymp, side=a.side)
        elif a.side == FROM_ABOVE and b.side == FROM_ABOVE:
            return Limit(res_e, asymp=asymp_add_inv(a.asymp, b.asymp), side=FROM_ABOVE)
        elif a.side == FROM_ABOVE and b.side == FROM_BELOW:
            cmp = asymp_compare(a.asymp, b.asymp)
            if cmp == UNKNOWN:
                return Limit(res_e, side=TWO_SIDED)
            elif cmp == LESS:
                return Limit(res_e, asymp=a.asymp, side=FROM_ABOVE)
            elif cmp == GREATER:
                return Limit(res_e, asymp=b.asymp, side=FROM_BELOW)
            else:  # EQUAL case
                return Limit(res_e, side=TWO_SIDED)
        elif a.side == FROM_BELOW and b.side == FROM_ABOVE:
            return limit_add(b, a)
        elif a.side == FROM_BELOW and b.side == FROM_BELOW:
            return Limit(res_e, asymp=asymp_add_inv(a.asymp, b.asymp), side=FROM_BELOW)
        else:
            raise NotImplementedError

def limit_uminus(a: Limit) -> Limit:
    """Negation of a limit."""
    if a.e is None:
        return Limit(None)
    elif a.e == POS_INF:
        return Limit(NEG_INF, asymp=a.asymp)
    elif a.e == NEG_INF:
        return Limit(POS_INF, asymp=a.asymp)
    else:
        res_e = (-(a.e)).normalize()
        if a.side == TWO_SIDED:
            return Limit(res_e, asymp=a.asymp, side=TWO_SIDED)
        elif a.side == FROM_ABOVE:
            return Limit(res_e, asymp=a.asymp, side=FROM_BELOW)
        elif a.side == FROM_BELOW:
            return Limit(res_e, asymp=a.asymp, side=FROM_ABOVE)
        elif a.side == AT_CONST:
            return Limit(res_e, side=AT_CONST)
        else:
            raise NotImplementedError

def limit_mult(a: Limit, b: Limit, conds: Optional[Conditions] = None) -> Limit:
    """Multiplication between two limits."""
    if conds is None:
        conds = Conditions()

    if a.e is None or b.e is None:
        return Limit(None)
    elif a.e == POS_INF and b.e == POS_INF:
        return Limit(POS_INF, asymp=asymp_mult(a.asymp, b.asymp))
    elif a.e == POS_INF and b.e == NEG_INF:
        return Limit(NEG_INF, asymp=asymp_mult(a.asymp, b.asymp))
    elif a.e == NEG_INF and b.e == POS_INF:
        return Limit(NEG_INF, asymp=asymp_mult(a.asymp, b.asymp))
    elif a.e == NEG_INF and b.e == NEG_INF:
        return Limit(POS_INF, asymp=asymp_mult(a.asymp, b.asymp))
    elif a.e == POS_INF:
        if conditions.is_positive(b.e, conds):
            return Limit(POS_INF, asymp=a.asymp)
        elif conditions.is_negative(b.e, conds):
            return Limit(NEG_INF, asymp=a.asymp)
        elif b.e == Const(0):
            if b.side == AT_CONST:
                return Limit(Const(0), side=AT_CONST)
            cmp = asymp_compare(a.asymp, b.asymp)
            if cmp == UNKNOWN:
                return Limit(None)
            elif cmp == LESS:
                return Limit(Const(0), asymp=asymp_div(b.asymp, a.asymp), side=b.side)
            elif cmp == GREATER:
                return Limit(POS_INF, asymp=asymp_div(a.asymp, b.asymp))
            else:  # EQUAL case
                return Limit(None)
        else:
            return Limit(None)
    elif b.e == POS_INF:
        return limit_mult(b, a, conds=conds)
    elif a.e == NEG_INF:
        return limit_uminus(limit_mult(limit_uminus(a), b, conds=conds))
    elif b.e == NEG_INF:
        return limit_mult(b, a, conds=conds)
    else:
        res_e = (a.e * b.e).normalize()
        if a.side == TWO_SIDED or b.side == TWO_SIDED:
            return Limit(res_e, asymp=asymp_mult(a.asymp, b.asymp), side=TWO_SIDED)
        elif a.side == AT_CONST:
            return Limit(res_e, asymp=b.asymp, side=b.side)
        elif b.side == AT_CONST:
            return Limit(res_e, asymp=a.asymp, side=a.side)
        elif a.side == FROM_ABOVE and b.side == FROM_ABOVE:
            return Limit(res_e, asymp=asymp_mult(a.asymp, b.asymp), side=FROM_ABOVE)
        elif a.side == FROM_ABOVE and b.side == FROM_BELOW:
            return Limit(res_e, asymp=asymp_mult(a.asymp, b.asymp), side=FROM_BELOW)
        elif a.side == FROM_BELOW and b.side == FROM_ABOVE:
            return Limit(res_e, asymp=asymp_mult(a.asymp, b.asymp), side=FROM_BELOW)
        elif a.side == FROM_BELOW and b.side == FROM_BELOW:
            return Limit(res_e, asymp=asymp_mult(a.asymp, b.asymp), side=FROM_ABOVE)
        else:
            raise NotImplementedError

def limit_inverse(a: Limit) -> Limit:
    """Inverse of a limit"""
    if a.e is None:
        return Limit(None)
    elif a.e == POS_INF:
        return Limit(Const(0), asymp=a.asymp, side=FROM_ABOVE)
    elif a.e == NEG_INF:
        return Limit(Const(0), asymp=a.asymp, side=FROM_BELOW)
    elif a.e == Const(0):
        if a.side == TWO_SIDED or a.side == AT_CONST:
            return Limit(None)
        elif a.side == FROM_ABOVE:
            return Limit(POS_INF, asymp=a.asymp)
        elif a.side == FROM_BELOW:
            return Limit(NEG_INF, asymp=a.asymp)
        else:
            raise NotImplementedError
    else:
        res_e = (Const(1) / a.e).normalize()
        if a.side == TWO_SIDED:
            return Limit(res_e, asymp=a.asymp, side=TWO_SIDED)
        elif a.side == AT_CONST:
            return Limit(res_e, side=AT_CONST)
        elif a.side == FROM_ABOVE:
            return Limit(res_e, asymp=a.asymp, side=FROM_BELOW)
        elif a.side == FROM_BELOW:
            return Limit(res_e, asymp=a.asymp, side=FROM_ABOVE)
        else:
            raise NotImplementedError

def limit_div(a: Limit, b: Limit, conds: Optional[Conditions] = None) -> Limit:
    """Compute the quotient of two limits."""
    return limit_mult(a, limit_inverse(b), conds=conds)

def limit_power(a: Limit, b: Limit, conds: Optional[Conditions] = None) -> Limit:
    """Compute a limit raised to another limit.
    
    TODO: many cases are still missing.
    
    """
    if conds is None:
        conds = Conditions()

    if a.e is None or b.e is None:
        return Limit(None)
    elif a.e == POS_INF:
        if b.e == POS_INF:
            # TODO: try to figure out asymp in more cases
            return Limit(POS_INF)
        elif b.e == NEG_INF:
            return Limit(None)
        elif is_positive(b.e, conds):
            # Raise to a constant positive power
            return Limit(POS_INF, asymp=asymp_power(a.asymp, b.e))
        elif is_negative(b.e, conds):
            # Raise to a constant negative power
            neg_e = (-(b.e)).normalize()
            return Limit(Const(0), asymp=asymp_power(a.asymp, neg_e), side=FROM_ABOVE)
        else:
            return Limit(None)
    elif a.e == NEG_INF:
        if b.e.is_const() and b.e.val > 0:
            # Positive power, test for parity of numerator
            if Fraction(b.e.val).numerator % 2 == 0:
                return Limit(POS_INF, asymp=asymp_power(a.asymp, b.e))
            else:
                return Limit(NEG_INF, asymp=asymp_power(a.asymp, b.e))
        elif b.e.is_const() and b.e.val < 0:
            # Negative power, test for parity of numerator
            if Fraction(b.e.val).numerator % 2 == 0:
                return Limit(Const(0), asymp=asymp_power(a.asymp, -(b.e)), side=FROM_ABOVE)
            else:
                return Limit(Const(0), asymp=asymp_power(a.asymp, -(b.e)), side=FROM_BELOW)
        else:
            return Limit(None)
    elif is_positive(a.e, conds):
        # Base is positive
        if b.e == POS_INF:
            return Limit(POS_INF, asymp=exp_asymp(b.asymp))
        elif b.e == NEG_INF:
            return Limit(Const(0), asymp=exp_asymp(b.asymp), side=FROM_ABOVE)
        elif is_positive(b.e, conds):
            # TODO: try to figure out asymp and side in more cases
            return Limit((a.e ^ b.e).normalize())
        else:
            return Limit(None)
    elif is_negative(a.e, conds):
        # Base is negative
        if b.e.is_const() and b.e.val > 0 and b.side == AT_CONST:
            return Limit((a.e ^ b.e).normalize())
        else:
            return Limit(None)
    elif a.e == Const(0):
        if a.side == AT_CONST:
            return Limit(Const(0), side=AT_CONST)
        elif a.side == FROM_ABOVE:
            if b.e == POS_INF or b.e == NEG_INF:
                return Limit(None)
            elif is_positive(b.e, conds):
                return Limit(Const(0), asymp=asymp_power(a.asymp, b.e), side=FROM_ABOVE)
            elif is_negative(b.e, conds):
                return Limit(POS_INF, asymp=asymp_power(a.asymp, -(b.e)))
            else:
                return Limit(None)
        elif a.side == FROM_BELOW:
            if b.e == POS_INF or b.e == NEG_INF:
                return Limit(None)
            elif b.e.is_const() and b.e.val > 0 and b.side == AT_CONST:
                if Fraction(b.e.val).numerator % 2 == 0:
                    return Limit(Const(0), asymp=asymp_power(a.asymp, b.e), side=FROM_ABOVE)
                else:
                    return Limit(Const(0), asymp=asymp_power(a.asymp, b.e), side=FROM_BELOW)
            else:
                return Limit(None)
        elif a.side == TWO_SIDED:
            if b.e == POS_INF or b.e == NEG_INF:
                return Limit(None)
            elif b.e.is_const() and b.e.val > 0 and b.side == AT_CONST:
                return Limit(Const(0), asymp=asymp_power(a.asymp, b.e))
            else:
                return Limit(None)
        else:
            raise NotImplementedError
    else:
        return Limit(None)

def limit_of_expr(e: Expr, var_name: str, conds: Optional[Conditions] = None) -> Limit:
    """Compute the limit of an expression as variable goes to infinity."""
    if e.is_const():
        return Limit(e)
    elif e.is_fun() and len(e.args) == 0:
        return Limit(e)
    elif e.is_var():
        if e.name == var_name:
            return Limit(POS_INF, asymp=PolyLog(Const(1)))
        else:
            return Limit(e)
    elif e.is_plus():
        l1 = limit_of_expr(e.args[0], var_name, conds=conds)
        l2 = limit_of_expr(e.args[1], var_name, conds=conds)
        return limit_add(l1, l2)
    elif e.is_uminus():
        l = limit_of_expr(e.args[0], var_name, conds=conds)
        return limit_uminus(l)
    elif e.is_minus():
        l1 = limit_of_expr(e.args[0], var_name, conds=conds)
        l2 = limit_of_expr(e.args[1], var_name, conds=conds)
        return limit_add(l1, limit_uminus(l2))
    elif e.is_times():
        l1 = limit_of_expr(e.args[0], var_name, conds=conds)
        l2 = limit_of_expr(e.args[1], var_name, conds=conds)
        return limit_mult(l1, l2, conds=conds)
    elif e.is_divides():
        l1 = limit_of_expr(e.args[0], var_name, conds=conds)
        l2 = limit_of_expr(e.args[1], var_name, conds=conds)
        return limit_div(l1, l2, conds=conds)
    elif e.is_power():
        l1 = limit_of_expr(e.args[0], var_name, conds=conds)
        l2 = limit_of_expr(e.args[1], var_name, conds=conds)
        return limit_power(l1, l2, conds=conds)
    elif e.is_fun() and e.func_name == 'exp':
        l = limit_of_expr(e.args[0], var_name, conds=conds)
        return limit_power(Limit(expr.E, side=AT_CONST), l)
    else:
        # TODO: add support for other functions
        return Limit(None)

def reduce_inf_limit(e: Expr, var_name: str, conds: Optional[Conditions] = None) -> Expr:
    """Reduce limits of expression as much as possible."""
    l = limit_of_expr(e, var_name, conds=conds)
    if l.e is not None:
        return l.e
    elif e.is_plus():
        l1 = reduce_inf_limit(e.args[0], var_name, conds=conds)
        l2 = reduce_inf_limit(e.args[1], var_name, conds=conds)
        if l1 not in (POS_INF, NEG_INF) and l2 not in (POS_INF, NEG_INF):
            return (l1 + l2).normalize()
        else:
            return expr.Limit(var_name, POS_INF, e)
    elif e.is_times():
        if not e.args[0].contains_var(var_name):
            return (e.args[0] * reduce_inf_limit(e.args[1], var_name, conds=conds)).normalize()
        elif not e.args[1].contains_var(var_name):
            return (e.args[1] * reduce_inf_limit(e.args[0], var_name, conds=conds)).normalize()
        else:
            return expr.Limit(var_name, POS_INF, e)
    else:
        return expr.Limit(var_name, POS_INF, e)