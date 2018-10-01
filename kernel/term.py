# Author: Bohua Zhan

import abc
from enum import Enum
from kernel.type import *

class UnknownTermException(Exception):
    pass

class OpenTermException(Exception):
    pass

class InvalidTermException(Exception):
    pass

class TermSubstitutionException(Exception):
    pass

class Term(abc.ABC):
    """Represents a term in higher-order logic.
    """

    (VAR, CONST, COMB, ABS, BOUND) = range(5)

    def __init__(self, ty):
        self.ty = ty

    def __str__(self):
        if self.ty == Term.VAR:
            return "Var(" + self.name + "," + str(self.T) + ")"
        elif self.ty == Term.CONST:
            return "Const(" + self.name + "," + str(self.T) + ")"
        elif self.ty == Term.COMB:
            # a $ b $ c associates to the left. So parenthesis is needed to
            # express a $ (b $ c).
            if self.arg.ty == Term.COMB:
                return str(self.fun) + " $ (" + str(self.arg) + ")"
            else:
                return str(self.fun) + " $ " + str(self.arg)
        elif self.ty == Term.ABS:
            return "Abs(" + self.var_name + "," + str(self.T) + "," + str(self.body) + ")"
        elif self.ty == Term.BOUND:
            return "Bound " + str(self.n)
        else:
            raise UnknownTermException()

    def _repr(self, bd_vars):
        """Print the term in short form. Note we do not yet handle name
        collisions in lambda terms.

        """
        if self.ty == Term.VAR or self.ty == Term.CONST:
            return self.name
        elif self.ty == Term.COMB:
            # a b c associates to the left. So parenthesis is needed to express
            # a (b c). Parenthesis is also needed for lambda terms.
            if self.fun.ty == Term.ABS:
                str_fun = "(" + self.fun._repr(bd_vars) + ")"
            else:
                str_fun = self.fun._repr(bd_vars)
            if self.arg.ty == Term.COMB or self.arg.ty == Term.ABS:
                str_arg = "(" + self.arg._repr(bd_vars) + ")"
            else:
                str_arg = self.arg._repr(bd_vars)
            return str_fun + " " + str_arg
        elif self.ty == Term.ABS:
            return "%" + self.var_name + ". " + self.body._repr([self.var_name] + bd_vars)
        elif self.ty == Term.BOUND:
            if self.n >= len(bd_vars):
                raise OpenTermException
            else:
                return bd_vars[self.n]
        else:
            raise UnknownTermException()

    def __repr__(self):
        return self._repr([])

    def __hash__(self):
        if self.ty == Term.VAR:
            return hash(("VAR", self.name, hash(self.T)))
        elif self.ty == Term.CONST:
            return hash(("CONST", self.name, hash(self.T)))
        elif self.ty == Term.COMB:
            return hash(("COMB", hash(self.fun), hash(self.arg)))
        elif self.ty == Term.ABS:
            return hash(("ABS", hash(self.T), hash(self.body)))
        elif self.ty == Term.BOUND:
            return hash(("BOUND", n))
        else:
            raise UnknownTermException()

    def __eq__(self, other):
        """Equality on terms is defined by alpha-conversion. This ignores
        suggested names in lambda terms.

        """
        if self.ty != other.ty:
            return False
        elif self.ty == Term.VAR or self.ty == Term.CONST:
            return self.name == other.name and self.T == other.T
        elif self.ty == Term.COMB:
            return self.fun == other.fun and self.arg == other.arg
        elif self.ty == Term.ABS:
            # Note the suggested variable name is not important
            return self.T == other.T and self.body == other.body
        elif self.ty == Term.BOUND:
            return self.n == other.n
        else:
            raise UnknownTermException()

    def _type_of(self, bd_vars):
        """Returns type of the term, with minimal type-checking.
        """
        if self.ty == Term.VAR or self.ty == Term.CONST:
            return self.T
        elif self.ty == Term.COMB:
            type_fun = self.fun._type_of(bd_vars)
            if type_fun.is_fun():
                return type_fun.range_type()
            else:
                raise InvalidTermException()
        elif self.ty == Term.ABS:
            return TFun(self.T, self.body._type_of([self.T] + bd_vars))
        elif self.ty == Term.BOUND:
            if self.n >= len(bd_vars):
                raise OpenTermException
            else:
                return bd_vars[self.n]
        else:
            raise UnknownTermException()
    
    def type_of(self):
        return self._type_of([])

    @staticmethod
    def Var(name, T):
        t = Term(Term.VAR)
        t.name = name
        t.T = T
        return t

    @staticmethod
    def Const(name, T):
        t = Term(Term.CONST)
        t.name = name
        t.T = T
        return t

    @staticmethod
    def Comb(fun, arg):
        t = Term(Term.COMB)
        t.fun = fun
        t.arg = arg
        return t

    @staticmethod
    def Abs(var_name, T, body):
        t = Term(Term.ABS)
        t.var_name = var_name
        t.T = T
        t.body = body
        return t

    @staticmethod
    def Bound(n):
        t = Term(Term.BOUND)
        t.n = n
        return t

    def subst_type(self, subst):
        """Perform substitution on type variables."""
        if self.ty == Term.VAR:
            return Var(self.name, self.T.subst(subst))
        elif self.ty == Term.CONST:
            return Const(self.name, self.T.subst(subst))
        elif self.ty == Term.COMB:
            return Comb(self.fun.subst_type(subst), self.arg.subst_type(subst))
        elif self.ty == Term.ABS:
            return Abs(self.var_name, self.T.subst(subst), self.body.subst_type(subst))
        elif self.ty == Term.BOUND:
            return self
        else:
            raise UnknownTermException()

    def subst(self, subst):
        """Perform substitution on term variables.

        Here subst must be a dictionary mapping from variable names to the
        substituted term. The type of the substituted term must match *exactly*
        the type of the variable. If substitution on types is needed, it should
        be performed before calling subst.

        """
        assert isinstance(subst, dict), "subst must be a dictionary"
        if self.ty == Term.VAR:
            if self.name in subst:
                t = subst[self.name]
                if t.type_of() == self.T:
                    return subst[self.name]
                else:
                    raise TermSubstitutionException()
            else:
                return self
        elif self.ty == Term.CONST:
            return self
        elif self.ty == Term.COMB:
            return Comb(self.fun.subst(subst), self.arg.subst(subst))
        elif self.ty == Term.ABS:
            return Abs(self.var_name, self.T, self.body.subst(subst))
        elif self.ty == Term.BOUND:
            return self
        else:
            raise UnknownTermException()

    @staticmethod
    def list_comb(f, ts):
        """Returns the term f t1 t2 ... tn."""
        for t in ts:
            f = Comb(f, t)
        return f

    def strip_comb(self):
        """Given a term f t1 t2 ... tn, returns (f, [t1, t2, ..., tn])."""
        if self.ty == Term.COMB:
            (f, args) = strip_comb(self.fun)
            return (f, args + [self.arg])
        else:
            return (self, [])

    def head_of(self):
        """Given a term f t1 t2 ... tn, returns f."""
        if self.ty == Term.COMB:
            return self.fun.head_of()
        else:
            return self

    def is_binop(self):
        """Whether self is of the form f t1 t2."""
        return self.ty == Term.COMB and self.fun.ty == Term.COMB

    def dest_binop(self):
        """Given a term f t1 t2, return (t1, t2)."""
        return (self.fun.arg, self.arg)

    def is_implies(self):
        """Whether self is of the form A --> B."""
        implies = Const("implies", TFun(hol_bool, TFun(hol_bool, hol_bool)))
        return self.is_binop() and self.head_of() == implies

    @staticmethod
    def mk_implies(s, t):
        """Construct the term s --> t."""
        implies = Const("implies", TFun(hol_bool, TFun(hol_bool, hol_bool)))
        return Term.list_comb(implies, [s, t])

    @staticmethod
    def equals(T):
        """Returns the equals constant for the given type."""
        return Const("equals", TFun(T, TFun(T, hol_bool)))

    def is_equals(self):
        """Whether self is of the form A = B."""
        if self.is_binop():
            f = self.head_of()
            return f.ty == Term.CONST and f.name == "equals"
        else:
            return False

    @staticmethod
    def mk_equals(s, t):
        """Construct the term s = t."""
        T = s.type_of()
        return Term.list_comb(Term.equals(T), [s, t])

# Export constructors of terms to global namespace.
Var = Term.Var
Const = Term.Const
Comb = Term.Comb
Abs = Term.Abs
Bound = Term.Bound
