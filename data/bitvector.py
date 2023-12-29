"""Utility functions for bitvectors."""

from typing import Dict
from kernel import term_ord, type
from kernel.proofterm import ProofTerm, refl
from kernel.type import TFun, TConst
from kernel import term
from kernel.term import Const, Term
from logic.conv import Conv, binop_conv, rewr_conv, arg_conv, arg1_conv

# List of currently allowed word lengths
allowed_lengths = tuple(range(1, 129))

# Mapping from word length to word type
WordType: Dict[int, type.Type] = dict()

# Mapping from word type to word length
WordTypeInv: Dict[type.Type, int] = dict()

# Initialize the content of WordType and WordTypeInv
for i in allowed_lengths:
    wordT = TConst("word" + str(i))
    WordType[i] = wordT
    WordTypeInv[wordT] = i

def is_word_type(T: type.Type) -> bool:
    return T in WordTypeInv

def get_word_length(T: type.Type) -> int:
    return WordTypeInv[T]

def zero_extend(len: int, inc: int) -> term.Term:
    """zero_extend function maps bitvector of length len to bitvector
    of length len + inc, padding the extra bits with zeros.
    
    """
    inputT = WordType[len]
    outputT = WordType[len + inc]
    return Const('bv_zero_extend', TFun(inputT, outputT))

def sign_extend(len: int, inc: int) -> term.Term:
    """sign_extend function maps bitvector of length len to bitvector
    of length len + inc, representing signed extension of the input.
    
    """
    inputT = WordType[len]
    outputT = WordType[len + inc]
    return Const('bv_sign_extend', TFun(inputT, outputT))

def extract(len: int, start: int, end: int) -> term.Term:
    """extract function maps bitvector of length len to bitvector
    of length end - start + 1, taking the sub-vector with given starting
    and ending indices.
    
    """
    inputT = WordType[len]
    outputT = WordType[end - start + 1]
    return Const('bv_extract_%d' % start, TFun(inputT, outputT))

def get_bitOf(len: int, num: int) -> term.Term:
    inputT = WordType[len]
    outputT = type.BoolType
    return Const('bv_bitOf_%d' % num, TFun(inputT, outputT))

def bool_word1() -> term.Term:
    return Const("bool_word1", TFun(type.BoolType, WordType[1]))

def concat(len1: int, len2: int) -> term.Term:
    """concat function joins together two bitvectors."""
    inputT1 = WordType[len1]
    inputT2 = WordType[len2]
    outputT = WordType[len1 + len2]
    return Const('bv_concat', TFun(inputT1, inputT2, outputT))

def parse_binary(s: str) -> int:
    """Parse binary number in string format into integer."""
    res = 0
    for b in s:
        if b == '0':
            res = res * 2
        elif b == '1':
            res = res * 2 + 1
        else:
            assert False
    return res

def bvnot(len) -> term.Term:
    """not function on bitvectors."""
    argT = WordType[len]
    return Const('bvnot', TFun(argT, argT))

def bvxor(len) -> term.Term:
    """xor function on bitvectors."""
    argT = WordType[len]
    return Const('bvxor', TFun(argT, argT, argT))

def bvand(len) -> term.Term:
    """and function on bitvectors."""
    argT = WordType[len]
    return Const('bvand', TFun(argT, argT, argT))

def bvor(len) -> term.Term:
    """or function on bitvectors."""
    argT = WordType[len]
    return Const('bvor', TFun(argT, argT, argT))

def bvsdiv(len) -> term.Term:
    """signed division function on bitvectors."""
    argT = WordType[len]
    return Const('bvsdiv', TFun(argT, argT, argT))

def bvudiv(len) -> term.Term:
    """unsigned division function on bitvectors."""
    argT = WordType[len]
    return Const('bvudiv', TFun(argT, argT, argT))

def bvsmod(len) -> term.Term:
    """unsigned smod function on bitvectors."""
    argT = WordType[len]
    return Const('bvsmod', TFun(argT, argT, argT))

def bvurem(len) -> term.Term:
    """unsigned smod function on bitvectors."""
    argT = WordType[len]
    return Const('bvurem', TFun(argT, argT, argT))

def bvuge(len) -> term.Term:
    """unsigned ge function on bitvectors."""
    argT = WordType[len]
    return Const('bvuge', TFun(argT, argT, type.BoolType))

def bvule(len) -> term.Term:
    """unsigned le function on bitvectors."""
    argT = WordType[len]
    return Const('bvule', TFun(argT, argT, type.BoolType))

def bvlshr(len) -> term.Term:
    """unsigned lshr function on bitvectors."""
    argT = WordType[len]
    return Const('bvlshr', TFun(argT, argT, type.BoolType))

def bvshl(len) -> term.Term:
    """unsigned shl function on bitvectors."""
    argT = WordType[len]
    return Const('bvshl', TFun(argT, argT, type.BoolType))


# Conversions: compared to the case of natural numbers and real numbers,
# additional complexity come from the fact that theorems are parameterized
# by word length.

# Hence, we define conversions for each individual applications of theorems,
# that selects the appropriate theorem to use depending on word length.

class bv_add_comm(Conv):
    """Commutativity of addition."""
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_add_comm: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        pt = pt.on_rhs(rewr_conv("bv_add_comm_" + str(inputLen)))
        return pt
    
class bv_add_assoc(Conv):
    """Associativity of addition."""
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_add_comm: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        pt = pt.on_rhs(rewr_conv("bv_add_assoc_" + str(inputLen)))
        return pt
    
class bv_distrib_left(Conv):
    """Associativity of addition."""
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_distrib_left: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        pt = pt.on_rhs(rewr_conv("bv_distrib_left_" + str(inputLen)))
        return pt
    
class bv_multi_comm(Conv):
    """Commutativity of multiple."""
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_multi_comm: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        pt = pt.on_rhs(rewr_conv("bv_multi_comm_" + str(inputLen)))
        return pt
    
class bv_distrib_left2(Conv):
    """Associativity of addition."""
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_distrib_left2: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        pt = pt.on_rhs(rewr_conv("bv_distrib_left_" + str(inputLen)))
        pt = pt.on_rhs(arg_conv(rewr_conv("bv_multi_comm_" + str(inputLen))))
        pt = pt.on_rhs(arg1_conv(rewr_conv("bv_multi_comm_" + str(inputLen))))
        return pt

class bv_swap_mult_r(Conv):
    """Rewrite (a * b) * c to (a * c) * b."""
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_swap_mult_r: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        pt = pt.on_rhs(rewr_conv("bv_multi_assoc_" + str(inputLen)))
        pt = pt.on_rhs(arg_conv(rewr_conv("bv_multi_comm_" + str(inputLen))))
        pt = pt.on_rhs(rewr_conv("bv_multi_assoc_" + str(inputLen), sym = True))
        return pt
    
class bv_norm_mult_atom(Conv):
    """ """
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_norm_mult_atom: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        if t.arg1.is_times():
            cp = term_ord.fast_compare(t.arg1.arg, t.arg)
            if cp > 0:
                return pt.on_rhs(bv_swap_mult_r(), arg1_conv(self)) 
            else:
                return pt
        else:
            cp = term_ord.fast_compare(t.arg1, t.arg)
            if cp > 0:
                return pt.on_rhs(bv_multi_comm())
            else:
                return pt
            
class bv_norm_mult_monomial(Conv):
    """ """
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_norm_mult_monomial: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        if t.arg.is_times():
            return pt.on_rhs(rewr_conv("bv_multi_assoc_" + str(inputLen), sym=True),
                              arg1_conv(self), 
                              bv_norm_mult_atom())
        else:
            return pt.on_rhs(bv_norm_mult_atom())
        
class bv_swap_add_r(Conv):
    """Rewrite (a + b) + c to (a + c) + b."""
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_swap_add_r: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        return pt.on_rhs(rewr_conv("bv_add_assoc_" + str(inputLen)),
                                   arg_conv(rewr_conv("bv_add_comm_" + str(inputLen))),
                                   rewr_conv("bv_add_assoc_" + str(inputLen), sym=True)) 

class bv_norm_add_monomial(Conv): ###bugs
    """ """
    def get_proof_term(self, t: Term) -> ProofTerm:
        argT = t.get_type()
        assert is_word_type(argT), "bv_norm_add_monomial: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        if t.arg1.is_plus(): # (a + b) + c
            print("is_plus")
            cp = term_ord.fast_compare(t.arg1.arg, t.arg)  # compare b with c
            if cp > 0:  # if b > c, need to swap b with c
                # print("run to cp > 0")
                return pt.on_rhs(
                    bv_swap_add_r(), # (a + c) + b 
                    arg1_conv(self)) # possibly move c further into a
            # elif cp == 0:   # if b and c have the same body, combine coefficients
            #     print("run to cp == 0")
            #     return pt.on_rhs(
            #         rewr_conv('bv_add_assoc_' + str(inputLen)),  # a + (c1 * b + c2 * b)
            #         arg_conv(rewr_conv('bv_distrib_r_' + str(inputLen), sym=True)), # a + (c1 + c2) * b
            #         arg_conv(arg1_conv(nat.nat_conv())))  # evaluate c1 + c2
            else:   # if b < c, monomials are already sorted
                return pt
        else:   # a + b
            cp = term_ord.fast_compare(t.arg1, t.arg)  # compare a with b
            if cp > 0:  # if a > b, need to swap a with b
                return pt.on_rhs(rewr_conv('bv_add_comm_' + str(inputLen)))
            # elif cp == 0:  # if a and b have the same body, combine coefficients
            #     print("not plus, cp == 0")
            #     return pt
            #     if t.arg.is_number():
            #         print("t.arg is number")
            #         raise NotImplementedError
            #     else:
            #         print("t.arg is not number")
            #         if is_word_type(t.arg1.get_type):
            #             return pt.on_rhs(
            #                 rewr_conv("bv_distrib_r_" + str(inputLen), sym=True),
            #                 arg1_conv(nat.nat_conv()))
            #         else:
            #             # return pt.on_rhs(
            #             #     rewr_conv("bv_distrib_r_" + str(inputLen), sym=True),
            #             #     arg1_conv(nat.nat_conv()))
            #             raise NotImplementedError
            else:
                return pt

class bv_norm_add_polynomial(Conv):
    def get_proof_term(self, t):
        argT = t.get_type()
        assert is_word_type(argT), "bv_norm_add_monomial: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        if t.arg1.is_zero():
            return pt.on_rhs(rewr_conv('bv_nat_plus_def_1_' + str(inputLen)))
        elif t.arg.is_zero():
            return pt.on_rhs(rewr_conv('bv_add_0_right_' + str(inputLen)))
        elif t.arg.is_plus():  # t is of form a + (b + c)
            return pt.on_rhs(
                rewr_conv('bv_add_assoc_' + str(inputLen), sym=True),  # (a + b) + c
                arg1_conv(self),  # merge terms in b into a (0 + x) + y
                bv_norm_add_monomial())  # merge c into a + b
        else:
            return pt.on_rhs(bv_norm_add_monomial())
        
class bv_norm_mult_poly_monomial(Conv):
    """Multiply a polynomial a_1 + ... + a_n with a monomial c."""
    def get_proof_term(self, t):
        argT = t.get_type()
        assert is_word_type(argT), "bv_norm_add_monomial: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        if t.arg1.is_plus():  # (a + b) * c
            print(t)
            return pt.on_rhs(
                rewr_conv('bv_distrib_r_' + str(inputLen)),  # a * c + b * c
                arg1_conv(self),  # process a * c
                arg_conv(bv_norm_mult_monomial()), # process b * c
                bv_norm_add_polynomial())  # add the results
        else:
            return pt.on_rhs(bv_norm_mult_monomial())
        
class bv_norm_mult_polynomials(Conv):
    """Multiply two polynomials."""
    def get_proof_term(self, t):
        argT = t.get_type()
        assert is_word_type(argT), "bv_norm_add_monomial: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        if t.arg1.is_zero():
            return pt.on_rhs(rewr_conv('bv_nat_times_def_1_' + str(inputLen)))
        elif t.arg.is_zero():
            return pt.on_rhs(rewr_conv('bv_mult_0_right_' + str(inputLen)))
        elif t.arg.is_plus():  # a * (b + c)
            return pt.on_rhs(
                rewr_conv('bv_distrib_left_' + str(inputLen)), # a * b + a * c
                arg1_conv(self),  # process a * b
                arg_conv(bv_norm_mult_poly_monomial()),  # process a * c
                bv_norm_add_polynomial())
        else:
            return pt.on_rhs(bv_norm_mult_poly_monomial())
        
class bv_norm_full(Conv):
    def get_proof_term(self, t):
        argT = t.get_type()
        assert is_word_type(argT), "bv_norm_add_monomial: input is not valid word type"
        inputLen = WordTypeInv[argT]
        pt = refl(t)
        if t.is_plus():
            return pt.on_rhs(binop_conv(self), bv_norm_add_polynomial())
        elif t.is_times():
            return pt.on_rhs(binop_conv(self), bv_norm_mult_polynomials())
        elif t.is_number():
            return pt
        else:
            return pt
        # elif t.is_nat_power() and t.arg.is_number():  # rewrite x ^ n to 1 * x ^ n
        #     return pt.on_rhs(rewr_conv('bv_mult_1_left_', sym=True))
        # else:  # rewrite x to 1 * x ^ 1
        #     return pt.on_rhs(
        #         rewr_conv('bv_nat_power_1', sym=True),
        #         rewr_conv('bv_mult_1_left', sym=True))