"""Utility functions for bitvectors."""

from kernel import type
from kernel.type import TFun, TConst
from kernel import term
from kernel.term import Const

# List of currently allowed word lengths
allowed_lengths = tuple(range(1, 129))

# Definitions of types of bitvectors
WordType = dict()
WordTypeInv = dict()
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

def bvuge(len) -> term.Term:
    """unsigned ge function on bitvectors."""
    argT = WordType[len]
    return Const('bvuge', TFun(argT, argT, type.BoolType))

def bvule(len) -> term.Term:
    """unsigned le function on bitvectors."""
    argT = WordType[len]
    return Const('bvule', TFun(argT, argT, type.BoolType))
