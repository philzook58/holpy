"""Utility functions for bitvectors."""

from kernel import type
from kernel.type import TFun, TConst
from kernel import term
from kernel.term import Const

# List of currently allowed word lengths
allowed_lengths = (1, 8, 16, 32, 64)

# Definitions of types of bitvectors

Word1Type = TConst('word1')
Word8Type = TConst('word8')
Word16Type = TConst('word16')
Word32Type = TConst('word32')
Word64Type = TConst('word64')

WordType = {
    1: Word1Type,
    8: Word8Type,
    16: Word16Type,
    32: Word32Type,
    64: Word64Type
}

WordTypeInv = {
    Word1Type: 1,
    Word8Type: 8,
    Word16Type: 16,
    Word32Type: 32,
    Word64Type: 64
}

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
    return Const('zero_extend', TFun(inputT, outputT))

def extract(len: int, start: int, end: int) -> term.Term:
    """extract function maps bitvector of length len to bitvector
    of length end - start + 1, taking the sub-vector with given starting
    and ending indices.
    
    """
    inputT = WordType[len]
    outputT = WordType[end - start + 1]
    return Const('extract_%d' % start, TFun(inputT, outputT))

def parse_binary(s: str) -> int:
    res = 0
    for b in s:
        if b == '0':
            res = res * 2
        elif b == '1':
            res = res * 2 + 1
        else:
            assert False
    return res

        