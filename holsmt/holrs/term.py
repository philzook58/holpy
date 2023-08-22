from holrs._reexport import *
from holrs.other_python_file import *

from typing import TYPE_CHECKING, Union

def get_svars(t: Union[Term, list[Term]]) -> list[Term]:
    """Returns list of schematic variables in a term or a list of terms."""
    if isinstance(t, Term):
        return t.get_svars()
    elif isinstance(t, list):
        found = set()
        res = []
        for s in t:
            for svar in s.get_svars():
                if svar not in found:
                    res.append(svar)
                    found.add(svar)
        return res
    else:
        raise TypeError

def get_vars(t: Union[Term, list[Term]]) -> list[Term]:
    """Returns list of variables in a term or a list of terms."""
    if isinstance(t, Term):
        return t.get_vars()
    elif isinstance(t, list):
        found = set()
        res = []
        for s in t:
            for var in s.get_vars():
                if var not in found:
                    res.append(var)
                    found.add(var)
        return res
    else:
        raise TypeError

def get_stvars(t: Union[Term, list[Term]]) -> list[Term]:
    """Get the list of type variables for a term."""
    if isinstance(t, Term):
        return t.get_stvars()
    elif isinstance(t, list):
        found = set()
        res = []
        for s in t:
            for stvar in s.get_stvars():
                if stvar not in found:
                    res.append(stvar)
                    found.add(stvar)
        return res
    else:
        raise TypeError