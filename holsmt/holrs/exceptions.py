try:
    from holrs import (
        TypeException,
        TypeMatchException,
        TermException,
        TypeCheckException,
        InvalidDerivationException,
        TheoryException,
        CheckProofException,
        ParameterQueryException,
        TacticException,
    )
except ImportError:

    class TypeMatchException(Exception):
        """todo"""

    class TypeException(Exception):
        """todo"""

    class TermException(Exception):
        """todo"""

    class TypeCheckException(Exception):
        """todo"""

    class InvalidDerivationException(Exception):
        """todo"""

    class TheoryException(Exception):
        """todo"""

    class CheckProofException(Exception):
        """todo"""

    class ParameterQueryException(Exception):
        """todo"""

    class TacticException(Exception):
        """todo"""