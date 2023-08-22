use std::fmt::{Debug, Formatter};
use std::io::Error;

use pyo3::create_exception;
use pyo3::exceptions::{PyException, PyIOError, PyRuntimeError};
use pyo3::prelude::*;
use thiserror::Error;

create_exception!(exceptions, TypeException, PyException);
create_exception!(exceptions, TypeMatchException, PyException);

create_exception!(exceptions, TermException, PyException);
create_exception!(exceptions, TypeCheckException, PyException);

create_exception!(exceptions, InvalidDerivationException, PyException);

create_exception!(exceptions, TheoryException, PyException);
create_exception!(exceptions, CheckProofException, PyException);
create_exception!(exceptions, ParameterQueryException, PyException);

create_exception!(exceptions, TacticException, PyException);
