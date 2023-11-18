use crate::ast::error::HolrsError;
use pyo3::create_exception;
use pyo3::exceptions::{PyException, PyRuntimeError};
use pyo3::prelude::*;
use std::fmt::{Debug, Formatter};
use thiserror::Error;

#[derive(Error)]
pub enum HolPyErr {
    #[error(transparent)]
    HolPy(#[from] HolrsError),
    #[error("{0}")]
    Other(String),
}

impl std::convert::From<HolPyErr> for PyErr {
    fn from(err: HolPyErr) -> PyErr {
        let default = || PyRuntimeError::new_err(format!("{:?}", &err));

        use HolPyErr::*;
        match &err {
            HolPy(err) => match err {
                HolrsError::TypeError(err) => TypeException::new_err(err.to_string()),
                HolrsError::TypeMatchError(err) => TypeMatchException::new_err(err.to_string()),
                HolrsError::TermError(err) => TermException::new_err(err.to_string()),
                HolrsError::TypeCheckError(err) => TypeCheckException::new_err(err.to_string()),
            },
            _ => default(),
        }
    }
}

impl Debug for HolPyErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use HolPyErr::*;
        match self {
            HolPy(err) => write!(f, "{err:?}"),
            Other(err) => write!(f, "Other: {err:?}"),
        }
    }
}

create_exception!(exceptions, TypeException, PyException);
create_exception!(exceptions, TypeMatchException, PyException);

create_exception!(exceptions, TermException, PyException);
create_exception!(exceptions, TypeCheckException, PyException);

create_exception!(exceptions, InvalidDerivationException, PyException);

create_exception!(exceptions, TheoryException, PyException);
create_exception!(exceptions, CheckProofException, PyException);
create_exception!(exceptions, ParameterQueryException, PyException);

create_exception!(exceptions, TacticException, PyException);
