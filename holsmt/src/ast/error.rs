use pyo3::exceptions::PyOSError;
use pyo3::prelude::*;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("expected a fun type")]
    NotFun,

    // todo
    #[error("convert_stvar")]
    ConvertSTVar,

    #[error("default error")]
    Default,

    #[error("{0}")]
    MatchError(String),

    #[error("{0}")]
    Custom(String),
}

impl std::convert::From<TypeError> for PyErr {
    fn from(err: TypeError) -> PyErr {
        PyOSError::new_err(err.to_string())
    }
}
