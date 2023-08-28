use thiserror::Error;

#[derive(Debug, Error)]
pub enum HolrsError {
    #[error("type error: {0}")]
    TypeError(String),

    #[error("type match error: {0}")]
    TypeMatchError(String),

    #[error("term error: {0}")]
    TermError(String),

    #[error("type check error: {0}")]
    TypeCheckError(String),
}

pub type HolrsResult<T> = Result<T, HolrsError>;
