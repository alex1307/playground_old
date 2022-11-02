use std::fmt::{Display, Formatter, Result as FmtResult};

use inner::IntoResult;
use serde::Serialize;
use serde_json::to_string_pretty;
use crate::cqrs_entity_errors::EntityError::{AlreadyExistsErr, NotFoundErr, InvalidDataErr};

#[derive(Debug, Serialize, PartialEq)]
#[allow(warnings)]
pub enum EntityError {
    AlreadyExistsErr(String),
    NotFoundErr(String),
    InvalidDataErr(String)
}

impl IntoResult<String, ()> for EntityError {
    fn into_result(self) -> Result<String, ()> {
        match self {
            AlreadyExistsErr(msg) => Ok(msg),
            NotFoundErr(msg) => Ok(msg),
            InvalidDataErr(msg) => Ok(msg)
        }
    }
}

impl Display for EntityError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", to_string_pretty(self).unwrap())
    }
}