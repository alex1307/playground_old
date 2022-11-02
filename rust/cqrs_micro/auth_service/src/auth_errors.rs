use std::fmt::{Display, Formatter, Result as FmtResult};

use inner::IntoResult;
use serde::Serialize;
use serde_json::to_string_pretty;

use crate::auth_errors::AuthError::{TokenInvalidErr, TokenExpiredErr, Forbidden, Unauthorized, ServiceErr};

#[derive(Debug, Serialize, PartialEq)]
#[allow(warnings)]
pub enum AuthError {
    TokenInvalidErr(String),
    TokenExpiredErr(String),
    Unauthorized(String),
    Forbidden(String),
    ServiceErr(String),
}

impl IntoResult<String, ()> for AuthError {
    fn into_result(self) -> Result<String, ()> {
        match self {
            TokenInvalidErr(msg) => Ok(msg),
            TokenExpiredErr(msg) => Ok(msg),
            Forbidden(msg) => Ok(msg),
            Unauthorized(msg) => Ok(msg),
            ServiceErr(msg) => Ok(msg)
        }
    }
}

impl Display for AuthError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", to_string_pretty(self).unwrap())
    }
}