use std::fmt::{Display, Formatter, Result as FmtResult};

use inner::IntoResult;
use serde::Serialize;
use serde_json::{Error, to_string_pretty};

#[derive(Debug, Serialize)]
#[allow(warnings)]
pub enum ServiceErrors {
    SendErr(String)
}

impl IntoResult<String, ()> for ServiceErrors {
    fn into_result(self) -> Result<String, ()> {
        use crate::errors::ServiceErrors::{SendErr};
        match self {
            SendErr(msg) => Ok(msg)
        }
    }
}

impl Display for ServiceErrors {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", to_string_pretty(self).unwrap())
    }
}

