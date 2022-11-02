use std::fmt::{Display, Formatter, Result as FmtResult};

use inner::IntoResult;
use serde::Serialize;
use serde_json::{Error, to_string_pretty};
use crate::errors::MapperError::AvroErr;

#[derive(Debug, Serialize)]
#[allow(warnings)]
pub enum MapperError {
    AvroErr(String),
    JsonParsingErr(String),
    FileErr(String)
}

impl IntoResult<String, ()> for MapperError {
    fn into_result(self) -> Result<String, ()> {
        use crate::errors::MapperError::{FileErr, JsonParsingErr};
        match self {

            JsonParsingErr(msg) => Ok(msg),
            AvroErr(msg) => Ok(msg),
            FileErr(msg) => Ok(msg)
        }
    }
}

impl Display for MapperError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", to_string_pretty(self).unwrap())
    }
}

impl From<serde_json::Error> for MapperError {
    fn from(source: Error) -> Self {
        println!("DEBUG: {} {}", source.line(), source.to_string());
        MapperError::JsonParsingErr(source.to_string())
    }
}

impl From<std::io::Error> for MapperError {
    fn from(source: std::io::Error) -> Self {
        MapperError::FileErr(source.to_string())
    }
}

