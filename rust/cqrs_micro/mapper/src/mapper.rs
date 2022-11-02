use lazy_static::lazy_static;
use std::fs::File;
use std::io::{BufReader, Cursor, Read};
use serde::de::DeserializeOwned;
use serde::Serialize;
use avro_rs::{Schema, Writer, to_value, from_avro_datum, to_avro_datum, Reader};
use crate::errors::MapperError;
use domain_model::message::{Message, Contact, Attachment};
use avro_rs::types::Value;

lazy_static! {
    pub static ref MESSAGE_SCHEMA_FILE: String = schema("_config_files/resources/message.avsc");
    pub static ref ATTACHMENT_SCHEMA_FILE: String = schema("_config_files/resources/attachment.avsc");
    pub static ref CONTACT_SCHEMA_FILE: String = schema("_config_files/resources/contact.avsc");

    pub static ref SCHEMAS: (Schema, Schema, Schema) =  {
        let schema_files: [&str; 3] = [
            ATTACHMENT_SCHEMA_FILE.as_str(),
            CONTACT_SCHEMA_FILE.as_str(),
            MESSAGE_SCHEMA_FILE.as_str()
        ];
        let mut schemas = Schema::parse_list(&schema_files).unwrap();
        (schemas.pop().unwrap(),schemas.pop().unwrap(),schemas.pop().unwrap())
    };

    pub static ref MESSAGE_SCHEMA: &'static Schema = {
        let tuple: &(Schema, Schema, Schema) = &SCHEMAS;
        let (message_schema, _, _) = tuple;
        message_schema
    };


}

pub fn flatten<T>(nested: Vec<Vec<T>>) -> Vec<T> {
    nested.into_iter().flatten().collect()
}

pub fn write_to_batch(messages: Vec<Message>) -> Result<Vec<u8>, MapperError> {
    let mut writer = Writer::new(&MESSAGE_SCHEMA, Vec::new());
    writer.extend_ser(messages);
    let bytes = writer.into_inner().unwrap();
    Ok(bytes)
}

pub fn read_from_batch(binaries: Vec<u8>) -> Result<Vec<Value>, MapperError> {
    let res = Reader::with_schema(&MESSAGE_SCHEMA, &binaries[..]).unwrap();
    let mut messages = Vec::with_capacity(1_000_000);
    for record in res {
        messages.push(record.unwrap());
    }
    Ok(messages)
}

pub fn schema(file_name: &str) -> String {
    let file = File::open(file_name).unwrap();
    let mut buf_reader = BufReader::new(file);
    let mut contents = String::new();
    buf_reader.read_to_string(&mut contents).unwrap();
    contents
}


pub trait  AvroM <'a, T: Serialize + DeserializeOwned> {
    fn schema() -> &'static Schema;

    fn from_binary(binary: &Vec<u8>) ->  Result<T, MapperError> {
        let msg = from_avro_datum(Self::schema(), &mut Cursor::new(binary), None).unwrap();
        Ok(avro_rs::from_value(&msg).unwrap())
    }

    fn to_binary(source: &T) -> Result<Vec<u8>, MapperError> {
        let result = to_value(source);
        match result {
            Ok(v) => {
                Ok(to_avro_datum(Self::schema(), v).unwrap())
            },
            Err(_err) => Err(MapperError::AvroErr("invalid".to_string()))
        }
    }
}

impl AvroM <'_,Message> for Message {
    fn schema() -> &'static Schema {
        let tuple: &(Schema, Schema, Schema) = &SCHEMAS;
        let (message_schema, _contact_schema, _attachment_schema) = tuple;
        message_schema
    }
}

impl AvroM <'_,Contact> for Contact {
    fn schema() -> &'static Schema {
        let tuple: &(Schema, Schema, Schema) = &SCHEMAS;
        let (_message_schema, contact_schema, _attachment_schema) = tuple;
        contact_schema
    }
}

impl AvroM <'_,Attachment> for Attachment {
    fn schema() -> &'static Schema {
        let tuple: &(Schema, Schema, Schema) = &SCHEMAS;
        let (_message_schema, _contact_schema, attachment_schema) = tuple;
        attachment_schema
    }
}
