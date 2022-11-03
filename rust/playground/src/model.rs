use serde::{Deserialize, Serialize};

use crate::{
    MONITORING_ARR, MONITORING_HEADER, REQUEST_ARR, REQUEST_HEADER, RESPONSE_ARR, RESPONSE_HEADER,
};

pub type ProcessResult<T> = std::result::Result<T, ProcessError>;
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProcessError {
    pub reason: Option<String>,
    pub error_type: ProcessErrorType,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ProcessErrorType {
    Continue,
    Break,
    Fatal,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TaskExecutionStatus {
    InProgress(String),
    Ok(String),
    Error(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SystemCommand {
    Ping,
    Pong,
    Ack,
    NoAck,
    HealthCheck,
    Ok,
    Err,
    Seq(u32),
    Async,
    Sync,
}

pub enum Header {
    RequestCommand(ServerCommand),
    ResponseCommand(ServerResponse),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServerCommand {
    Ping,
    Ack(u32),
    Status,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Command<T> {
    Get(T),
    Execute(T),
    Cancel(T),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExecutionStatus<T> {
    NotFound(T),
    InProgress(T),
    Ok(T),
    Error(T),
    Aborted(T),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServerResponse {
    Pong,
    Running,
    Busy,
    Down,
    Ok,
    Error,
    Seq(u32),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    pub from: Vec<String>,
    pub road_map: Vec<String>,
    pub payload: Option<Vec<u8>>,
    pub correlation_id: Option<String>,
    pub internal_command: Option<ServerCommand>,
    pub command: Option<Command<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub from: Vec<String>,
    pub payload: Option<Vec<u8>>,
    pub correlation_id: Option<String>,
    pub server_response: Option<ServerResponse>,
    pub status: Option<ExecutionStatus<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemMessage {
    pub from: String,
    pub payload: Option<Vec<u8>>,
    pub correlation_id: Option<String>,
    pub command: Option<SystemCommand>,
}

#[derive(Debug, Clone)]
pub struct CommandHandler {
    pub(crate) sender: crossbeam::channel::Sender<Vec<u8>>,
}

impl CommandHandler {
    pub fn send(&self, message: Vec<u8>) {
        let _ = self.sender.try_send(message);
    }
}

pub struct Server<Req, Res> {
    pub handle_request: fn(from: &str, Req),
    pub handle_response: fn(Res),
    pub handler_status: fn(ServerCommand) -> ProcessResult<ServerResponse>,
}

#[derive(PartialEq, Debug)]
pub enum MessageType {
    WorkerResponse(Vec<u8>),
    Monitoring(Vec<u8>),
    ClientRequest(Vec<u8>),
    Error,
}

impl MessageType {
    pub fn new_monitoring_message(binary: Vec<u8>) -> Self {
        Self::new(binary, 1)
    }

    pub fn new_request(binary: Vec<u8>) -> Self {
        Self::new(binary, 2)
    }

    pub fn new_response(binary: Vec<u8>) -> Self {
        Self::new(binary, 3)
    }

    fn new(binary: Vec<u8>, enum_type: u8) -> Self {
        match enum_type {
            1 => MessageType::Monitoring(binary),
            2 => MessageType::ClientRequest(binary),
            3 => MessageType::WorkerResponse(binary),
            _ => MessageType::Error,
        }
    }

    pub fn parse(binary: &Vec<u8>) -> Self {
        if binary.len() < 3 {
            return MessageType::Error;
        }

        let prefix = &binary[0..3];

        if prefix == &MONITORING_ARR {
            MessageType::Monitoring(Vec::from(&binary[3..]))
        } else if prefix == &REQUEST_ARR {
            MessageType::ClientRequest(Vec::from(&binary[3..]))
        } else if prefix == &RESPONSE_ARR {
            MessageType::WorkerResponse(Vec::from(&binary[3..]))
        } else {
            MessageType::Error
        }
    }

    pub fn to_binary(&self) -> Vec<u8> {
        let (mut result, mut binary) = match self {
            MessageType::ClientRequest(source) => (REQUEST_HEADER.clone(), source.clone()),
            MessageType::WorkerResponse(source) => (RESPONSE_HEADER.clone(), source.clone()),
            MessageType::Monitoring(source) => (MONITORING_HEADER.clone(), source.clone()),
            _ => return vec![],
        };
        result.append(&mut binary);
        result
    }
}
