pub mod model;
use std::{
    collections::HashMap,
    string::FromUtf8Error,
    sync::Mutex,
    thread::{sleep, spawn},
    time::Duration,
};
pub const MONITORING_ARR: [u8; 3] = [0x02, b's', 0x03];
pub const REQUEST_ARR: [u8; 3] = [0x02, b'q', 0x03];
pub const RESPONSE_ARR: [u8; 3] = [0x02, b'r', 0x03];

#[macro_use]
extern crate lazy_static;
lazy_static! {
    static ref REQUEST_HEADER: Vec<u8> = REQUEST_ARR.to_vec();
    static ref RESPONSE_HEADER: Vec<u8> = RESPONSE_ARR.to_vec();
    static ref MONITORING_HEADER: Vec<u8> = MONITORING_ARR.to_vec();
}

use log::{error, info, warn};
use log4rs::init_file;
use model::{
    Command, ExecutionStatus, ProcessResult, Response, Server, ServerCommand, ServerResponse,
    SystemCommand, TaskExecutionStatus,
};
use serde::{de::DeserializeOwned, Serialize};

use crate::model::{
    CommandHandler, MessageType, ProcessError, ProcessErrorType, Request, SystemMessage,
};

lazy_static! {
    static ref ACTORS: Mutex<HashMap<String, CommandHandler>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}



pub fn to_binary<T: Serialize>(source: T) -> ProcessResult<Vec<u8>> {
    let json = serde_json::to_string(&source)?;
    Ok(Vec::from(json.as_bytes()))
}

pub fn from_binary<T>(binary: Vec<u8>) -> ProcessResult<T>
where
    T: ?Sized + DeserializeOwned,
{
    if binary.is_empty() {
        error!("binary.empty");
        return Err(ProcessError {
            reason: Some("binary.empty".to_string()),
            error_type: ProcessErrorType::Continue,
        });
    }

    let string = match String::from_utf8(binary) {
        Ok(utf8) => utf8,
        Err(err) => {
            error!("failed.parsing.binary => Err: {}", err);
            return Err(ProcessError {
                reason: Some("utf8_error:invalid.binary".to_string()),
                error_type: ProcessErrorType::Continue,
            });
        }
    };
    Ok(serde_json::from_str::<T>(&string)?)
}

pub fn configure_log4rs() {
    init_file("config/logs/log4rs.yml", Default::default()).unwrap();
    // let stdout = ConsoleAppender::builder()
    //     .encoder(Box::new(PatternEncoder::new(r#"{h(==> {d} == {l} == - [ {I} {T}:{M}]:{L} - {m} {n} )}"#)))
    //     .build();
    // let config = Config::builder()
    //     .appender(Appender::builder().build("stdout", Box::new(stdout)))
    //     .build(Root::builder().appender("stdout").build(LevelFilter::Trace))
    //     .unwrap();
    // let _handle = log4rs::init_config(config).unwrap();
    warn!(r#"SUCCESS: Loggers are configured with dir: _log/*"#);
}

pub fn send_client_request(from: Vec<String>, server_name: &String, message: Vec<u8>) {
    info!("sending 1");
    let actors = ACTORS.lock().unwrap();
    info!("sending 2");
    let destination = actors.get(server_name);
    if destination.is_some() {
        info!("sending ....");
        let correlation_id = "123".to_string();
        let request = Request {
            from,
            road_map: vec![],
            payload: Some(message),
            correlation_id: Some(correlation_id.clone()),
            internal_command: Some(ServerCommand::Ack(101)),
            command: Some(Command::Execute(correlation_id)),
        };
        let bin_req = Vec::from(serde_json::to_string(&request).unwrap().as_bytes());
        let mt = MessageType::new_request(bin_req);
        let client_request = mt.to_binary();
        info!("sending {:?} to {}", client_request, server_name);
        destination.unwrap().send(client_request);
    } else {
        error!("server {} not found", server_name);
    }
}

pub fn start_server(server_name: String) {
    let (tx, rx) = crossbeam::channel::unbounded();
    let mut actors = ACTORS.lock().unwrap();
    actors.insert(server_name.clone(), CommandHandler { sender: tx.clone() });
    // let _t = spawn(move || server(server_name, rx, tx.clone()));
    let server = Server::<Request, Response> {
        handle_request: process_request,
        handle_response: process_response,
        handler_status: handle_srv_command,
    };
    let _t = spawn(move || run_server(server_name, rx, tx.clone(), server));
}

pub fn process_request(from: &str, req: Request) {
    info!("#1 process server request");
    spawn(move || test(Box::new("done".to_string())));

    let response = Response {
        from: req.from,
        payload: None,
        correlation_id: None,
        server_response: None,
        status: Some(ExecutionStatus::Ok("done".to_string())),
    };

    let actors = ACTORS.lock().unwrap();
    let destination = actors.get(from);
    if destination.is_some() {
        let handler = destination.unwrap();
        let message = MessageType::new_response(
            serde_json::to_string(&response)
                .unwrap()
                .as_bytes()
                .to_vec(),
        );
        handler.send(message.to_binary());
    }
}

pub fn process_response(_response: Response) {
    info!("#2 process server response");
}

pub fn handle_srv_command(_cmd: ServerCommand) -> ProcessResult<ServerResponse> {
    info!("#INFO server command");
    Ok(ServerResponse::Ok)
}

pub fn run_server(
    server_name: String,
    receiver: crossbeam::channel::Receiver<Vec<u8>>,
    sender: crossbeam::channel::Sender<Vec<u8>>,
    server: Server<Request, Response>,
) {
    loop {
        info!("looping");
        if let Ok(bin) = receiver.recv_timeout(Duration::from_secs(1)) {
            let message = MessageType::parse(&bin);
            info!("SRV[{}] => Message: {:?}", &server_name, message);
            match message {
                MessageType::ClientRequest(source) => {
                    let request = from_binary::<Request>(source).unwrap();
                    let sc = server_name.clone();
                    let lr = request.clone();

                    let _ = spawn(move || (server.handle_request)(&sc.clone(), request.clone()));
                    if lr.internal_command.is_some() {
                        let bin_cmd = to_binary(lr.internal_command.unwrap()).unwrap();
                        let mt = MessageType::new_monitoring_message(bin_cmd);
                        let _ = sender.try_send(mt.to_binary());
                    }
                    continue;
                }
                MessageType::WorkerResponse(source) => {
                    let mut response = from_binary::<Response>(source).unwrap();
                    let v = response.from.pop();
                    if v.is_some() {
                        /*
                            Forward the response to the next recipient.
                        */
                        let recipient = v.unwrap();
                        info!("ATT! sending to {}", recipient);
                        let bin = serde_json::to_string(&response)
                            .unwrap()
                            .as_bytes()
                            .to_vec();
                        info!("RESPONSE: {:?}", &response);
                        let message = MessageType::new_response(bin);
                        let actors = ACTORS.lock().unwrap();
                        let handler = actors.get(&recipient);
                        if handler.is_some() {
                            let _ = handler.unwrap().send(message.to_binary());
                        }
                    } else {
                        (server.handle_response)(response);
                        continue;
                    };
                }
                MessageType::Monitoring(binary) => {
                    let cmd = from_binary::<ServerCommand>(binary);
                    let _status = match cmd {
                        Ok(command) => (server.handler_status)(command),
                        Err(_err) => continue,
                    };

                    continue;
                }
                _ => {
                    error!(
                        "{} -> Client request must be provided {:?}",
                        &server_name, bin
                    );
                    continue;
                }
            };
        } else {
            info!("server [{}] is awaiting for something", &server_name);
            continue;
        }
    }
}

pub fn exchange_command(from: String, to: String, command: SystemCommand) {
    let actors = ACTORS.lock().unwrap();
    let destination = actors.get(&to);
    if destination.is_some() {
        let handler = destination.unwrap();
        let system_message = SystemMessage {
            from: from.clone(),
            payload: None,
            correlation_id: None,
            command: Some(command.clone()),
        };

        let result = serde_json::to_string(&system_message);
        let binary = Vec::from(result.unwrap().as_bytes());
        let _ = handler.send(binary);
    }
}

pub fn exchange_message(from: String, to: String, message: String, correlation_id: Option<String>) {
    let actors = ACTORS.lock().unwrap();
    let destination = actors.get(&to);
    if destination.is_some() {
        let handler = destination.unwrap();
        let system_message = SystemMessage {
            from: from.clone(),
            payload: Some(Vec::from(message.as_bytes())),
            correlation_id,
            command: None,
        };

        let result = serde_json::to_string(&system_message);
        let binary = Vec::from(result.unwrap().as_bytes());
        info!("sending message: {:?}", system_message);
        let _ = handler.send(binary);
    }
}

fn execute<T: Sized>(
    f: &dyn Fn(Box<T>) -> ProcessResult<()>,
    param: Box<T>,
    sender: crossbeam::channel::Sender<Vec<u8>>,
) {
    let c_id = "123".to_string();
    send_message(&TaskExecutionStatus::InProgress(c_id.clone()), &sender);
    match f(param) {
        Ok(_) => send_message(&TaskExecutionStatus::Ok(c_id.clone()), &sender),
        Err(_) => send_message(&TaskExecutionStatus::Error(c_id), &sender),
    };
}

fn send_message(status: &TaskExecutionStatus, sender: &crossbeam::channel::Sender<Vec<u8>>) {
    let in_progress = serde_json::to_string(status);
    let msg = Vec::from(in_progress.unwrap().as_bytes());
    let _ = sender.try_send(msg);
}

fn test(_source: Box<String>) -> ProcessResult<()> {
    info!("let sleep for 5 sec");
    sleep(Duration::from_secs(5));
    info!("DONE. Lets finish....");
    Ok(())
}

impl From<FromUtf8Error> for ProcessError {
    fn from(_err: FromUtf8Error) -> Self {
        ProcessError {
            reason: Some("utf8_error".to_string()),
            error_type: ProcessErrorType::Continue,
        }
    }
}

impl From<serde_json::Error> for ProcessError {
    fn from(_err: serde_json::Error) -> Self {
        ProcessError {
            reason: Some("parsing.error".to_string()),
            error_type: ProcessErrorType::Continue,
        }
    }
}

#[cfg(test)]
mod unit_tests {
    use crate::MessageType;

    fn process_binary(binary: &Vec<u8>) -> MessageType {
        if binary.len() < 3 {
            return MessageType::Error;
        }

        let prefix = &binary[0..3];
        match prefix {
            [2, 119, 3] => MessageType::WorkerResponse(Vec::from(&binary[3..])),
            [2, 99, 3] => MessageType::ClientRequest(Vec::from(&binary[3..])),
            _ => MessageType::Error,
        }
    }

    #[test]
    fn match_binaries_test() {
        let worker = [0x02, b'w', 0x03, b'a', b'l', b'e', b'x'];
        let client = [0x02, b'c', 0x03, b'a', b'l', b'e', b'x'];
        let empty = [];
        let any = [99, 100, 101, 102];
        let w = process_binary(&Vec::from(worker));
        let c = process_binary(&Vec::from(client));
        let e = process_binary(&Vec::from(empty));
        let a = process_binary(&Vec::from(any));

        assert_eq!(MessageType::WorkerResponse(Vec::from("alex".as_bytes())), w);
        assert_eq!(MessageType::ClientRequest(Vec::from("alex".as_bytes())), c);
        assert_eq!(MessageType::Error, e);
        assert_eq!(MessageType::Error, a);

        let v = vec![
            2, 99, 3, 123, 34, 102, 114, 111, 109, 34, 58, 34, 99, 108, 105, 101, 110, 116, 49, 34,
            44, 34, 112, 97, 121, 108, 111, 97, 100, 34, 58, 91, 49, 49, 53, 44, 49, 49, 49, 44,
            49, 48, 57, 44, 49, 48, 49, 44, 51, 50, 44, 49, 49, 53, 44, 49, 49, 54, 44, 49, 49, 52,
            44, 49, 48, 53, 44, 49, 49, 48, 44, 49, 48, 51, 93, 44, 34, 99, 111, 114, 114, 101,
            108, 97, 116, 105, 111, 110, 95, 105, 100, 34, 58, 34, 49, 50, 51, 34, 44, 34, 105,
            110, 116, 101, 114, 110, 97, 108, 95, 99, 111, 109, 109, 97, 110, 100, 34, 58, 123, 34,
            65, 99, 107, 34, 58, 49, 48, 49, 125, 44, 34, 99, 111, 109, 109, 97, 110, 100, 34, 58,
            123, 34, 69, 120, 101, 99, 117, 116, 101, 34, 58, 34, 49, 50, 51, 34, 125, 125,
        ];
        let r = process_binary(&v);
        assert_eq!(
            MessageType::ClientRequest(vec![
                123, 34, 102, 114, 111, 109, 34, 58, 34, 99, 108, 105, 101, 110, 116, 49, 34, 44,
                34, 112, 97, 121, 108, 111, 97, 100, 34, 58, 91, 49, 49, 53, 44, 49, 49, 49, 44,
                49, 48, 57, 44, 49, 48, 49, 44, 51, 50, 44, 49, 49, 53, 44, 49, 49, 54, 44, 49, 49,
                52, 44, 49, 48, 53, 44, 49, 49, 48, 44, 49, 48, 51, 93, 44, 34, 99, 111, 114, 114,
                101, 108, 97, 116, 105, 111, 110, 95, 105, 100, 34, 58, 34, 49, 50, 51, 34, 44, 34,
                105, 110, 116, 101, 114, 110, 97, 108, 95, 99, 111, 109, 109, 97, 110, 100, 34, 58,
                123, 34, 65, 99, 107, 34, 58, 49, 48, 49, 125, 44, 34, 99, 111, 109, 109, 97, 110,
                100, 34, 58, 123, 34, 69, 120, 101, 99, 117, 116, 101, 34, 58, 34, 49, 50, 51, 34,
                125, 125,
            ]),
            r
        );
    }
}
