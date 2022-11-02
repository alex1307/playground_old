use actix_web::{get, HttpResponse, post, web};
use actix_web::web::{get, Buf};
use auth_model::user_repository::{create, get as get_user};
use auth_model::auth_model::{RegisterDTO, LoginDTO, UserToken};
use domain_model::message::Message;
use auth_service::auth_service::AuthorizationService;
use mapper::mapper::AvroM;
use std::collections::HashMap;


#[get("/user/{email}")]
async fn find_user(path: web::Path<String>) -> HttpResponse {
    let email = path.into_inner();
    let found = get_user(email.clone().as_str()).unwrap().clone();
    HttpResponse::Ok().json(found)
}


#[post("/message")]
async fn message(message: web::Bytes) -> HttpResponse {
//    let uuid = message_service_kafka::service::send_message(&message.into_inner()).unwrap();
    let response = serde_json::from_slice::<Message>(message.bytes()).unwrap();
    let uuid = response.uuid.clone();
    let _binary = Message::to_binary(&response).unwrap();
//    println!("payload: {:?}", message);
//    let uuid = message_service_kafka::service::send_message(&response.unwrap());

    HttpResponse::Ok().body(uuid)
}

pub fn configure(cfg: &mut web::ServiceConfig) {
    cfg.service(find_user);
    cfg.service(message);
}