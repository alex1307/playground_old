use actix_web::{get, HttpResponse, post, web};
use actix_web::web::{get, Buf};
use auth_model::user_repository::{create, get as get_user};
use auth_model::auth_model::{RegisterDTO, LoginDTO, UserToken};
use domain_model::message::Message;
use auth_service::auth_service::AuthorizationService;
use command_service::send_message_service::send_message;
use mapper::mapper::AvroM;
use std::collections::HashMap;

#[post("/")]
async fn register(register: web::Json<RegisterDTO>) -> HttpResponse {
    let inner = register.into_inner();
    create(inner.email.clone(),inner.clone());
    println!("user has been registered: {}", inner.clone().email.clone());
    HttpResponse::Accepted().body("success")
}

#[get("/{email}")]
async fn find_user(path: web::Path<String>) -> HttpResponse {
    let email = path.into_inner();
    let found = get_user(email.clone().as_str()).unwrap().clone();
    HttpResponse::Ok().json(found)
}

#[post("/login")]
async fn login(user: web::Json<LoginDTO>) -> HttpResponse {
    let token = UserToken::login(&user.into_inner()).unwrap();
    let mut response = HashMap::new();
    response.insert("access_token", token.encode().unwrap());
    HttpResponse::Ok().json(&response)
}

#[post("/message")]
async fn message(message: web::Bytes) -> HttpResponse {
//    let uuid = message_service_kafka::service::send_message(&message.into_inner()).unwrap();
    let response = serde_json::from_slice::<Message>(message.bytes()).unwrap();
    let uuid = response.uuid.clone();
    let binary = Message::to_binary(&response).unwrap();
//    println!("payload: {:?}", message);
//    let uuid = message_service_kafka::service::send_message(&response.unwrap());
    send_message("messages", 0, &binary, &vec![1,2, 3]);
    HttpResponse::Ok().body(uuid)
}

pub fn configure(cfg: &mut web::ServiceConfig) {
    cfg.service(register);
    cfg.service(find_user);
    cfg.service(login);
    cfg.service(message);
}