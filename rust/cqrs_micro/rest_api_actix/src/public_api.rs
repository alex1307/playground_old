use actix_web::{get, HttpResponse, post, web};
use actix_web::web::{get, Buf};
use auth_model::user_repository::{create, get as get_user};
use auth_model::auth_model::{RegisterDTO, LoginDTO, UserToken};
use domain_model::message::Message;
use auth_service::auth_service::AuthorizationService;
use mapper::mapper::AvroM;
use std::collections::HashMap;

#[post("/register")]
async fn register(register: web::Json<RegisterDTO>) -> HttpResponse {
    let inner = register.into_inner();
    create(inner.email.clone(),inner.clone());
    println!("user has been registered: {}", inner.clone().email.clone());
    HttpResponse::Accepted().body("success")
}

#[post("/login")]
async fn login(user: web::Json<LoginDTO>) -> HttpResponse {
    let token = UserToken::login(&user.into_inner()).unwrap();
    let mut response = HashMap::new();
    response.insert("access_token", token.encode().unwrap());
    HttpResponse::Ok().json(&response)
}


pub fn configure(cfg: &mut web::ServiceConfig) {
    cfg.service(register);
    cfg.service(login);
}