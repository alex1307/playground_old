use actix_web::{HttpServer, middleware, App};
use actix_web::dev::Server;
use crate::authorization;
use crate::protected_api;
use crate::public_api;
use actix_web_httpauth::middleware::HttpAuthentication;
use actix_web::web::scope;
use log::{info};

pub fn start(port: u16, workers: usize) -> Server {
    let http_host = format!("0.0.0.0:{}", port);
    info!("Starting HTTP service on: {}", http_host.clone());
    HttpServer::new(|| {
        let auth =
            HttpAuthentication::bearer(authorization::validator);
        App::new()
            .wrap(middleware::Logger::default())
            .service(scope("/users").configure(public_api::configure))
            .service(scope("/protected").wrap(auth).configure(protected_api::configure))
    })
        .bind(http_host).unwrap()
        .workers(workers)
        .run()

}