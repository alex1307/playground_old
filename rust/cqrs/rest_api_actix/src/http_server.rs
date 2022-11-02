use actix_web::{HttpServer, middleware, App};
use actix_web::dev::Server;
use crate::authorization;
use crate::controllers;
use actix_web_httpauth::middleware::HttpAuthentication;
use actix_web::web::scope;

pub fn start(port: u16, workers: usize) -> Server {
    let http_host = format!("cqrs:{}", port);
    HttpServer::new(|| {
        let auth =
            HttpAuthentication::bearer(authorization::validator);
        App::new()
            .wrap(middleware::Logger::default())
            .service(scope("/users").configure(controllers::configure))
            .service(scope("/protected").wrap(auth).configure(controllers::configure))
    })
        .bind(http_host).unwrap()
        .workers(workers)
        .run()

}