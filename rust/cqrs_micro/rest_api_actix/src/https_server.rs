use actix_web::{App, HttpServer, middleware};
use actix_web::dev::Server;
use actix_web::web::scope;
use actix_web_httpauth::middleware::HttpAuthentication;
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod};

use crate::protected_api;
use crate::public_api;
use crate::authorization;
use log::{info};

pub fn start(port: u16, workers: usize) -> Server {
    let mut builder =
        SslAcceptor::mozilla_intermediate(SslMethod::tls()).unwrap();
    builder
        .set_private_key_file("_config_files/cert/ayagasha.key", SslFiletype::PEM)
        .unwrap();
    builder.set_certificate_chain_file("_config_files/cert/ayagasha.crt").unwrap();
    let mut host: String = "0.0.0.0:".to_owned();
    host.push_str(&port.to_string());
    info!("Starting HTTP service on: {}", host.clone());
    HttpServer::new(move || {
        let auth =
            HttpAuthentication::bearer(authorization::validator);
        App::new()
            .wrap(middleware::Logger::default())
            .service(scope("/users").configure(public_api::configure))
            .service(scope("/protected").wrap(auth).configure(protected_api::configure))

    })
        .bind_openssl(host.clone(), builder).unwrap()
        .workers(workers)
        .run()
}