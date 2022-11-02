use actix_web::{App, HttpServer, middleware};
use actix_web::dev::Server;
use actix_web::web::scope;
use actix_web_httpauth::middleware::HttpAuthentication;
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod};

use crate::authorization;
use crate::controllers;

pub fn start(port: u16, workers: usize) -> Server {
    let mut builder =
        SslAcceptor::mozilla_intermediate(SslMethod::tls()).unwrap();
    builder
        .set_private_key_file("_config_files/cert/ayagasha.key", SslFiletype::PEM)
        .unwrap();
    builder.set_certificate_chain_file("_config_files/cert/ayagasha.crt").unwrap();
    let  host = format!("127.0.0.1:{}", port);
    let mut host: String = "127.0.0.1:".to_owned();
    host.push_str(&port.to_string());
    let localhost = format!("https://localhost:{}", port);
    let iphost = format!("https://{}:{}", host, port);

    HttpServer::new(move || {
        let auth =
            HttpAuthentication::bearer(authorization::validator);
        App::new()
            .wrap(middleware::Logger::default())
            .service(scope("/users").configure(controllers::configure))
            .service(scope("/protected").wrap(auth).configure(controllers::configure))
    })
        .bind_openssl(host.clone(), builder).unwrap()
        .workers(workers)
        .run()
}