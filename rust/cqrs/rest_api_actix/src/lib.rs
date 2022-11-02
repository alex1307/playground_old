mod controllers;
mod https_server;
mod http_server;
mod authorization;
use futures::future;
use log::{info};
#[actix_rt::main]
pub async fn start_actix() -> std::io::Result<()> {
    info!("Starting http and https servers...");
    let http = http_server::start(8044, 8);
    let https = https_server::start(8043, 8);
    info!("actix servers are started...");
    future::try_join(http, https).await?;
//    http.await?;
    Ok(())
}