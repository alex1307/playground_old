use rest_api_actix;
use commons_lib;
use command_service;
use std::time::Duration;

fn main() {
    commons_lib::configure_log4rs();
    command_service::metadata::print_metadata(
        "r-kafka:9092",
        Some("messages"),
        Duration::from_millis(60000),
        true,
    );
    rest_api_actix::start_actix();
}
