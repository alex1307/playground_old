use rest_api_actix;
use commons_lib;
use std::time::Duration;

fn main() {
    commons_lib::configure_log4rs();
    rest_api_actix::start_actix();
}
