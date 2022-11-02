use log::{debug, error, info, warn};
use log4rs;

pub mod cqrs_entity_errors;

pub fn configure_log4rs() {
    log4rs::init_file("_config_files/log4rs.yml", Default::default()).unwrap();
    warn!("SUCCESS: Loggers are configured with dir: _log/*");;
}
