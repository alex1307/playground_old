use std::{thread::sleep, time::Duration};

use playground::{configure_log4rs, send_client_request, start_server};

fn main() {
    configure_log4rs();
    let server_name = "server1".to_string();
    let cleint_name = "client1".to_string();
    let cleint_name2 = "client2".to_string();
    start_server(server_name.clone());
    start_server(cleint_name.clone());
    start_server(cleint_name2.clone());
    let binary = Vec::from("some string".as_bytes());
    // sleep(Duration::from_secs(2));
    // let server_name = "server1".to_string();
    let _r = send_client_request(
        vec![cleint_name.clone(), cleint_name2.clone()],
        &server_name,
        binary.clone(),
    );
    // sleep(Duration::from_secs(5));
    // let _r = send_client_request("client1".to_string(), &server_name, binary);
    sleep(Duration::from_secs(10));
}
