#[macro_use]
extern crate serial_test;

extern crate message_model;

use message_model::model::message::Message;
use crate::lib::AvroM;
use std::time::Instant;
use futures::prelude::future::join4;
use std::fs::File;
use std::io::Read;
use std::error::Error;



async fn task(label: &'static str, now: std::time::Instant) -> Result<(), Box<dyn Error + Send + Sync>> {
    // Simulate network delay using thread sleep for 2 seconds
    println!(
        "OS Thread {:?} - {} started: {:?}",
        std::thread::current().id(),
        label,
        now.elapsed(),
    );
    let mut file = File::open("./test/message.json").unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content);
    let json = content.clone();
    for _ in 0..250_000 {
        let _bin = Message::to_binary(&serde_json::from_slice::<Message>(content.clone().as_ref()).unwrap());
    }

    println!(
        "OS Thread {:?} - {} finished: {:?}",
        std::thread::current().id(),
        label,
        now.elapsed()
    );
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error + Send + Sync>>{

    let now = Instant::now();
    match join4(
        tokio::spawn(task("task1", now.clone())),
        tokio::spawn(task("task2", now.clone())),
        tokio::spawn(task("task3", now.clone())),
        tokio::spawn(task("task4", now.clone())),
    ).await {
        (x, y, z, t) => {
            (x.ok(), y.ok(), z.ok(), t.ok())
        }
    };

    let duration = Instant::now().duration_since(now);
    println!("String -> json -> binary  1_000_000 times: {:?}", duration);
    Ok(())
}