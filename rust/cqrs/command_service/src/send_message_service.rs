use std::borrow::Borrow;
use std::fmt::Display;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use inner::IntoResult;
use lazy_static::lazy_static;
use rdkafka::ClientConfig;
use rdkafka::message::OwnedHeaders;
use rdkafka::producer::{BaseProducer, BaseRecord, Producer};
use serde::{Deserialize, Serialize};
use serde::de::DeserializeOwned;
use serde_json::ser::Formatter;
use serde_json::to_string_pretty;
use uuid::Uuid;
use crate::errors::ServiceErrors;


lazy_static! {
    pub static ref KAFKA_PRODUCER: BaseProducer = ClientConfig::new()
            .set("bootstrap.servers", "r-kafka:9092")
            .create()
            .expect("Producer creation error");

}

pub static mut COUNTER: i32 = 0;

pub fn send_message(topic: &str, partition: i32, payload: &Vec<u8>, header_key: &Vec<u8>) -> Result<(), ServiceErrors> {
    KAFKA_PRODUCER.send(
        BaseRecord::to(topic)
            .partition(partition)
            .payload(payload)
            .key(header_key),
    ).unwrap();
    unsafe {flush();}
    Ok(())
}

unsafe fn flush() {
    COUNTER = COUNTER + 1;
    if COUNTER % 1000 == 0 {
        KAFKA_PRODUCER.flush(Duration::from_millis(100));
    }
}

#[cfg(test)]
mod kafka_lib_test {

    use super::*;

    use std::time::{Duration, Instant};
    use log::info;
    use rdkafka::ClientConfig;
    use rdkafka::producer::{BaseProducer, BaseRecord, Producer};


    use std::sync::Once;

    static INIT: Once = Once::new();

    pub fn initialize() {
        INIT.call_once(|| {
            setup_logger(true, Some("rdkafka=info"));
        });
    }

    #[test]
    fn test_producer_with_10B_payload() {
        initialize();

        let producer: BaseProducer = ClientConfig::new()
            .set("bootstrap.servers", "r-kafka:9092")
            .create()
            .expect("Producer creation error");
        let start = Instant::now();
        for _ in 1..100_000 {
            producer.send(
                BaseRecord::to("benchmark_topic_10B")
                    .partition(0)
                    .payload("0123456789")
                    .key("key"),
            ).unwrap();
        }
        let duration = Instant::now().duration_since(start);
        info!("Sending 100K small (10 bytes) messages: {:?}", duration);
// Poll at regular intervals to process all the asynchronous delivery events.
        for _ in 0..100 {
            producer.poll(Duration::from_millis(100));
        }

// And/or flush the producer before dropping it.
        producer.flush(Duration::from_millis(200));
    }

    #[test]
    fn test_producer_with_200B_payload() {
        initialize();
        let producer: BaseProducer = ClientConfig::new()
            .set("bootstrap.servers", "r-kafka:9092")
            .create()
            .expect("Producer creation error");
        let binary:Vec<u8> =  (0..200).map(|i| {50 + (i % 50)}).collect();
        info!("Binary payload: {}", binary.len());
        let start = Instant::now();
        for _ in 1..100_000 {
            producer.send(
                BaseRecord::to("benchmark_topic_200B")
                    .partition(0)
                    .payload(&binary)
                    .key("key"),
            ).unwrap();
        }
        let duration = Instant::now().duration_since(start);
        info!("Sending 100K messages with size 200B: {:?}", duration);
        for _ in 0..100 {
            producer.poll(Duration::from_millis(200));
        }

        producer.flush(Duration::from_secs(1));
    }

    #[test]
    fn test_producer_with_1K_payload() {
        initialize();
        let producer: BaseProducer = ClientConfig::new()
            .set("bootstrap.servers", "r-kafka:9092")
            .create()
            .expect("Producer creation error");
        let binary:Vec<u8> =  (0..1024).map(|i| {50 + (i % 50) as u8}).collect();

        let start = Instant::now();
        for _ in 1..100_000 {
            producer.send(
                BaseRecord::to("benchmark_topic_1KB")
                    .partition(0)
                    .payload(&binary)
                    .key("and this is a key"),
            ).unwrap();
        }
        let duration = Instant::now().duration_since(start);
        info!("Sending 100 000 messages with size of 1K: {:?}", duration);

        // Poll at regular intervals to process all the asynchronous delivery events.
        for _ in 0..20 {
            producer.poll(Duration::from_millis(100));
        }

        // And/or flush the producer before dropping it.
        producer.flush(Duration::from_millis(200));
    }

    #[test]
    fn test_producer_with_10K_payload() {
        initialize();
        let producer: BaseProducer = ClientConfig::new()
            .set("bootstrap.servers", "r-kafka:9092")
            .create()
            .expect("Producer creation error");
        let binary:Vec<u8> =  (0..10240).map(|i| {50 + (i % 50) as u8}).collect();

        let start = Instant::now();
        for _ in 1..100_000 {
            producer.send(
                BaseRecord::to("benchmark_topic_10KB")
                    .partition(0)
                    .payload(&binary)
                    .key("and this is a key"),
            ).unwrap();
        }
        let duration = Instant::now().duration_since(start);
        info!("Sending 100 000 messages with size of 10K: {:?}", duration);

        // Poll at regular intervals to process all the asynchronous delivery events.
        for _ in 0..20 {
            producer.poll(Duration::from_secs(1));
        }

        // And/or flush the producer before dropping it.
        producer.flush(Duration::from_secs(10));
    }
}
