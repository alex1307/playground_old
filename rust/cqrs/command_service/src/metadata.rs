use std::time::Duration;

use clap::{value_t, App, Arg};
use log::{info};

use rdkafka::config::ClientConfig;
use rdkafka::consumer::{BaseConsumer, Consumer};


pub fn print_metadata(brokers: &str, topic: Option<&str>, timeout: Duration, fetch_offsets: bool) {
    let consumer: BaseConsumer = ClientConfig::new()
        .set("bootstrap.servers", brokers)
        .create()
        .expect("Consumer creation failed");

    info!("Consumer created");

    let metadata = consumer
        .fetch_metadata(None, timeout)
        .expect("Failed to fetch metadata");

    let mut message_count = 0;

    info!("Cluster information:");
    info!("  Broker count: {}", metadata.brokers().len());
    info!("  Topics count: {}", metadata.topics().len());
    info!("  Metadata broker name: {}", metadata.orig_broker_name());
    info!("  Metadata broker id: {}\n", metadata.orig_broker_id());

    info!("Brokers:");
    for broker in metadata.brokers() {
        info!(
            "  Id: {}  Host: {}:{}  ",
            broker.id(),
            broker.host(),
            broker.port()
        );
    }

    info!("\nTopics:");
    for topic in metadata.topics() {
        info!("  Topic: {}  Err: {:?}", topic.name(), topic.error());
        for partition in topic.partitions() {
            info!(
                "     Partition: {}  Leader: {}  Replicas: {:?}  ISR: {:?}  Err: {:?}",
                partition.id(),
                partition.leader(),
                partition.replicas(),
                partition.isr(),
                partition.error()
            );
            if fetch_offsets {
                let (low, high) = consumer
                    .fetch_watermarks(topic.name(), partition.id(), Duration::from_secs(1))
                    .unwrap_or((-1, -1));
                info!(
                    "       Low watermark: {}  High watermark: {} (difference: {})",
                    low,
                    high,
                    high - low
                );
                message_count += high - low;
            }
        }
        if fetch_offsets {
            info!("     Total message count: {}", message_count);
        }
    }
}

