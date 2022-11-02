#[cfg(test)]
mod test_avro_message_mapper {
    use std::fs::File;
    use std::io::Read;
    use std::time::Instant;

    use avro_rs::Schema;

    use super::*;
    use crate::mapper::{AvroM, read_from_batch, write_to_batch, SCHEMAS, flatten};
    use domain_model::message::{Contact, Message, Attachment};

    #[test]
    #[serial]
    fn avro_test_01_should_read_the_content_of_schema() {

        let tuple: &(Schema, Schema, Schema) = &SCHEMAS;
        let (a, b, c) = tuple;
        println!("a schema: {:?}", a);
        println!("b schema: {:?}", b);
    }

    #[test]
    fn test_1_message_to_avro_binary() {
        let contact = Contact {
            first_name: "Alex".to_string(),
            last_name: "Todorov".to_string(),
            email: "alex@email.com".to_string(),
        };

        let recipient = Contact {
            first_name: "James".to_string(),
            last_name: "Bond".to_string(),
            email: "007@mi6.com".to_string(),
        };

        let mut message = Message::empty_message(contact, vec![recipient]);
        let attachment = Attachment::new(message.uuid.clone(), "download.me".to_string(), "zip".to_string(), vec![]);
        message.attachments = vec![attachment];

        let binary = Message::to_binary(&message).unwrap();
        println!("1 -> bin size: {}", binary.clone().len());
        println!("binary: {:?}", binary.clone());
        let messages = Message::from_binary(&binary.clone());
        let decoded = messages.unwrap();
        // // assert_eq!(message.from, decoded.from);
        // // assert_eq!(message.recipients, decoded.recipients);
        // assert_eq!(message.created_at, decoded.created_at);
        // assert_eq!(message.subject, decoded.subject);
        // assert_eq!(message.body, decoded.body);
        // assert_eq!(message.uuid, decoded.uuid);
        println!("decoded: {:?}", decoded);
        //
        // let json = Message::to_json(&decoded).unwrap();
        // println!("json size: {}", json.into_bytes().len())
    }


    #[test]
    fn test_1_000_000_writes() {
        println!("======writes datum -> start =====");
        let start = Instant::now();
        let mut list_1_000_000 = Vec::with_capacity(1_000_000);
        for _i in 0..1_000_000 {
            let mut message = Message::default_message("ABC_abc_123".to_string(), vec!["ABCD".to_string()]);
            let attachment = Attachment::new(message.uuid.clone(), "download.me".to_string(), "zip".to_string(), vec![]);
            message.attachments = vec![attachment];
            list_1_000_000.push(message);
        }
        let duration = Instant::now().duration_since(start);
        println!("Generating 1M records: {:?}", duration);
        assert!(true);

        let start_encoding = Instant::now();
        for m in list_1_000_000 {
            Message::to_binary(&m);
        }
        let enc_duration = Instant::now().duration_since(start_encoding);
        println!("Encoding 1M records: {:?}", enc_duration);

        let start_encoding_1 = Instant::now();
        let mut message = Message::default_message("ABC_abc_123".to_string(), vec!["ABCD".to_string()]);
        let attachment = Attachment::new(message.uuid.clone(), "download.me".to_string(), "zip".to_string(), vec![]);
        message.attachments = vec![attachment];
        for _i in 0..1_000_000 {
            Message::to_binary(&message);
        }
        let enc_duration1 = Instant::now().duration_since(start_encoding_1);
        println!("Encoding 1 record 1_000_000 times: {:?}", enc_duration1);

        assert!(true);
        println!("======writes datum -> end =====");
    }

    #[test]
    fn test_1_000_000_reads() {
        println!("======reads datum -> start =====");
        let start = Instant::now();
        let mut list_1_000_000 = Vec::with_capacity(1_000_000);
        for _i in 0..1_000_000 {
            let mut message = Message::default_message("ABC_abc_123".to_string(), vec!["ABCD".to_string()]);
            let attachment = Attachment::new(message.uuid.clone(), "download.me".to_string(), "zip".to_string(), vec![]);
            message.attachments = vec![attachment];
            list_1_000_000.push(Message::to_binary(&message).unwrap());
        }
        let duration = Instant::now().duration_since(start);
        let flatten = flatten(list_1_000_000.clone());
        println!("Creating and encoding 1M records: {:?}. Size of 1M avro obj: {}", duration, flatten.len());
        assert!(true);

        let start_decoding = Instant::now();
        for bin in list_1_000_000 {
            Message::from_binary(&bin);
        }
        let decoding_duration = Instant::now().duration_since(start_decoding);
        println!("Decoding 1M records: {:?}", decoding_duration);
        assert!(true);
        println!("======reads datum -> end =====");
    }

    #[test]
    fn test_1_000_000_batch_writes() {
        println!("======writes batch -> start =====");
        let mut list_1_000_000 = Vec::with_capacity(1_000_000);
        for _i in 0..1_000_000 {
            let mut message = Message::default_message("ABC_abc_123".to_string(), vec!["ABCD".to_string()]);
            let attachment = Attachment::new(message.uuid.clone(), "download.me".to_string(), "zip".to_string(), vec![]);
            message.attachments = vec![attachment];
            list_1_000_000.push(message);
        }
        let start = Instant::now();
        write_to_batch(list_1_000_000);
        let duration = Instant::now().duration_since(start);
        println!("Encoding 1M messages in batch: {:?}", duration);
        assert!(true);
        println!("======writes batch -> end =====");
    }

    //Reader::with_schema(schema, &bytes[..]).unwrap();

    #[test]
    fn test_1_000_000_batch_reads() {
        println!("======reads batch -> start =====");
        let mut list_1_000_000 = Vec::with_capacity(1_000_000);
        for _i in 0..1_000_000 {
            let mut message = Message::default_message("ABC_abc_123".to_string(), vec!["ABCD".to_string()]);
            let attachment = Attachment::new(message.uuid.clone(), "download.me".to_string(), "zip".to_string(), vec![]);
            message.attachments = vec![attachment];
            list_1_000_000.push(message);
        }
        let start = Instant::now();
        let binaries = write_to_batch(list_1_000_000).unwrap();
        let duration = Instant::now().duration_since(start);
        println!("Encoding 1M messages in batch: {:?}", duration);

        let start_decoding = Instant::now();
        let res = read_from_batch(binaries);
        let decoding_duration = Instant::now().duration_since(start_decoding);
        println!("Decoding 1M messages in batch: {:?}", decoding_duration);
        println!("result: {:?}", res.unwrap().len());
        assert!(true);
        println!("======reads batch -> end =====");
    }
}