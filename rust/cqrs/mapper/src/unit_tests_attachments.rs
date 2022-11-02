#[cfg(test)]
mod test_avro_attachment_mapper {


    use super::*;
    use avro_rs::Schema;
    use std::fs::File;
    use std::io::Read;
    use std::time::Instant;
    use crate::mapper::AvroM;
    use domain_model::message::{Attachment, Contact};


    #[test]
    fn test_1_attachment_to_avro_binary() {
        let attachment = Attachment::new("1234-4321".to_string(), "file.txt".to_string(), "txt/xml".to_string(), vec![87, 89, 90, 102, 103, 96, 56, 57, 78]);
        let binary = Attachment::to_binary(&attachment).unwrap();
        println!("1 -> bin size: {}", binary.clone().len());
        println!("binary: {:?}", binary.clone());
        let decoded = Attachment::from_binary(&binary.clone()).unwrap();

        assert_eq!(decoded.name, attachment.name);
        assert_eq!(decoded.file_type, attachment.file_type);
        assert_eq!(decoded.payload, attachment.payload);
        assert_eq!(decoded.message_id, attachment.message_id);
        println!("decoded: {:?}", decoded);
    }

    #[test]
    fn test_2_contact_to_avro_binary() {
        let contact = Contact{
            first_name: "James".to_string(),
            last_name: "Bond".to_string(),
            email: "james.007@mi6.co.uk".to_string()
        };
        let binary = Contact::to_binary(&contact).unwrap();
        println!("1 -> bin size: {}", binary.clone().len());
        println!("binary: {:?}", binary.clone());
        let decoded = Contact::from_binary(&binary.clone()).unwrap();

        assert_eq!(decoded.first_name, contact.first_name);
        assert_eq!(decoded.last_name, contact.last_name);
        assert_eq!(decoded.email, contact.email);
        println!("decoded: {:?}", decoded);
    }


    #[test]
    fn test_3_contacts_1_000_000_writes() {
        println!("======writes datum -> start =====");
        let start = Instant::now();
        let mut list_1_000_000 = Vec::with_capacity(1_000_000);
        for _i in 0..1_000_000 {
            let contact = Contact{
                first_name: "James".to_string(),
                last_name: "Bond".to_string(),
                email: "james.007@mi6.co.uk".to_string()
            };
            list_1_000_000.push(contact);
        }
        let duration = Instant::now().duration_since(start);
        println!("Generating 1M records: {:?}", duration);
        assert!(true);

        let start_encoding = Instant::now();
        for m in list_1_000_000 {
            Contact::to_binary(&m);
        }
        let enc_duration = Instant::now().duration_since(start_encoding);
        println!("Encoding 1M records: {:?}", enc_duration);

        let start_encoding_1 = Instant::now();
        let mut contact =  Contact{
            first_name: "James".to_string(),
            last_name: "Bond".to_string(),
            email: "james.007@mi6.co.uk".to_string()
        };
        for _i in 0..1_000_000 {
            Contact::to_binary(&contact);
        }
        let enc_duration1 = Instant::now().duration_since(start_encoding_1);
        println!("Encoding 1 record 1_000_000 times: {:?}", enc_duration1);

        assert!(true);
        println!("======writes datum -> end =====");
    }

    #[test]
    fn test_4_attachments_1_000_000_writes() {
        println!("======writes datum -> start =====");
        let start = Instant::now();
        let mut list_1_000_000 = Vec::with_capacity(1_000_000);
        for _i in 0..1_000_000 {
            let value = Attachment {
                message_id: "1234-1234".to_string(),
                name: "test_file.txt".to_string(),
                file_type: "text/xml".to_string(),
                payload: vec![1, 2, 4, 5, 6, 7,8, 10, 101, 98, 88, 99]
            };
            list_1_000_000.push(value);
        }
        let duration = Instant::now().duration_since(start);
        println!("Generating 1M records: {:?}", duration);
        assert!(true);

        let start_encoding = Instant::now();
        for m in list_1_000_000 {
            Attachment::to_binary(&m);
        }
        let enc_duration = Instant::now().duration_since(start_encoding);
        println!("Encoding 1M records: {:?}", enc_duration);

        let start_encoding_1 = Instant::now();
        let mut value =  Attachment {
            message_id: "1234-1234".to_string(),
            name: "test_file.txt".to_string(),
            file_type: "text/xml".to_string(),
            payload: vec![1, 2, 4, 5, 6, 7,8, 10, 101, 98, 88, 99]
        };
        for _i in 0..1_000_000 {
            Attachment::to_binary(&value);
        }
        let enc_duration1 = Instant::now().duration_since(start_encoding_1);
        println!("Encoding 1 record 1_000_000 times: {:?}", enc_duration1);

        assert!(true);
        println!("======writes datum -> end =====");
    }

}