use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::time::Instant;

#[cfg(test)]
mod test_avro_message_mapper {
    use std::error::Error;
    use std::fs::File;
    use std::io::Read;
    use std::time::Instant;

    use crate::mapper::AvroM;
    use domain_model::message::Message;


    #[test]
    #[serial]
    fn read_json_and_convert_it_to_avro_binary_test() {
        let mut file = File::open("./test/message.json").unwrap();
        let mut content = String::new();
        file.read_to_string(&mut content);
        let json = content.clone();
        let start = Instant::now();
        for _ in 0..1_000_000 {
            let _bin = Message::to_binary(&serde_json::from_slice::<Message>(json.as_ref()).unwrap());
        }
        let duration = Instant::now().duration_since(start);
        println!("String -> json -> binary  1_000_000 times: {:?}", duration);
    }
}
