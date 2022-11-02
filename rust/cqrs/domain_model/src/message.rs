use serde::{Deserialize, Serialize};
use chrono::Utc;
use uuid::Uuid;



#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Message {
    pub uuid: String,
    pub from: Contact,
    pub created_at: i64,
    pub subject: String,
    pub body: String,
    pub recipients: Vec<Contact>,
    pub attachments: Vec<Attachment>
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Attachment {
    pub message_id: String,
    pub name: String,
    pub file_type: String,
    pub payload: Vec<i32>
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Contact {
    pub first_name: String,
    pub last_name: String,
    pub email: String
}

#[derive(Debug, Deserialize, Serialize)]
pub struct MessageStatus {
    pub message_id: String,
    pub status: Status,
    pub created_at: i64
}


#[derive(Debug, Deserialize, Serialize)]
pub enum Status {
    SENT,
    RECEIVED,
    READ,
}

impl Message {
    pub fn new(from: Contact, recipients: Vec<Contact>, subject: String, body: String) -> Self {
        let now = Utc::now();
        Message {
            uuid: Uuid::new_v4().to_string(),
            from,
            created_at: now.timestamp_millis(),
            subject: subject.clone(),
            body: body.clone(),
            recipients,
            attachments:vec![]
        }
    }

    pub fn empty_message(from: Contact, recipients: Vec<Contact>) -> Self {
        let now = Utc::now();
        Message {
            uuid: Uuid::new_v4().to_string(),
            from,
            created_at: now.timestamp_millis(),
            subject: "".to_string(),
            body: "".to_string(),
            recipients: recipients.clone(),
            attachments: vec![]
        }
    }

    pub fn default_message(from: String, emails: Vec<String>) -> Self {

        let email_to_contact = |email: &String| -> Contact {
            Contact{
                first_name: "".to_string(),
                last_name: "".to_string(),
                email: email.clone()
            }
        };

        let recipients: Vec<Contact> = emails.iter().map(email_to_contact).collect();

        let now = Utc::now();
        Message {
            uuid: Uuid::new_v4().to_string(),
            from: email_to_contact(&from),
            created_at: now.timestamp_millis(),
            subject: "".to_string(),
            body: "".to_string(),
            recipients: recipients.clone(),
            attachments: vec![]
        }
    }

}

impl Attachment {
    pub fn new(message_id: String, name: String, file_type: String, payload: Vec<i32>) -> Self {
        Attachment {
            message_id,
            name,
            file_type,
            payload
        }
    }
}

impl Contact {
    fn new(first_name: String, last_name: String, email: String) -> Self {
        Contact {
            first_name,
            last_name,
            email
        }
    }
}