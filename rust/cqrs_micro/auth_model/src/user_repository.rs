use std::collections::HashMap;
use std::sync::RwLock;
use std::time::{SystemTime, UNIX_EPOCH};

use crypto::digest::Digest;
use crypto::sha2::Sha256;
use lazy_static::lazy_static;
use crate::auth_model::{RegisterDTO, UserDTO};
use commons_lib::cqrs_entity_errors::EntityError;


lazy_static! {
    static ref HASHMAP: RwLock<HashMap<String, Option<UserDTO>>> = RwLock::new(HashMap::new());
}

pub fn create(email: String, user: RegisterDTO) -> Result<String, EntityError> {
    match get(&email) {
        Some(_found) =>
            Err(EntityError::AlreadyExistsErr("already.exists".to_string())),
        None => {
            let mut sha = Sha256::new();
            sha.input_str(user.password.as_str());
            let new_user = Some(UserDTO {
                email: email.clone(),
                password: sha.result_str(),
                first_name: user.first_name,
                last_name: user.last_name,
                created_at: timestamp(),
                enabled: true,
                authorities: user.authorities,
                roles: user.roles,
            });
            HASHMAP.write().unwrap().insert(email.clone(), new_user);
            Ok(email.clone())
        }
    }
}

pub fn register_user(email: String, password: String) -> Result<String, EntityError> {
    create(email.clone(), RegisterDTO {
        email: email.clone(),
        password: password.clone(),
        first_name: "".to_string(),
        last_name: "".to_string(),
        authorities: vec![],
        roles: vec![],
    })
}

pub fn size() -> usize {
    HASHMAP.read().unwrap().len()
}

pub fn update(email: &str, user: UserDTO) -> Result<UserDTO, EntityError> {
    match get(&email) {
        None =>
            Err(EntityError::NotFoundErr("user.not.found".to_string())),
        Some(_) => {
            *(HASHMAP.write().unwrap()).get_mut(email).unwrap() = Some(user.clone());
            Ok(user)
        }
    }
}

pub fn get(email: &str) -> Option<UserDTO> {
    match HASHMAP.read().unwrap().get(email) {
        Some(found) =>
            Some(found.clone().unwrap()),
        None => None
    }
}

pub fn delete(email: &str) -> Result<UserDTO, EntityError> {
    match get(&email) {
        None =>
            Err(EntityError::NotFoundErr("user.not.found".to_string())),
        Some(_) =>
            Ok(HASHMAP.write().unwrap().remove(email).unwrap().unwrap())
    }
}

pub fn validate_credentials(email: &str, password: &str) -> Result<UserDTO, EntityError> {
    if let Some(user) = get(email) {
        match user.password == password && user.enabled {
            true => Ok(UserDTO {
                email: user.email.clone(),
                password: user.password.clone(),
                first_name: user.first_name.clone(),
                last_name: user.last_name.clone(),
                created_at: user.created_at,
                enabled: user.enabled,
                authorities: user.authorities.clone(),
                roles: user.roles.clone(),
            }),

            false => Err(EntityError::InvalidDataErr("invalid.password".to_string()))
        }
    } else {
        Err(EntityError::NotFoundErr("not.found".to_string()))
    }
}

pub fn timestamp() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis()
}

#[cfg(test)]
pub mod user_service_unit_tests {

    use super::*;
    use std::time::Instant;
    use bcrypt::{hash};
    use uuid::Uuid;
    use crate::auth_model::Roles::Admin;
    use crate::auth_model::Authorities::READ;

    static EMAIL1: &str = "alex.test@email.com";
    static EMAIL2: &str = "alex.test2@email.com";

    #[test]
    #[serial]
    fn user_service_0_bcrypt_test() {
        let start = Instant::now();
        let pwd = hash("1234qebadfaf", 4).unwrap();
        let elapsed = start.elapsed().as_millis();
        println!("Hash: {}. Duration: {} ", pwd, elapsed);
    }

    #[test]
    #[serial]
    fn user_service_1_create_user_test() {
        if let Ok(email) = create(EMAIL2.clone().to_string(), RegisterDTO {
            email: String::from(EMAIL2.clone()),
            password: "12345678".to_string(),
            first_name: "Alex".to_string(),
            last_name: "Todorov".to_string(),
            authorities: vec![],
            roles: vec![],
        }) {
            assert_eq!(1, size());
            assert_eq!(EMAIL2.clone(), email);
        } else {
            assert!(false);
        }
    }

    #[test]
    #[serial]
    fn user_service_2_register_user_test() {
        register_user(EMAIL1.clone().to_string(), "1234".to_string());
        assert_eq!(2, size());
    }

    #[test]
    #[serial]
    fn user_service_3_update_user_test() {
        let now = timestamp();
        let mut sha = Sha256::new();
        sha.input_str("1234@5678.com");
        if let Ok(updated) = update(EMAIL2.clone(), UserDTO {
            email: String::from(EMAIL2.clone()),
            password: sha.result_str(),
            first_name: "James".to_string(),
            last_name: "Bond".to_string(),
            created_at: now,
            enabled: true,
            authorities: vec![READ],
            roles: vec![Admin],
        }) {
            assert_eq!(2, size());
            assert_eq!(now, updated.created_at);
            assert_eq!(sha.result_str(), updated.password);
            assert_eq!(true, updated.enabled);
            assert_eq!(EMAIL2.clone(), updated.email);
            println!("FOUND: {:?}", updated);
            assert_eq!(2, size());
        } else {
            assert!(false);
        }
    }

    #[test]
    #[serial]
    fn user_service_4_delete_user_test() {
        let deleted1 = delete(EMAIL1.clone()).unwrap();
        assert_eq!(1, size());
        assert_eq!(EMAIL1.clone(), deleted1.email);
        println!("DELETED: {:?}", deleted1);

        let deleted2 = delete(EMAIL2.clone()).unwrap();
        assert_eq!(0, size());
        assert_eq!(EMAIL2.clone(), deleted2.email);
        println!("DELETED: {:?}", deleted2);
    }

    #[test]
    #[serial]
    fn user_service_5_create_100000_users() {
        let start = SystemTime::now();
        for i in 0..100000 {
            register_user(i.to_string().clone(), (i * 11).to_string().clone());
        }
        assert_eq!(100000, size());
        println!("Registering {} duration: {:?}", size(), start.elapsed());
    }

    #[test]
    #[serial]
    #[should_panic(expected = "already.exists")]
    fn user_service_6_user_already_exists_test() {
        register_user(EMAIL1.clone().to_string(), "1234".to_string());
        println!("trying to register user with email {}", "alex1@eamil.com".to_string());
        match register_user(EMAIL1.clone().to_string(), "1234".to_string()) {
            Ok(_email) => println!("success"),
            Err(ue) =>
                {
                    assert_eq!(ue, EntityError::AlreadyExistsErr("already.exists".to_string()));
                    panic!("already.exists")
                }
        }
    }

    #[test]
    #[should_panic(expected = "not.found")]
    fn update_should_fail_with_not_found_test() {
        let email = Uuid::new_v4().to_string();
        if let Err(err) = update(email.as_str(), UserDTO {
            email: EMAIL2.clone().to_string(),
            password: "4567890".to_string(),
            first_name: "".to_string(),
            last_name: "".to_string(),
            created_at: 0,
            enabled: false,
            authorities: vec![],
            roles: vec![],
        }) {
            assert_eq!(err, EntityError::NotFoundErr("not.found".to_string()));
            panic!("not.found");
        } else {
            assert!(false);
        }
    }

    #[test]
    #[serial]
    fn validate_credentials_test() {
        let mut sha = Sha256::new();
        sha.input_str("1234");
        if let Ok(user) =  validate_credentials(EMAIL1, sha.result_str().as_str()) {
            assert_eq!(EMAIL1, user.email);
        } else {
            assert!(false);
        }

    }

    #[test]
    #[should_panic(expected = "not.found")]
    fn delete_should_fail_with_not_found_test() {
        let email = Uuid::new_v4().to_string();
        if let Err(err) = delete(email.as_str()) {
            assert_eq!(err, EntityError::NotFoundErr("not.found".to_string()));
            panic!("not.found");
        } else {
            assert!(false);
        }
    }
}
