use std::collections::HashMap;
use std::sync::RwLock;

use chrono::{Duration, Utc};
use crypto::digest::Digest;
use crypto::sha2::Sha256;
use jsonwebtoken::{Algorithm, decode, DecodingKey, encode, EncodingKey, Header, Validation};
use lazy_static::lazy_static;

use crate::auth_errors::AuthError;
use auth_model::user_repository::validate_credentials;
use auth_model::auth_model::{LoginDTO, UserToken};

lazy_static! {
    static ref TOKENS: RwLock<HashMap<String, String>> = RwLock::new(HashMap::new());
}

static KEY: &str = "dc)3)E~}+!tW&)cWvDraXtU5Tb}X%A$D[o9@SM3;!dg/BpCf";

pub trait AuthorizationService {
    fn encode(&self) -> Result<String, AuthError>;
    fn is_valid(&self) -> Result<bool, AuthError>;
    fn login(login: &LoginDTO) -> Result<UserToken, AuthError>;
    fn decode(token: &str) -> Result<UserToken, AuthError>;
}

impl AuthorizationService for UserToken {
    fn encode(&self) -> Result<String, AuthError> {
        if let Ok(valid) = self.is_valid() {
            if let Ok(token) = encode(
                &Header::new(Algorithm::HS256),
                &self,
                &EncodingKey::from_secret(KEY.as_bytes().clone())) {
                Ok(token)
            } else {
                Err(AuthError::ServiceErr("temp.unavailable".to_string()))
            }
        } else {
            Err(AuthError::TokenExpiredErr("expired".to_string()))
        }
    }

    fn is_valid(&self) -> Result<bool, AuthError> {
        let now = Utc::now().timestamp_millis() as usize;
        if now < self.exp {
            Ok(true)
        } else {
            Err(AuthError::TokenExpiredErr("expired".to_string()))
        }
    }

    fn login(login: &LoginDTO) -> Result<UserToken, AuthError> {
        let mut sha = Sha256::new();
        sha.input_str(login.password.as_str());
        let hashed = sha.result_str();
        if let Ok(user) = validate_credentials(&login.email, &hashed) {
            let now = Utc::now().timestamp_millis();
            Ok(UserToken {
                created_at: now.clone(),
                user_id: login.email.clone(),
                duration: Duration::hours(1).num_milliseconds(),
                exp: (now.clone() + Duration::hours(1).num_milliseconds()) as usize,
                authorities: user.authorities.clone(),
                roles: user.roles.clone(),
            })
        } else {
            Err(AuthError::Unauthorized("unauthorized".to_string()))
        }
    }

    fn decode(token: &str) -> Result<UserToken, AuthError> {
        match decode::<UserToken>(token,
                                  &DecodingKey::from_secret(KEY.as_bytes()),
                                  &Validation::new(Algorithm::HS256)) {
            Ok(data) => {
                let user_token = data.claims;
                Ok(UserToken{
                    created_at: user_token.created_at,
                    duration: user_token.duration,
                    user_id: user_token.user_id.clone(),
                    authorities:user_token.authorities.clone(),
                    roles: user_token.roles.clone(),
                    exp: user_token.exp
                })
            },
            _ => Err(AuthError::TokenInvalidErr("Invalid".to_string()))
        }
    }
}

#[cfg(test)]
pub mod auth_service_unit_tests {

    use crate::auth_service::AuthorizationService;
    use auth_model::auth_model::Authorities::FullAccess;
    use auth_model::auth_model::Roles::Admin;
    use auth_model::auth_model::{RegisterDTO, UserToken, LoginDTO};
    use auth_model::user_repository::{get, create, size, delete};

    static EMAIL1: &str = "auth1.test@email.com";
    static PWD: &str = "1234";

    #[test]
    #[serial]
    fn test_1_setup() {
        let user1 = RegisterDTO {
            email: EMAIL1.to_string(),
            password: PWD.to_string(),
            first_name: "Alex".to_string(),
            last_name: "The First".to_string(),
            authorities: vec![FullAccess],
            roles: vec![Admin],
        };

        create(EMAIL1.to_string(), user1);

        if let Some(user) = get(EMAIL1) {
            assert_eq!(EMAIL1, user.email);
            assert_eq!(1, user.authorities.len());
            assert_eq!(1, user.roles.len());
            assert!(user.authorities.contains(&FullAccess));
            assert!(user.roles.contains(&Admin));
            assert_eq!(vec![Admin], user.roles);
            assert_eq!(vec![FullAccess], user.authorities);
        } else {
            assert!(false)
        }
    }


    #[test]
    #[serial]
    fn test_2_login() {
        let login = LoginDTO {
            email: EMAIL1.to_string(),
            password: PWD.to_string(),
            remember_me: false
        };
        if let Ok(token) = UserToken::login(&login) {
            println!("token has been created: {:?}", token);
            assert_eq!(EMAIL1, token.user_id);
            assert_eq!(1, token.authorities.len());
            assert_eq!(1, token.roles.len());
            assert!(token.authorities.contains(&FullAccess));
            assert!(token.roles.contains(&Admin));
            assert_eq!(vec![Admin], token.roles);
            assert_eq!(vec![FullAccess], token.authorities);
        } else {
            assert!(false)
        }
    }

    #[test]
    #[serial]
    fn test_3_is_token_valid() {
        let login = LoginDTO {
            email: EMAIL1.to_string(),
            password: PWD.to_string(),
            remember_me: false
        };
        if let Ok(token) = UserToken::login(&login) {
            assert!(token.is_valid().unwrap())
        } else {
            assert!(false);
        }
    }

    #[test]
    #[serial]
    fn test_4_encode() {
        let login = LoginDTO {
            email: EMAIL1.to_string(),
            password: PWD.to_string(),
            remember_me: false
        };
        if let Ok(user_token) = UserToken::login(&login) {
            if let Ok(token) = user_token.encode() {
                println!("Token: {}", token);
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
    }

    #[test]
    #[serial]
    fn test_5_decode() {
        let login = LoginDTO {
            email: EMAIL1.to_string(),
            password: PWD.to_string(),
            remember_me: false
        };
        if let Ok(user_token) = UserToken::login(&login) {
            if let Ok(token) = user_token.encode() {
                println!("Token: {}", token);
                if let Ok(user_token) = UserToken::decode(&token) {
                    assert_eq!(EMAIL1, user_token.user_id);
                    assert_eq!(1, user_token.authorities.len());
                    assert_eq!(1, user_token.roles.len());
                    assert!(user_token.authorities.contains(&FullAccess));
                    assert!(user_token.roles.contains(&Admin));
                    assert_eq!(vec![Admin], user_token.roles);
                    assert_eq!(vec![FullAccess], user_token.authorities);
                }
            } else {
                assert!(false);
            }
        } else {
            assert!(false);
        }
    }

    #[test]
    #[serial]
    fn test_99999_clean() {
        let original_size = size();
        if let Ok(deleted) = delete(EMAIL1) {
            assert_eq!(EMAIL1, deleted.email);
            let current_size = size();
            assert_eq!(current_size, original_size - 1);
        } else {
            assert!(false);
        }
    }
}