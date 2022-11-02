use actix_web::{dev, Error, FromRequest, HttpRequest};
use actix_web::error::ErrorUnauthorized;
use futures::future::{err, ok, Ready};
use serde::{Deserialize, Serialize};
use actix_web::dev::ServiceRequest;
use actix_web_httpauth::extractors::bearer::{BearerAuth, Config};
use actix_web_httpauth::extractors::AuthenticationError;
use auth_service::auth_service::AuthorizationService;
use auth_model::auth_model::UserToken;
use log::{info};

pub struct Authorization;

impl FromRequest for Authorization {
    type Error = Error;
    type Future = Ready<Result<Authorization, Error>>;
    type Config = ();

    fn from_request(_req: &HttpRequest, _payload: &mut dev::Payload) -> Self::Future {
        let _auth = _req.headers().get("Authorization");
        match _auth {
            Some(_) => {
                let _split: Vec<&str> = _auth.unwrap().to_str().unwrap().split("Bearer").collect();
                let token = _split[1].trim();
                match UserToken::decode(token) {
                    Ok(_token) => ok(Authorization),
                    Err(_e) => err(ErrorUnauthorized("invalid token!")),
                }
            },
            None => err(ErrorUnauthorized("blocked!")),
        }
    }
}

pub async fn validator(req: ServiceRequest, credentials: BearerAuth) -> Result<ServiceRequest, Error> {
    let config = req
        .app_data::<Config>()
        .map(|data| data.clone())
        .unwrap_or_else(Default::default);
    info!("Token: {}", credentials.token().clone());
    match UserToken::decode(credentials.token()) {
        Ok(res) => {
            if res.is_valid().unwrap() {
                info!("TOKEN IS VALID");
                Ok(req)
            } else {
                info!("===INVALID===");
                Err(AuthenticationError::from(config).into())
            }
        }
        Err(_) => {
            info!("===ERROR===");
            Err(AuthenticationError::from(config).into())
        }
    }
}