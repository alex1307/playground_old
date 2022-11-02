use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct UserDTO {
    pub email: String,
    pub password: String,
    pub first_name: String,
    pub last_name: String,
    pub created_at: u128,
    pub enabled: bool,
    pub authorities: Vec<Authorities>,
    pub roles: Vec<Roles>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum Authorities {
    READ,
    WRITE,
    DELETE,
    FullAccess,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UserToken {
    pub created_at: i64,
    pub duration: i64,
    pub user_id: String,
    pub authorities: Vec<Authorities>,
    pub roles: Vec<Roles>,
    pub exp: usize,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum Roles {
    Admin,
    User,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct RegisterDTO {
    pub email: String,
    pub password: String,
    pub first_name: String,
    pub last_name: String,
    pub authorities: Vec<Authorities>,
    pub roles: Vec<Roles>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct LoginDTO {
    pub email: String,
    pub password: String,
    #[serde(default)]
    pub remember_me: bool,
}