use actix_web::{HttpResponse, post};
use actix_web::web::Json;


use crate::model::DownloadRequest;

#[post("/")]
pub fn download(_words: Json<DownloadRequest>) -> HttpResponse {
    HttpResponse::Accepted().body("success")
}