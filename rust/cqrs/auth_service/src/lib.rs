#[macro_use]
extern crate serial_test;

mod auth_errors;
pub mod auth_service;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
