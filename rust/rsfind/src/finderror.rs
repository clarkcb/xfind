use core::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct FindError {
    pub description: String,
}

impl FindError {
    /// Create a new Error instance
    pub fn new(desc: &str) -> FindError {
        FindError {
            description: desc.to_string(),
        }
    }
}

impl fmt::Display for FindError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.description)
    }
}

impl Error for FindError {
    fn description(&self) -> &str {
        &self.description
    }
}
