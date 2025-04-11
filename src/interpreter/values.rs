use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(bool) => write!(f, "{}", bool),
        }
    }
}