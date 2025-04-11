use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    Str(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::Number(num) => write!(f, "{}", num),
            Value::Str(s) => write!(f, "{}", s),
        }
    }
}