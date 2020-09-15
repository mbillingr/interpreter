use crate::integer::Int;

#[derive(Clone, Debug)]
pub enum Number {
    Integer(Int),
    Float(f64),
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Number::Integer(i) => write!(f, "{}", i),
            Number::Float(x) => write!(f, "{}", x),
        }
    }
}
