use std::fmt::Display;

use peng_parser::{parser::ast::{BinaryOperator, UnaryLeftOperator}, join};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int, Float, Bool, Char, String,
    Vector(Option<Box<Self>>), Object,

    Function(Vec<Self>, Option<Box<Self>>),
    Option(Option<Box<Self>>), Result(Option<(Box<Self>, Box<Self>)>)
}
impl Type {
    pub fn binary(op: BinaryOperator, left: &Self, right: &Self) -> Option<Self> {
        match op {
            BinaryOperator::Add => match (left, right) {
                (Self::Int, Self::Int) => Some(Self::Int),
                (Self::Float, Self::Float) => Some(Self::Float),
                (Self::Int, Self::Float) => Some(Self::Float),
                (Self::Float, Self::Int) => Some(Self::Float),
                (Self::String, Self::String) => Some(Self::String),
                (Self::String, Self::Char) => Some(Self::String),
                (Self::Vector(a), Self::Vector(b)) if a == b => Some(Self::Vector(a.clone())),
                _ => None
            }
            BinaryOperator::Sub => match (left, right) {
                (Self::Int, Self::Int) => Some(Self::Int),
                (Self::Float, Self::Float) => Some(Self::Float),
                (Self::Int, Self::Float) => Some(Self::Float),
                (Self::Float, Self::Int) => Some(Self::Float),
                _ => None
            }
            BinaryOperator::Mul => match (left, right) {
                (Self::Int, Self::Int) => Some(Self::Int),
                (Self::Float, Self::Float) => Some(Self::Float),
                (Self::Int, Self::Float) => Some(Self::Float),
                (Self::Float, Self::Int) => Some(Self::Float),
                (Self::String, Self::Int) => Some(Self::String),
                (Self::Char, Self::Int) => Some(Self::String),
                _ => None
            }
            BinaryOperator::Div => match (left, right) {
                (Self::Int, Self::Int) => Some(Self::Float),
                (Self::Float, Self::Float) => Some(Self::Float),
                (Self::Int, Self::Float) => Some(Self::Float),
                (Self::Float, Self::Int) => Some(Self::Float),
                _ => None
            }
            BinaryOperator::Mod => match (left, right) {
                (Self::Int, Self::Int) => Some(Self::Int),
                (Self::Float, Self::Float) => Some(Self::Float),
                (Self::Int, Self::Float) => Some(Self::Float),
                (Self::Float, Self::Int) => Some(Self::Float),
                _ => None
            }
            BinaryOperator::Pow => match (left, right) {
                (Self::Int, Self::Int) => Some(Self::Int),
                (Self::Float, Self::Float) => Some(Self::Float),
                (Self::Int, Self::Float) => Some(Self::Float),
                (Self::Float, Self::Int) => Some(Self::Float),
                _ => None
            }
            BinaryOperator::EQ => match (left, right) {
                (Self::Int, Self::Int) | (Self::Float, Self::Float) |
                (Self::Int, Self::Float) | (Self::Float, Self::Int) |
                (Self::String, Self::String) | (Self::Char, Self::Char) |
                (Self::Object, Self::Object)
                => Some(Self::Bool),
                (Self::Vector(a), Self::Vector(b)) if a == b => Some(Self::Bool),
                _ => None
            }
            BinaryOperator::NE => match (left, right) {
                (Self::Int, Self::Int) | (Self::Float, Self::Float) |
                (Self::Int, Self::Float) | (Self::Float, Self::Int) |
                (Self::String, Self::String) | (Self::Char, Self::Char) |
                (Self::Object, Self::Object)
                => Some(Self::Bool),
                (Self::Vector(a), Self::Vector(b)) if a == b => Some(Self::Bool),
                _ => None
            }
            BinaryOperator::LT => match (left, right) {
                (Self::Int, Self::Int) | (Self::Float, Self::Float) |
                (Self::Int, Self::Float) | (Self::Float, Self::Int)
                => Some(Self::Bool),
                _ => None
            }
            BinaryOperator::GT => match (left, right) {
                (Self::Int, Self::Int) | (Self::Float, Self::Float) |
                (Self::Int, Self::Float) | (Self::Float, Self::Int)
                => Some(Self::Bool),
                _ => None
            }
            BinaryOperator::LE => match (left, right) {
                (Self::Int, Self::Int) | (Self::Float, Self::Float) |
                (Self::Int, Self::Float) | (Self::Float, Self::Int)
                => Some(Self::Bool),
                _ => None
            }
            BinaryOperator::GE => match (left, right) {
                (Self::Int, Self::Int) | (Self::Float, Self::Float) |
                (Self::Int, Self::Float) | (Self::Float, Self::Int)
                => Some(Self::Bool),
                _ => None
            }
            BinaryOperator::And => match (left, right) {
                (Self::Bool, Self::Bool) => Some(Self::Bool),
                _ => None
            }
            BinaryOperator::Or => match (left, right) {
                (Self::Bool, Self::Bool) => Some(Self::Bool),
                _ => None
            }
            BinaryOperator::In => match (left, right) {
                (Self::Int, Self::Vector(t)) => if let Some(t) = t {
                    if t.as_ref() == &Self::Int {
                        Some(Self::Bool)
                    } else {
                        None
                    }
                } else {
                    None
                }
                (Self::Float, Self::Vector(t)) => if let Some(t) = t {
                    if t.as_ref() == &Self::Int {
                        Some(Self::Bool)
                    } else {
                        None
                    }
                } else {
                    None
                }
                (Self::Bool, Self::Vector(t)) => if let Some(t) = t {
                    if t.as_ref() == &Self::Int {
                        Some(Self::Bool)
                    } else {
                        None
                    }
                } else {
                    None
                }
                (Self::Char, Self::Vector(t)) => if let Some(t) = t {
                    if t.as_ref() == &Self::Int {
                        Some(Self::Bool)
                    } else {
                        None
                    }
                } else {
                    None
                }
                (Self::String, Self::Vector(t)) => if let Some(t) = t {
                    if t.as_ref() == &Self::Int {
                        Some(Self::Bool)
                    } else {
                        None
                    }
                } else {
                    None
                }
                (Self::Vector(a), Self::Vector(b)) if b == &Some(Box::new(Self::Vector(a.clone()))) => Some(Self::Bool),
                (Self::Object, Self::Vector(t)) => if let Some(t) = t {
                    if t.as_ref() == &Self::Object {
                        Some(Self::Bool)
                    } else {
                        None
                    }
                } else {
                    None
                }
                (Self::String, Self::Object) => Some(Self::Bool),
                _ => None
            }
        }
    }
    pub fn unary_left(op: UnaryLeftOperator, right: &Self) -> Option<Self> {
        match op {
            UnaryLeftOperator::Neg => match right {
                Self::Int => Some(Self::Int),
                Self::Float => Some(Self::Float),
                _ => None
            }
            UnaryLeftOperator::Not => match right {
                Self::Bool => Some(Self::Bool),
                _ => None
            }
        }
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::Char => write!(f, "char"),
            Self::String => write!(f, "str"),
            Self::Vector(sub) => if let Some(sub) = sub {
                write!(f, "vec<{}>", sub)
            } else {
                write!(f, "vec<?>")
            }
            Self::Object => write!(f, "obj"),
            Self::Function(params, ret) => write!(f, "func({}){}", join!(params, ", "),
                if let Some(ret) = ret {
                    format!(" -> {}", ret)
                } else {
                    String::new()
                }
            ),
            Self::Option(sub) => if let Some(sub) = sub {
                write!(f, "opt<{}>", sub)
            } else {
                write!(f, "opt<?>")
            }
            Self::Result(subs) => if let Some(subs) = subs {
                write!(f, "result<{}, {}>", subs.0, subs.1)
            } else {
                write!(f, "result<?, ?>")
            }
        }
    }
}