use std::fmt::Display;

use crate::ast::{BlockStatement, Identifier};

use super::environ::Environment;

#[derive(Debug)]
pub enum ObjectType {
    Return,
    Integer,
    Boolean,
    Null,
    Error,
    Function,
    String,
    Builtin,
    Array,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Return => write!(f, "RETURN"),
            ObjectType::Integer => write!(f, "INTEGER"),
            ObjectType::Boolean => write!(f, "BOOLEAN"),
            ObjectType::Null => write!(f, "NULL"),
            ObjectType::Error => write!(f, "ERROR"),
            ObjectType::Function => write!(f, "FUNCTION"),
            ObjectType::String => write!(f, "STRING"),
            ObjectType::Builtin => write!(f, "BUILTIN"),
            ObjectType::Array => write!(f, "ARRAY"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(String),
    Function(Function),
    String(String),
    Builtin(fn(Vec<Object>) -> Object),
    Array(Vec<Object>),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "{}", s),
            Object::Null => write!(f, "null"),
            Object::Return(r) => write!(f, "{}", r),
            Object::Error(e) => write!(f, "{}", e),
            Object::Function(fun) => write!(
                f,
                "fn({}) {{\n{}\n}}",
                fun.parameters
                    .iter()
                    .map(|p| p.token.literal())
                    .collect::<Vec<_>>()
                    .join(", "),
                fun.body
            ),
            Object::Builtin(_) => write!(f, "<builtin function>"),
            Object::Array(a) => write!(
                f,
                "[{}]",
                a.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

pub fn obj_type(obj: &Object) -> ObjectType {
    match obj {
        Object::Integer(_) => ObjectType::Integer,
        Object::Boolean(_) => ObjectType::Boolean,
        Object::Null => ObjectType::Null,
        Object::Return(_) => ObjectType::Return,
        Object::Error(_) => ObjectType::Error,
        Object::Function(_) => ObjectType::Function,
        Object::String(_) => ObjectType::String,
        Object::Builtin(_) => ObjectType::Builtin,
        Object::Array(_) => ObjectType::Array,
    }
}

#[derive(Clone, Debug)]
pub struct Integer {
    pub value: i64,
}

#[derive(Clone, Debug)]
pub struct Boolean {
    pub value: bool,
}

#[derive(Clone, Debug)]
pub struct Null {}

#[derive(Clone, Debug)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

#[derive(Clone, Debug)]
pub struct Error {
    pub message: String,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

#[derive(Clone, Debug)]
pub struct StringObj {
    pub value: String,
}

#[derive(Clone, Debug)]
pub struct Builtin {
    pub func: fn(Vec<Object>) -> Object,
}

#[derive(Clone, Debug)]
pub struct Array {
    pub elements: Vec<Object>,
}
