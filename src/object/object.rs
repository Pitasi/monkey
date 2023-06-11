use std::fmt::Display;

use crate::ast::AsAny;

#[derive(Debug)]
pub enum ObjectType {
    Return,
    Integer,
    Boolean,
    Null,
    Error,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Return => write!(f, "RETURN"),
            ObjectType::Integer => write!(f, "INTEGER"),
            ObjectType::Boolean => write!(f, "BOOLEAN"),
            ObjectType::Null => write!(f, "NULL"),
            ObjectType::Error => write!(f, "ERROR"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    Return(ReturnValue),
    Error(Error),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i.value),
            Object::Boolean(b) => write!(f, "{}", b.value),
            Object::Null(_) => write!(f, "null"),
            Object::Return(r) => write!(f, "{}", r),
            Object::Error(e) => write!(f, "{}", e),
        }
    }
}

pub fn obj_type(obj: &Object) -> ObjectType {
    match obj {
        Object::Integer(_) => ObjectType::Integer,
        Object::Boolean(_) => ObjectType::Boolean,
        Object::Null(_) => ObjectType::Null,
        Object::Return(_) => ObjectType::Return,
        Object::Error(_) => ObjectType::Error,
    }
}

pub trait ObjectTrait: AsAny {
    fn obj_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Clone, Debug)]
pub struct Integer {
    pub value: i64,
}

impl ObjectTrait for Integer {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct Boolean {
    pub value: bool,
}

impl ObjectTrait for Boolean {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct Null {}

impl ObjectTrait for Null {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Clone, Debug)]
pub struct ReturnValue {
    pub value: Box<Object>,
}

impl Display for ReturnValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl ObjectTrait for ReturnValue {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Return
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct Error {
    pub message: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl ObjectTrait for Error {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Error
    }

    fn inspect(&self) -> String {
        format!("{}", self.message)
    }
}
