use std::fmt::Display;

use crate::ast::{AsAny, BlockStatement, Identifier};

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
    Function(Function),
    String(StringObj),
    Builtin(Builtin),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i.value),
            Object::Boolean(b) => write!(f, "{}", b.value),
            Object::Null(_) => write!(f, "null"),
            Object::Return(r) => write!(f, "{}", r),
            Object::Error(e) => write!(f, "{}", e),
            Object::Function(fun) => write!(f, "{}", fun.inspect()),
            Object::String(s) => write!(f, "{}", s.inspect()),
            Object::Builtin(b) => write!(f, "{}", b.inspect()),
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
        Object::Function(_) => ObjectType::Function,
        Object::String(_) => ObjectType::String,
        Object::Builtin(_) => ObjectType::Builtin,
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

#[derive(Clone, Debug)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl ObjectTrait for Function {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Function
    }

    fn inspect(&self) -> String {
        format!(
            "fn({}) {{\n{}\n}}",
            self.parameters
                .iter()
                .map(|p| p.token.literal())
                .collect::<Vec<_>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Clone, Debug)]
pub struct StringObj {
    pub value: String,
}

impl ObjectTrait for StringObj {
    fn obj_type(&self) -> ObjectType {
        ObjectType::String
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Clone, Debug)]
pub struct Builtin {
    pub func: fn(Vec<Object>) -> Object,
}

impl ObjectTrait for Builtin {
    fn obj_type(&self) -> ObjectType {
        ObjectType::Builtin
    }

    fn inspect(&self) -> String {
        "builtin function".to_string()
    }
}
