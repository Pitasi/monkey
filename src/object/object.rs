use std::fmt::Display;

use crate::{
    ast::{BlockStatement, Identifier},
    map::InefficientMap,
};

use super::environ::Environment;

#[derive(Debug, Clone, PartialEq)]
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
    HashMap,
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
            ObjectType::HashMap => write!(f, "HASHMAP"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Function(Function),
    String(String),
    Builtin(fn(Vec<Object>) -> Object),
    Array(Vec<Object>),
    Error(Err),
    HashMap(InefficientMap<Object, Object>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Err {
    UnknownInfixOperator(String, Box<Object>, Box<Object>),
    UnknownPrefixOperator(String, Box<Object>),
    UnsupportedIndexOperator(Box<Object>),
    IndexNotInteger(Box<Object>),
    WrongArgumentsCount(usize, usize),
    NotAFunction(Box<Object>),
    IdentifierNotFound(Identifier),
    WrongArgumentType(String, ObjectType, Box<Object>),
}

impl<'a> Display for Err {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Err::UnknownPrefixOperator(op, val) => {
                write!(f, "unknown operator: {}{}", op, obj_type(&val))
            }
            Err::UnknownInfixOperator(op, l, r) => write!(
                f,
                "unknown operator: {} {} {}",
                obj_type(&l),
                op,
                obj_type(&r)
            ),
            Err::UnsupportedIndexOperator(obj) => {
                write!(f, "index operator not supported: {}", obj_type(&obj))
            }
            Err::WrongArgumentsCount(expected, got) => {
                write!(
                    f,
                    "wrong number of arguments: expected {}, got {}",
                    expected, got
                )
            }
            Err::NotAFunction(obj) => write!(f, "not a function: {}", obj_type(&obj)),
            Err::IdentifierNotFound(id) => write!(f, "identifier not found: {}", id),
            Err::IndexNotInteger(obj) => write!(f, "index not integer: {}", obj_type(&obj)),
            Err::WrongArgumentType(name, expected, got) => write!(
                f,
                "wrong argument type for {}: expected {}, got {}",
                name,
                expected,
                obj_type(&got)
            ),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Object::*;
        match self {
            Integer(i) => write!(f, "{}", i),
            Boolean(b) => write!(f, "{}", b),
            String(s) => write!(f, "{}", s),
            Null => write!(f, "null"),
            Return(r) => write!(f, "{}", r),
            Error(e) => write!(f, "{}", e),
            Function(fun) => write!(
                f,
                "fn({}) {{\n{}\n}}",
                fun.parameters
                    .iter()
                    .map(|p| p.token.literal())
                    .collect::<Vec<_>>()
                    .join(", "),
                fun.body
            ),
            Builtin(_) => write!(f, "<builtin function>"),
            Array(a) => write!(
                f,
                "[{}]",
                a.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            HashMap(h) => write!(
                f,
                "{{ {} }}",
                h.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

pub fn obj_type(obj: &Object) -> ObjectType {
    use Object::*;
    match obj {
        Integer(_) => ObjectType::Integer,
        Boolean(_) => ObjectType::Boolean,
        Null => ObjectType::Null,
        Return(_) => ObjectType::Return,
        Error(_) => ObjectType::Error,
        Function(_) => ObjectType::Function,
        String(_) => ObjectType::String,
        Builtin(_) => ObjectType::Builtin,
        Array(_) => ObjectType::Array,
        HashMap(_) => ObjectType::HashMap,
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

#[derive(Clone, Debug, PartialEq)]
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
