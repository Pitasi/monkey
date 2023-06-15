use std::collections::HashMap;

use super::object::{Err, Object, ObjectType};

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: Environment) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        let mut v = self.store.get(name);
        if v.is_some() {
            return v;
        }

        v = match &self.outer {
            Some(outer) => outer.get(name),
            None => None,
        };
        if v.is_some() {
            return v;
        }

        v = get_builtin(name);
        if v.is_some() {
            return v;
        }

        None
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}

fn get_builtin(name: &str) -> Option<&'static Object> {
    match name {
        "len" => Some(&Object::Builtin(|args| {
            if args.len() != 1 {
                return Object::Error(Err::WrongArgumentsCount(1, args.len()));
            }

            match &args[0] {
                Object::String(s) => Object::Integer(s.len() as i64),
                Object::Array(a) => Object::Integer(a.len() as i64),
                _ => Object::Error(Err::WrongArgumentType(
                    "len".to_string(),
                    ObjectType::Array,
                    Box::new(args[0].clone()),
                )),
            }
        })),
        "first" => Some(&Object::Builtin(|args| {
            if args.len() != 1 {
                return Object::Error(Err::WrongArgumentsCount(1, args.len()));
            }

            match &args[0] {
                Object::Array(a) => {
                    if a.len() > 0 {
                        return a[0].clone();
                    }
                    Object::Null
                }
                _ => Object::Error(Err::WrongArgumentType(
                    "first".to_string(),
                    ObjectType::Array,
                    Box::new(args[0].clone()),
                )),
            }
        })),
        "last" => Some(&Object::Builtin(|args| {
            if args.len() != 1 {
                return Object::Error(Err::WrongArgumentsCount(1, args.len()));
            }

            match &args[0] {
                Object::Array(a) => {
                    if a.len() > 0 {
                        return a.last().unwrap().clone();
                    }
                    Object::Null
                }
                _ => Object::Error(Err::WrongArgumentType(
                    "last".to_string(),
                    ObjectType::Array,
                    Box::new(args[0].clone()),
                )),
            }
        })),
        "rest" => Some(&Object::Builtin(|args| {
            if args.len() != 1 {
                return Object::Error(Err::WrongArgumentsCount(1, args.len()));
            }

            match &args[0] {
                Object::Array(a) => {
                    if a.len() == 0 {
                        return Object::Null;
                    }
                    let new_elements = a[1..].to_vec();
                    Object::Array(new_elements)
                }
                _ => Object::Error(Err::WrongArgumentType(
                    "last".to_string(),
                    ObjectType::Array,
                    Box::new(args[0].clone()),
                )),
            }
        })),
        "push" => Some(&Object::Builtin(|args| {
            if args.len() != 2 {
                return Object::Error(Err::WrongArgumentsCount(2, args.len()));
            }

            match &args[0] {
                Object::Array(a) => {
                    let mut new_elements = a.clone();
                    new_elements.push(args[1].clone());
                    Object::Array(new_elements)
                }
                _ => Object::Error(Err::WrongArgumentType(
                    "push".to_string(),
                    ObjectType::Array,
                    Box::new(args[0].clone()),
                )),
            }
        })),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::super::object::Object;
    use super::Environment;

    #[test]
    fn test_enclosed_environment() {
        let mut env = Environment::new();
        set(&mut env, "a", 1);
        set(&mut env, "b", 1);

        let mut env2 = Environment::new_enclosed_environment(env.clone());
        set(&mut env2, "b", 42);
        set(&mut env2, "c", 2);
        assert_eq!(1, get(&env2, "a").unwrap());
        assert_eq!(42, get(&env2, "b").unwrap());
        assert_eq!(2, get(&env2, "c").unwrap());
        assert_eq!(1, get(&env, "b").unwrap());
    }

    fn set(env: &mut Environment, name: &str, value: i64) {
        env.set(name.to_string(), Object::Integer(value));
    }

    fn get(env: &Environment, name: &str) -> Option<i64> {
        match env.get(name) {
            Some(Object::Integer(obj)) => Some(*obj),
            _ => None,
        }
    }
}
