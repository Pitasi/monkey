use std::collections::HashMap;

use super::object::{obj_type, Builtin, Error, Integer, Object};

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
        "len" => Some(&Object::Builtin(Builtin {
            func: |args| {
                if args.len() != 1 {
                    return Object::Error(Error {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &args[0] {
                    Object::String(s) => Object::Integer(Integer {
                        value: s.value.len() as i64,
                    }),
                    _ => Object::Error(Error {
                        message: format!(
                            "argument to `len` not supported, got {}",
                            obj_type(&args[0])
                        ),
                    }),
                }
            },
        })),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use crate::object::object::Integer;

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
        env.set(name.to_string(), Object::Integer(Integer { value }));
    }

    fn get(env: &Environment, name: &str) -> Option<i64> {
        match env.get(name) {
            Some(Object::Integer(obj)) => Some(obj.value),
            _ => None,
        }
    }
}
