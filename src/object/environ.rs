use std::collections::HashMap;

use super::object::Object;

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
        match (&self.outer, self.store.get(name)) {
            (_, Some(obj)) => Some(obj),
            (Some(outer), None) => outer.get(name),
            _ => None,
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
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
