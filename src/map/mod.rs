#[derive(Debug, Clone, PartialEq)]
pub struct InefficientMap<K, V> {
    pub keys: Vec<K>,
    pub values: Vec<V>,
}

impl<K: PartialEq, V> InefficientMap<K, V> {
    pub fn new() -> Self {
        Self {
            keys: Vec::new(),
            values: Vec::new(),
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.keys
            .iter()
            .enumerate()
            .find(|(_, k)| *k == key)
            .map(|(i, _)| &self.values[i])
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.keys.push(key);
        self.values.push(value);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.keys.iter().zip(self.values.iter())
    }

    pub fn len(&self) -> usize {
        self.keys.len()
    }
}
