use std::collections::{HashSet, HashMap};

#[derive(Debug, Clone)]
pub struct Context { 
    pub var_map: HashMap<String, isize>,
    pub current_scope: HashSet<String>,
    pub stack_index: isize,
}

impl Context {
    pub fn new() -> Self {
        Context {
            var_map: HashMap::new(),
            current_scope: HashSet::new(),
            stack_index: -8,
        }
    }

    pub fn reset_scope(&self) -> Self {
        Context {
            var_map: self.var_map.clone(),
            current_scope: HashSet::new(),
            stack_index: self.stack_index,
        }
    }
}
