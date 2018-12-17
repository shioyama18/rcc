use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Default)]
pub struct Context {
    pub var_map: HashMap<String, isize>,
    pub current_scope: HashSet<String>,
    pub stack_index: isize,
    pub break_label: Option<String>,
    pub continue_label: Option<String>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            stack_index: -8,
            ..Default::default()
        }
    }

    pub fn reset_scope(&self) -> Self {
        Context {
            current_scope: HashSet::new(),
            ..self.clone()
        }
    }
}
