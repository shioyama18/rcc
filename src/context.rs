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
    pub fn new(params: &Vec<String>) -> Self {
        let mut var_map = HashMap::new();
        let mut current_scope = HashSet::new();
        let mut param_offset = 16;

        params.iter().for_each(|id| {
            var_map.insert(id.clone(), param_offset);
            current_scope.insert(id.clone());
            param_offset += 8;
        });

        Context {
            var_map: var_map,
            stack_index: -8,
            current_scope: current_scope,
            ..Default::default()
        }
    }

    pub fn reset_scope(&self) -> Self {
        Context {
            current_scope: HashSet::new(),
            ..self.clone()
        }
    }

    pub fn add_var(&mut self) {}
}
