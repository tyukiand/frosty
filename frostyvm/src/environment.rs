use std::collections::HashMap;
use std::hash::Hash;

pub struct Env<N, V>(HashMap<N, V>);

impl<N, V> Env<N, V>
where
    N: Clone + Hash + Eq,
{
    pub fn new() -> Env<N, V> {
        Env(HashMap::new())
    }

    pub fn get_mut(&mut self, name: &N) -> Option<&mut V> {
        self.0.get_mut(name)
    }

    pub fn with_variables<Z, B, F>(
        &mut self,
        variable_names: Vec<N>,
        create_binding: B,
        f: F,
    ) -> (Z, Vec<V>)
    where
        B: Fn(usize, &N) -> V,
        F: FnOnce(&mut Self) -> Z,
    {
        let mut shadowed = Vec::new();
        {
            // Modify the environment, save the shadowed bindings so
            // we can revert the changes later.
            for (idx, name) in variable_names.iter().enumerate() {
                let binding = create_binding(idx, name);
                let name_clone = name.clone();
                self.0.insert(name_clone, binding).into_iter().for_each(
                    |old_binding| {
                        shadowed.push((idx, old_binding));
                    },
                );
            }
        }

        // invoke `f`
        let z_result = f(self);

        // pull the `V`s from the hash map
        let v_result: Vec<V> = variable_names
            .iter()
            .map(|n| self.0.remove(n).unwrap())
            .collect();

        // revert changes: put the shadowed values back into self
        for (i, restored_value) in shadowed.into_iter() {
            let restored_name = variable_names[i].clone();
            self.0.insert(restored_name, restored_value);
        }

        (z_result, v_result)
    }
}
