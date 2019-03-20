use crate::process::{Path, Proc, ProcCons::*, Value, ValueCons::*};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;

pub struct LinkerTable {
    current_surrogate_index: u64,
    table: HashMap<Path, u64>,
}

impl LinkerTable {
    pub fn new() -> Self {
        LinkerTable {
            current_surrogate_index: 0,
            table: HashMap::new(),
        }
    }

    pub fn get_surrogate(&mut self, p: Path) -> u64 {
        match self.table.entry(p) {
            Vacant(e) => {
                *(e.insert({
                    self.current_surrogate_index += 1;
                    self.current_surrogate_index
                }))
            }
            Occupied(e) => *(e.get()),
        }
    }

    /// Recursive helper method that descends into a `Proc` term and replaces
    /// all `Path` names by surrogates.
    fn link_proc(&mut self, process: &mut Proc) -> () {
        match process {
            Parallel(ps) => {
                for p in ps {
                    self.link_proc(p)
                }
            }
            Unfreeze(v) => self.link_value(v),
            Receive {
                channel: c,
                body: b,
                ..
            } => {
                self.link_value(c);
                self.link_proc(b);
            }
            Tell {
                channel: c,
                messages: ms,
            } => {
                self.link_value(c);
                for m in ms {
                    self.link_value(m);
                }
            }
            New { body: b, .. } => self.link_proc(b),
            BuiltInService(_) => { /* do nothing */ }
            MemReplacement => { /* do nothing */ }
        }
    }

    /// Recursive helper method that descends into a `Value` term and
    /// replaces all `Path` names by surrogates.
    fn link_value(&mut self, v: &mut Value) -> () {
        match v {
            DeBruijnIndex(_) => { /* do nothing */ }
            Freeze(p) => self.link_proc(p),
            PathName(_) => {
                if let PathName(p) = std::mem::replace(v, Depleted) {
                    *v = PathSurrogate(self.get_surrogate(p));
                } else {
                    panic!("Cannot happen.");
                }
            }
            PathSurrogate(s) => panic!(
                "Unexpected: `PathSurrogate{}` in a term that has not \
                 been linked yet. Where did that come from?",
                s
            ),
            UnforgeableName(_) => { /* do nothing */ }
            Depleted => { /* do nothing */ }
            BuiltInChannelName(_) => { /* do nothing */ }
            U => { /* do nothing */ }
            B(_) => { /* do nothing */ }
            I(_) => { /* do nothing */ }
            S(_) => { /* do nothing */ }
        }
    }

    /// Replaces all paths by shorter VM-local ids that
    /// are stored in this symbol table.
    pub fn link(&mut self, procs: &mut Vec<Proc>) -> () {
        for p in procs {
            self.link_proc(p);
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::process::Path;

    #[test]
    fn test_get_surrogate() {
        let a = Path(vec!["shadows".to_string(), "first".to_string()]);
        let b = Path(vec!["boiling".to_string(), "blood".to_string()]);
        let c = Path(vec!["umbral".to_string(), "planes".to_string()]);
        let mut t = LinkerTable::new();
        assert_eq!(t.get_surrogate(a.clone()), 1);
        assert_eq!(t.get_surrogate(b.clone()), 2);
        assert_eq!(t.get_surrogate(a.clone()), 1);
        assert_eq!(t.get_surrogate(b.clone()), 2);
        assert_eq!(t.get_surrogate(c.clone()), 3);
        assert_eq!(t.get_surrogate(a.clone()), 1);
        assert_eq!(t.get_surrogate(b.clone()), 2);
    }
}
