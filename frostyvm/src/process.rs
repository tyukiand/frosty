use crate::built_in::BuiltInChannel;
use crate::comm_queue::CommQueue;
use crate::prettyprint::PrettyPrintable;
use crate::prettyprint::PrettyPrintable::*;
use std::fmt;
use std::fmt::Display;

use uuid::Uuid;

#[derive(Clone, Debug, PartialEq)]
pub enum ProcCons<Binders> {
    Parallel(Vec<ProcCons<Binders>>),
    Unfreeze(ValueCons<Binders>),
    Receive {
        channel: ValueCons<Binders>,
        binders: Binders,
        body: Box<ProcCons<Binders>>,
    },
    New {
        binders: Binders,
        body: Box<ProcCons<Binders>>,
    },
    Tell {
        channel: ValueCons<Binders>,
        messages: Vec<ValueCons<Binders>>,
    },

    // Dummy values used at various points
    BuiltInService(BuiltInChannel),
    MemReplacement,
}

use crate::process::ProcCons::*;

#[derive(PartialEq, Clone, Debug, Eq, Hash)]
pub struct Path(pub Vec<String>);

/// Process with variable occurrence counts saved in the binders, for efficient
/// substitution in environments without garbage collector.
pub type Proc = ProcCons<Vec<u32>>;

/// Implements methods for processes that store occurrence counts in the
/// binders. The occurrence counts are necessary so that no superfluous
/// clones have to be generated during substitution.
impl Proc {
    fn substitute(
        &mut self,
        replacement: &mut Vec<Value>,
        depth: u32,
        remaining_copies: &mut Vec<u32>,
    ) {
        match self {
            Parallel(ps) => {
                for p in ps {
                    p.substitute(replacement, depth, remaining_copies);
                }
            }
            Unfreeze(v) => {
                v.substitute(replacement, depth, remaining_copies);
            }
            Receive {
                channel: c,
                binders: ocs,
                body: b,
            } => {
                c.substitute(replacement, depth, remaining_copies);
                b.substitute(
                    replacement,
                    depth + ocs.len() as u32,
                    remaining_copies,
                );
            }
            New {
                binders: ocs,
                body: b,
            } => {
                b.substitute(
                    replacement,
                    depth + ocs.len() as u32,
                    remaining_copies,
                );
            }
            Tell {
                channel: c,
                messages: ms,
            } => {
                c.substitute(replacement, depth, remaining_copies);
                for m in ms {
                    m.substitute(replacement, depth, remaining_copies);
                }
            }
            BuiltInService(_) => {}
            MemReplacement => {}
        }
    }

    fn beta(&mut self, mut values: Vec<Value>) {
        let owned = std::mem::replace(self, MemReplacement);
        let (mut ocs, mut body) = match owned {
            Receive {
                binders: ocs,
                body: b,
                ..
            } => (ocs, *b),
            New {
                binders: ocs,
                body: b,
            } => (ocs, *b),
            _ => {
                panic!("Invoked `beta`-rule on a non-binder struct: {:?}", self)
            }
        };
        assert_eq!(ocs.len(), values.len());
        body.substitute(&mut values, ocs.len() as u32, &mut ocs);
        *self = body;
    }

    /// Recursive helper method for `explode`.
    fn explode_rec(
        self,
        rcv_snd_buffer: &mut Vec<Proc>,
        new_buffer: &mut Vec<Proc>,
    ) {
        match self {
            Parallel(ps) => {
                for p in ps.into_iter() {
                    p.explode_rec(rcv_snd_buffer, new_buffer);
                }
            }
            Unfreeze(Freeze(p)) => p.explode_rec(rcv_snd_buffer, new_buffer),
            Unfreeze(sth_else) => panic!(
                "Corrupt bytecode: `Unfreeze` was expected to contain \
                 `Freeze`, but contained {:?}",
                sth_else
            ),
            r @ Receive { .. } => rcv_snd_buffer.push(r),
            s @ Tell { .. } => rcv_snd_buffer.push(s),
            n @ New { .. } => new_buffer.push(n),
            BuiltInService(n) => panic!(
                "BuiltInService-process with name {:?} somehow ended up in a \
                 place where `explode_rec` has been invoked on it. This should \
                 never happen.",
                n
            ),
            MemReplacement => {
                panic!(
                    "Attempted to `explode` the dummy element `MemReplacement`."
                );
            }
        }
    }

    /// Eliminates `unfreeze-freeze`, flattens out nested `Parallel` processes,
    /// keeps reducing everything until only `Receive`s, `Tell`s and `New`s
    /// remain. The sends and receives are returned in first buffer. News
    /// are returned separately in the second buffer.
    /// No `Parallel` or `Unfreeze` processes will appear in the result.
    pub fn explode(self) -> (Vec<Proc>, Vec<Proc>) {
        let mut rcv_snd_buffer = Vec::new();
        let mut new_buffer = Vec::new();
        self.explode_rec(&mut rcv_snd_buffer, &mut new_buffer);
        (rcv_snd_buffer, new_buffer)
    }

    /// Returns number of bound variables in `New` if it is one, and `None` if
    /// this proc is not `New`.
    fn num_new_vars(&self) -> Option<usize> {
        match self {
            New { binders: ocs, .. } => Some(ocs.len()),
            _ => None,
        }
    }

    /// Performs substitution on this term, replacing all bound variables by
    /// fresh unforgeable names. This operation makes sense only on `New`,
    /// and will panic for any other process.
    pub fn supply_unforgeables(&mut self) {
        let num_vars = self.num_new_vars().expect("Should be a `New`");
        let uuids = (0..num_vars)
            .map(|_i| Value::new_unforgeable_name())
            .collect();
        self.beta(uuids);
    }

    /// Repeatedly applies `comm`-rule until no two pairs of processes can
    /// be combined any more.
    /// Expects a vector that consists only of `Receive`s and `Tell`s.
    /// Returns the irreducible residuum.
    pub fn reduce_until_local_convergence(rcv_snds: Vec<Proc>) -> Vec<Proc> {
        let mut comm_queue = CommQueue::new();
        let mut news_queue = Vec::new(); // stack, but call it "queue" anyway

        // enqueue all processes in the `comm` queue
        for p in rcv_snds.into_iter() {
            comm_queue.push_back(p);
        }

        let mut progress = true;

        // as long as anything changes, repeatedly keep clearing both queues
        while progress {
            progress = false;

            // clear the queue with `(receive, send)`-pairs
            while let Some((mut rcv, snd)) = comm_queue.pop_front() {
                progress = true;
                if let Tell { messages: ms, .. } = snd {
                    match rcv {
                        // "delta reduction"
                        BuiltInService(b) => {
                            let (rcv_snd, news) =
                                b.handle_request(ms).explode();
                            for r in rcv_snd.into_iter() {
                                comm_queue.push_back(r);
                            }
                            news_queue.extend(news);
                        }
                        // "beta reduction"
                        _ => {
                            rcv.beta(ms);
                            let (rcv_snd, news) = rcv.explode();
                            for p in rcv_snd.into_iter() {
                                comm_queue.push_back(p);
                            }
                            news_queue.extend(news);
                        }
                    }
                } else {
                    panic!(
                        "Fatal error: second component of a pair dequed from a \
                         `CommQueue` was not a `Tell`: {:?}",
                        snd
                    )
                }
            }

            // clear the queue with `new`-binders by feeding UUIDs to them
            while let Some(mut n) = news_queue.pop() {
                progress = true;
                n.supply_unforgeables();
                let (rcv_snd, news) = n.explode();
                for p in rcv_snd.into_iter() {
                    comm_queue.push_back(p);
                }
                news_queue.extend(news);
            }
        }

        comm_queue.harvest()
    }

    /// Attempts to get the channel on which a process is sending or listening.
    ///
    /// Returns `Some` value only if the channel is a `Receive` or a `Tell`,
    /// otherwise returns `None`.
    ///
    /// Does not check whether the channel is an actual name, or some random
    /// unevaluated expression - checking it is left to the caller.
    pub fn peek_channel(&self) -> Option<&Value> {
        // This method caused: 1 bugs (last updated 2019-02-17)
        match self {
            Receive { channel: c, .. } => Some(c),
            Tell { channel: c, .. } => Some(c),
            _ => None,
        }
    }

    pub fn is_built_in(&self) -> bool {
        match self {
            BuiltInService(_) => true,
            _ => false
        }
    }
}

impl From<&Proc> for PrettyPrintable {
    fn from(p: &Proc) -> PrettyPrintable {
        match p {
            Parallel(ps) => Delimited {
                start: "{".to_string(),
                content: ps
                    .iter()
                    .enumerate()
                    .map(|(idx, p)| {
                        if idx == ps.len() - 1 {
                            p.into()
                        } else {
                            Juxtaposition(vec![
                                p.into(),
                                Atom(" | ".to_string()),
                            ])
                        }
                    })
                    .collect(),
                end: "}".to_string(),
            },
            Tell {
                channel: c,
                messages: ms,
            } => Juxtaposition(vec![
                c.into(),
                Delimited {
                    start: "![".to_string(),
                    content: ms.iter().map(|m| m.into()).collect(),
                    end: "]".to_string(),
                },
            ]),
            Receive {
                channel: c,
                binders: ocs,
                body: b,
            } => {
                let num_binders = ocs.len();
                Juxtaposition(vec![
                    c.into(),
                    Atom("?".to_string()),
                    Delimited {
                        start: "[".to_string(),
                        content: 
                            ocs
                            .iter()
                            .enumerate()
                            .map(move |(idx, count)| if idx < num_binders - 1 {
                                Juxtaposition(vec![Atom(format!("{}", *count)), Atom(",".to_string())])
                            } else {
                                Atom(format!("{}", *count))
                            }).collect(),
                        end: "]".to_string()
                    },
                    Atom(".".to_string()),
                    b.as_ref().into(),
                ])
            }
            New {
                binders: ocs,
                body: b,
            } => {
                Juxtaposition(vec![
                    Atom("new".to_string()),
                    Delimited {
                        start: "[".to_string(),
                        content: 
                            ocs.iter().map(|i| Atom(format!("{}", i))).collect(),
                        end: "]".to_string()
                    },
                    b.as_ref().into(),
                ])
            },
            Unfreeze(v) => Delimited {
                start: "~(".to_string(),
                content: vec![v.into()],
                end: ")".to_string(),
            },
            BuiltInService(n) => {
                Juxtaposition(vec![Atom("builtin-rcv:".to_string()), n.into()])
            }
            MemReplacement => Atom("<MemReplacementDummy>".to_string()),
        }
    }
}

impl Display for Proc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = PrettyPrintable::from(self).pretty_print(60);
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ValueCons<Binders> {
    DeBruijnIndex(u32),
    Freeze(Box<ProcCons<Binders>>),

    // built-in values
    U,
    B(bool),
    I(i32),
    S(String),

    // Representable names (path names eliminated by linker)
    PathName(Path),
    BuiltInChannelName(BuiltInChannel),

    // Unrepresentable names used internally by the virtual machine
    PathSurrogate(u64),
    UnforgeableName(Uuid),

    // Placeholder used during substitutions / beta-rule applications,
    // needed for mem::replace. Not representable, should never occur
    // in real terms.
    Depleted,
}

pub type Value = ValueCons<Vec<u32>>;

use crate::process::ValueCons::*;

impl Value {
    fn new_unforgeable_name() -> Value {
        UnforgeableName(Uuid::new_v4())
    }

    fn substitute(
        &mut self,
        replacement: &mut Vec<Value>,
        depth: u32, // how many bound vars are above this point
        remaining_copies: &mut Vec<u32>,
    ) {
        let num_vars = replacement.len();
        match self {
            DeBruijnIndex(i) => {
                // `j` is the depth on which the referenced var is, the 
                // de-Bruijn index `i` is 1-based, that is, 1 corresponds to 
                // the most recently pushed variable.
                let j = (depth - *i) as usize;
                if j < num_vars {
                    if remaining_copies[j] > 1 {
                        let v = replacement[j].clone();
                        if let Depleted = v {
                            panic!("Fatal error: `Depleted` in var binding.");
                        }
                        *self = v;
                        remaining_copies[j] -= 1;
                    } else if remaining_copies[j] == 1 {
                        *self =
                            std::mem::replace(&mut replacement[j], Depleted);
                        remaining_copies[j] = 0;
                    } else {
                        panic!(
                            "Bytecode corrupted: \
                             invalid number of variable occurrences,  \
                             at depth = {}, dbi = {}, self = {:?}",
                            depth, j, self
                        );
                    }
                }
            }
            Freeze(p) => p.substitute(replacement, depth, remaining_copies),
            PathName(_) => {}
            PathSurrogate(_) => {}
            UnforgeableName(_) => {}
            BuiltInChannelName(_) => {}
            Depleted => { /* thoughtfully & carefully do nothing */ }
            U => { /* do nothing */ }
            B(_) => { /* do nothing */ }
            I(_) => { /* do nothing */ }
            S(_) => { /* do nothing */ }
        }
    }

    pub fn is_channel(&self) -> bool {
        match self {
            PathName(_) => true,
            BuiltInChannelName(_) => true,
            PathSurrogate(_) => true,
            UnforgeableName(_) => true,
            _ => false
        }
    }
}

impl From<&Value> for PrettyPrintable {
    fn from(v: &Value) -> PrettyPrintable {
        match v {
            Freeze(p) => p.as_ref().into(),
            PathName(path) => Atom(
                path.0
                    .iter()
                    .map(|x| format!("/{}", x))
                    .collect::<Vec<String>>()
                    .join(""),
            ),
            DeBruijnIndex(i) => Atom(format!("\u{2768}{}\u{2769}", i)),
            PathSurrogate(x) => Atom(format!("\u{27ec}{}\u{27ed}", x)),
            UnforgeableName(u) => Atom(format!("{}", u)),
            BuiltInChannelName(c) => c.into(),
            Depleted => Atom("<depleted>".to_string()),
            U => Atom("()".to_string()),
            B(b) => Atom(format!("{}", b)),
            I(i) => Atom(format!("{}", i)),
            S(s) => Atom(format!("\"{}\"", s.to_string())),
        }
    }
}

//=============================================================================

#[cfg(test)]
mod tests {

    use super::Path;
    use super::Proc;
    use super::ProcCons::*;
    use super::ValueCons::*;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    #[test]
    fn test_path_eq() {
        let a = Path(vec!["foo".to_string(), "bar".to_string()]);
        let b = Path(vec!["foo".to_string(), "bar".to_string()]);
        let c = Path(vec!["bar".to_string(), "baz".to_string()]);
        assert_eq!(a, a);
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert_ne!(b, c);

        let ha = {
            let mut hasher = DefaultHasher::new();
            a.hash(&mut hasher);
            hasher.finish()
        };

        let hb = {
            let mut hasher = DefaultHasher::new();
            b.hash(&mut hasher);
            hasher.finish()
        };

        assert_eq!(ha, hb);
    }

    #[test]
    fn test_de_bruijn_indexing() {
        let p = proc! {
            (receive (/c) [x, y, z]
                        // 0  1  2  (depth)
                        // 2  4  1  (occurrence counts)
                (new [u, v]
                   // 3  4  (depth)
                   // 1  1  (occurrence counts)
                    (send (/e) [x, y, z, u, v, x, y, y, y])
                             // 5  4  3  2  1  5  4  4  4 (de-Bruijn indices)
                )
            )
        };

        assert_eq!(
            p,
            Receive {
                channel: value! { (/c) },
                binders: vec![2, 4, 1],
                body: Box::new(New {
                    binders: vec![1, 1],
                    body: Box::new(Tell {
                        channel: value! { (/e) },
                        messages: [5, 4, 3, 2, 1, 5, 4, 4, 4]
                            .into_iter()
                            .map(|i| DeBruijnIndex(*i))
                            .collect()
                    })
                })
            }
        )
    }

    #[test]
    fn test_beta_run_id() {
        // listens on channel `/c`, unfreezes and runs whatever it receives
        // as single argument.
        let mut p = proc! { (receive (/c) [x] (unfreeze x)) };

        assert_eq!(
            p,
            Receive {
                channel: PathName(Path(vec!["c".to_string()])),
                binders: vec![1],
                body: Box::new(Unfreeze(DeBruijnIndex(1))) // dbi-1-based
            }
        );

        // frozen terminated process
        let q = value! { (freeze {}) };

        p.beta(vec![q]);
        let expected = proc! { (unfreeze (freeze {})) };
        assert_eq!(p, expected);
    }

    #[test]
    fn test_whitebox_substitute_repeated_values() {
        let p = proc! {
            (receive (/c) [x, y, z]
                (new [u, v]
                    (send (/e) [x, y, z, u, v, x, y, y, y])
                )
            )
        };

        if let Receive {
            body: b,
            binders: mut occ_counts,
            ..
        } = p
        {
            let mut body = *b;
            let mut values = vec![value! { 42 }, value! { 58 }, value! { 100 }];
            body.substitute(&mut values, 3, &mut occ_counts);
            assert_eq!(
                body,
                proc! {
                    (new [u, v]
                        (send (/e) [42, 58, 100, u, v, 42, 58, 58, 58])
                    )
                }
            );
            for i in 0..values.len() {
                assert_eq!(&values[i], &Depleted);
                assert_eq!(occ_counts[i], 0);
            }
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_beta_repeated_values() {
        let mut p = proc! {
            (receive (/c) [x, y, z]
                (new [u, v] (send (/e) [x, y, z, u, v, x, y, y, y]))
            )
        };

        let values = vec![value! { 42 }, value! { 58 }, value! { 100 }];
        p.beta(values);
        assert_eq!(
            p,
            proc! {
                (new [u, v]
                    (send (/e) [42, 58, 100, u, v, 42, 58, 58, 58])
                )
            }
        );
    }

    #[test]
    fn test_explode() {
        let p = proc! { {
            (unfreeze (freeze (unfreeze (freeze (send (/c) [42])))))
            {
                (receive (/c) [x] (unfreeze x))
                (unfreeze (freeze (send (/x) [58])))
            }
            (unfreeze (freeze (unfreeze (freeze (send (/x) [142])))))
            (unfreeze (freeze (new [x, y] (send x [y]))))
        }};

        let (rcv_snd, news) = p.explode();

        assert_eq!(
            rcv_snd,
            vec![
                proc! { (send (/c) [42]) },
                proc! { (receive (/c) [x] (unfreeze x)) },
                proc! { (send (/x) [58]) },
                proc! { (send (/x) [142]) },
            ]
        );
        assert_eq!(news, vec![proc! { (new [x, y] (send x [y])) },]);
    }

    #[test]
    fn test_local_comm_rule_single_step_empty_unfreeze_freeze() {
        let res = Proc::reduce_until_local_convergence(vec![
            proc! { (send (surrogate 42) [(freeze {})]) },
            proc! { (receive (surrogate 42) [x] (unfreeze x)) },
        ]);

        assert_eq!(res, vec![]);
    }

    #[test]
    fn test_local_comm_rule_new_nested_pars() {
        // CS-XVIp72
        let res = Proc::reduce_until_local_convergence(vec![
            proc! {
                (receive (surrogate 0) [p] {
                    (receive (surrogate 1) [c] {
                        (send c [(surrogate 42)])
                        {}
                    })
                    (unfreeze p)
                })
            },
            proc! {
                (send (surrogate 0) [ (freeze {
                    {}
                    (new [a] {
                        (send (surrogate 1) [a])
                        (receive a [x] {
                            (send x [(surrogate 58)])
                        })
                    })
                })])
            },
        ]);
        let expected_single_process = proc! {
            (send (surrogate 42) [(surrogate 58)])
        };
        assert_eq!(res, vec![expected_single_process]);
    }
}
