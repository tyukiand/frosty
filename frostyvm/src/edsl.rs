use crate::environment::Env;
use crate::process::{Path, Proc, ProcCons, Value, ValueCons};

#[derive(Debug, PartialEq)]
pub enum FoProc {
    Parallel(Vec<FoProc>),
    Unfreeze(FoValue),
    Receive {
        channel: FoValue,
        variables: Vec<String>,
        body: Box<FoProc>,
    },
    New {
        variables: Vec<String>,
        body: Box<FoProc>,
    },
    Tell {
        channel: FoValue,
        messages: Vec<FoValue>,
    },
}

#[derive(Debug)]
pub struct Binder {
    depth: u32,
    occurrences_count: u32,
}

impl Binder {
    /* TODO: CRUFT?
    fn new(depth: u32) -> Binder {
        Binder {
            depth,
            occurrences_count: 0,
        }
    }
    */
}

use crate::edsl::FoProc::*;

impl FoProc {
    pub fn to_de_bruijn(
        self,
        env: &mut Env<String, Binder>,
        curr_depth: u32,
    ) -> Proc {
        match self {
            Parallel(ps) => ProcCons::Parallel(
                ps.into_iter()
                    .map(|p| p.to_de_bruijn(env, curr_depth))
                    .collect(),
            ),
            Unfreeze(v) => ProcCons::Unfreeze(v.to_de_bruijn(env, curr_depth)),
            Receive {
                channel: c,
                variables: vs,
                body: b,
            } => {
                let num_vars = vs.len();
                let db_chan = c.to_de_bruijn(env, curr_depth);

                let (db_body, occ_counts) = env.with_variables(
                    vs,
                    |idx, _name_ref| Binder {
                        depth: curr_depth + idx as u32,
                        occurrences_count: 0u32,
                    },
                    move |upd_env| {
                        b.to_de_bruijn(upd_env, curr_depth + (num_vars as u32))
                    },
                );
                ProcCons::Receive {
                    channel: db_chan,
                    binders: occ_counts
                        .into_iter()
                        .map(|b| b.occurrences_count)
                        .collect(),
                    body: Box::new(db_body),
                }
            }
            New {
                variables: vs,
                body: b,
            } => {
                let num_vars = vs.len();
                let (db_body, occ_counts) = env.with_variables(
                    vs,
                    |idx, _name_ref| Binder {
                        depth: curr_depth + idx as u32,
                        occurrences_count: 0u32,
                    },
                    move |upd_env| {
                        b.to_de_bruijn(upd_env, curr_depth + (num_vars as u32))
                    },
                );
                ProcCons::New {
                    binders: occ_counts
                        .into_iter()
                        .map(|b| b.occurrences_count)
                        .collect(),
                    body: Box::new(db_body),
                }
            }
            Tell {
                channel: c,
                messages: ms,
            } => ProcCons::Tell {
                channel: c.to_de_bruijn(env, curr_depth),
                messages: ms
                    .into_iter()
                    .map(|m| m.to_de_bruijn(env, curr_depth))
                    .collect(),
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FoValue {
    Variable(String),
    Freeze(Box<FoProc>),
    PathName(Path),
    PathSurrogate(u64),
    I(i32),
}

use crate::edsl::FoValue::*;

impl FoValue {
    pub fn to_de_bruijn(
        self,
        env: &mut Env<String, Binder>,
        curr_depth: u32,
    ) -> Value {
        match self {
            Variable(v) => match env.get_mut(&v) {
                Some(binder) => {
                    println!(
                        "binder for {} is {:?}, curr_depth = {}, dbi = {}",
                        &v,
                        &binder,
                        curr_depth,
                        curr_depth - binder.depth - 1
                    );
                    binder.occurrences_count += 1;
                    ValueCons::DeBruijnIndex(curr_depth - binder.depth)
                }
                None => panic!("Unbound var {}", v),
            },
            Freeze(p) => {
                ValueCons::Freeze(Box::new((*p).to_de_bruijn(env, curr_depth)))
            }
            PathName(p) => ValueCons::PathName(p),
            PathSurrogate(n) => ValueCons::PathSurrogate(n),
            I(i) => ValueCons::I(i),
        }
    }
}

impl From<i32> for FoValue {
    fn from(x: i32) -> FoValue {
        I(x)
    }
}

macro_rules! proc {
    ($p: tt) => {
        foProc!($p).to_de_bruijn(&mut $crate::environment::Env::new(), 0)
    };
}

macro_rules! value {
    ($v: tt) => {
        foValue!($v).to_de_bruijn(&mut $crate::environment::Env::new(), 0)
    };
}

macro_rules! foProc {
    ({$($p: tt)*}) => {
        $crate::edsl::FoProc::Parallel(vec![$(foProc!{$p}),*])
    };
    ((unfreeze $x: tt)) => { $crate::edsl::FoProc::Unfreeze(foValue!{ $x }) };
    ((receive $c: tt [$($v: ident),*] $b: tt)) => {
        $crate::edsl::FoProc::Receive {
            channel: foValue!{$c},
            variables: vec![$(stringify!($v).to_string()),*],
            body: Box::new(foProc!{ $b })
        }
    };
    ((send $c: tt [$($msg: tt),*])) => {
        $crate::edsl::FoProc::Tell {
            channel: foValue!{$c},
            messages: vec![$(foValue!{$msg}),*]
        }
    };
    ((new [$($v: ident),*] $b: tt)) => {
        $crate::edsl::FoProc::New {
            variables: vec![$(stringify!($v).to_string()),*],
            body: Box::new(foProc!{ $b })
        }
    };
}

macro_rules! foValue {
    ((surrogate $num: tt)) => {
        $crate::edsl::FoValue::PathSurrogate($num)
    };
    ($name: ident) => {
        $crate::edsl::FoValue::Variable(stringify!($name).to_string())
    };
    ((freeze $p: tt)) => {
        $crate::edsl::FoValue::Freeze(Box::new(foProc!{ $p }))
    };
    (($(/ $pathComponent: ident)*)) => {
        $crate::edsl::FoValue::PathName(
            $crate::process::Path(vec![
                $(stringify!($pathComponent).to_string()),*
            ])
        )
    };
    ($expr: expr) => { $crate::edsl::FoValue::from($expr) }
}

#[cfg(test)]
mod tests {

    use super::FoProc::*;
    use super::FoValue::*;
    use crate::process::Path;

    #[test]
    fn test_macro_basic_variable() {
        assert_eq!(foValue!(hello), Variable("hello".to_string()));
        assert_eq!(
            foProc! { (unfreeze x) },
            Unfreeze(Variable("x".to_string()))
        );
    }

    #[test]
    fn test_macro_freeze_unfreeze() {
        assert_eq!(
            foValue! { (freeze (unfreeze x)) },
            Freeze(Box::new(Unfreeze(Variable("x".to_string()))))
        );
    }

    #[test]
    fn test_macro_path() {
        assert_eq!(
            foValue! { (/foo/bar/baz) },
            PathName(Path(vec![
                "foo".to_string(),
                "bar".to_string(),
                "baz".to_string()
            ]))
        );
    }

    #[test]
    fn test_macro_parallel_cons() {
        assert_eq!(
            foProc! {
                {(unfreeze x) (unfreeze y) (unfreeze z)}
            },
            Parallel(
                ["x", "y", "z"]
                    .iter()
                    .map(|r| Unfreeze(Variable(r.to_string())))
                    .collect()
            )
        );
    }

    #[test]
    fn test_macro_send_receive() {
        assert_eq!(
            foProc! {
                (receive (/ski/k) [x, y] (unfreeze x))
            },
            Receive {
                channel: PathName(Path(vec![
                    "ski".to_string(),
                    "k".to_string()
                ])),
                variables: vec!["x".to_string(), "y".to_string()],
                body: Box::new(Unfreeze(Variable("x".to_string())))
            }
        );

        assert_eq!(
            foProc! {
                (send (/a/b) [x, y, (freeze (unfreeze z))])
            },
            Tell {
                channel: PathName(Path(vec!["a".to_string(), "b".to_string()])),
                messages: vec![
                    Variable("x".to_string()),
                    Variable("y".to_string()),
                    Freeze(Box::new(Unfreeze(Variable("z".to_string()))))
                ]
            }
        );
    }

    #[test]
    fn test_macro_new() {
        assert_eq!(
            foProc! { (new [x, y] (unfreeze x)) },
            New {
                variables: vec!["x".to_string(), "y".to_string()],
                body: Box::new(Unfreeze(Variable("x".to_string())))
            }
        );
    }

    #[test]
    fn test_macro_expr_values() {
        assert_eq!(
            foProc! { (send (/a) [42, 58]) },
            Tell {
                channel: foValue! { (/a) },
                messages: vec![I(42), I(58)]
            }
        )
    }
}
