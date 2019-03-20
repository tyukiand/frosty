use crate::prettyprint::PrettyPrintable;
use crate::prettyprint::PrettyPrintable::Atom;
use crate::process::{Proc, Value, ProcCons, ValueCons};

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum BuiltInChannel {
    EqU,
    UToS,
    If,
    EqB,
    AndB,
    OrB,
    NotB,
    XorB,
    BToS,
    EqI,
    LeI,
    LeqI,
    GeI,
    GeqI,
    AddI,
    NegI,
    SubI,
    MulI,
    DivI,
    RemI,
    IToS,
    EqS,
    ConcatS,
    StdOut,
    StdErr,
    StdIn,
}

use crate::built_in::BuiltInChannel::*;

macro_rules! check_arity {
    ($name: ident, $n: expr, $msgs: expr) => {
        let n_got = $msgs.len();
        if $n != n_got {
            panic!(
                "Wrong number of arguments for {}, expected {}, got {}.",
                stringify!($name),
                $n,
                n_got
            )
        }
    };
}

macro_rules! check_channel {
    ($name: ident, $ch: expr) => {
        if !($ch).is_channel() {
            panic!(
                "Built-In {} expected a channel argument, \
                but got {:?} instead.",
                stringify!($name),
                $ch
            );
        }
    }
}

macro_rules! check_binop_input {
    ($name: ident, $msgs: expr) => {
        check_arity!($name, 3, $msgs);
        check_channel!($name, $msgs[2]);
    }
}

macro_rules! check_unop_input {
    ($name: ident, $msgs: expr) => {
        check_arity!($name, 2, $msgs);
        check_channel!($name, $msgs[1]);
    }
}

macro_rules! take_args {
    ($($xs: ident),* ; $arr: expr) => {
        take_args!([$($xs)*] [] $arr)
    };
    ([$x: ident $($xs: ident)*] [$($ys: ident)*] $arr: expr) => {
        take_args!([$($xs)*] [$x $($ys)*] $arr)
    };
    ([] [$($xs: ident)*] $arr: expr) => {
        $(let $xs = $arr.pop().unwrap();)*
    };
    
}

/** Right hand side for unary operations on `Copy` types (bools, ints etc.) */
macro_rules! copy_unop {
    ($opName: ident, $msgs: expr, $typIn: ident, $typOut: ident, $op: tt) => {{
        check_unop_input!($opName, &$msgs);
        take_args!(a, ret; $msgs);
        if let $typIn(x) = &a {
            Tell { channel: ret, messages: vec![$typOut($op *x)] }
        } else {
            panic!(
                "Unexpected input for `{}`: {:?}",
                stringify!($opName),
                a
            );
        }
    }}
}

/** Right hand side for binary operations on `Copy` types (bools, ints etc.) */
macro_rules! copy_binop {
    ($opName: ident, $msgs: expr, $typIn: ident, $typOut: ident, $op: tt) => {{
        check_binop_input!($opName, &$msgs);
        take_args!(a, b, ret; $msgs);
        if let ($typIn(x), $typIn(y)) = (&a, &b) {
            Tell { channel: ret, messages: vec![$typOut(*x $op *y)] }
        } else {
            panic!(
                "Unexpected inputs for `{}`: {:?} {:?} {:?}",
                stringify!($opName),
                a,
                b,
                ret
            );
        }
    }}
}

use ValueCons::*;
use ProcCons::*;

impl BuiltInChannel {

    pub fn handle_request(&self, mut ms: Vec<Value>) -> Proc {
        match self {
            EqU => {
                check_binop_input!(EqU, &ms);
                let ret = ms.pop().unwrap();
                let b = ms.pop().unwrap();
                let a = ms.pop().unwrap();
                if let (U, U) = (&a, &b) {
                    Tell { channel: ret, messages: vec![ B(true) ] }
                } else {
                    panic!(
                        "EqU expected two units, got {:?} {:?}.",
                        a,
                        b
                    )
                }
            },
            UToS => {
                check_unop_input!(UToS, &ms);
                take_args!(a, ret; ms);
                if let U = &a {
                    Tell {
                        channel: ret,
                        messages: vec![ S("()".to_string()) ]
                    }
                } else {
                    panic!("UToS unexpected input: {:?}", a);
                }
            },
            If => {
                check_arity!(If, 3, &ms);
                take_args!(c, t, e; ms);
                match (c, t, e) {
                    (B(cnd), Freeze(thn), Freeze(els)) => {
                        if cnd {
                            *thn
                        } else {
                            *els
                        }
                    },
                    (x, y, z) => {
                        panic!(
                            "Unexpected inputs for `If`-else:\n\
                             [[[cond]]]`{:?}`\n\
                             [[[then]]]`{:?}`\n\
                             [[[else]]]`{:?}\
                             Expected boolean and two frozen processes.",
                            x,
                            y,
                            z
                        );
                    }
                }
            },
            EqB => copy_binop!(EqB, ms, B, B, ==),
            AndB => copy_binop!(AndB, ms, B, B, &&),
            OrB => copy_binop!(OrB, ms, B, B, ||),
            NotB => copy_unop!(NotB, ms, B, B, !),
            XorB => copy_binop!(XorB, ms, B, B, ^),
            BToS => {
                check_unop_input!(UToS, &ms);
                take_args!(a, ret; ms);
                if let B(b) = &a {
                    Tell {
                        channel: ret,
                        messages: vec![ S(format!("{}", b)) ]
                    }
                } else {
                    panic!("BToS unexpected input: {:?}", a);
                }
            },
            EqI => copy_binop!(EqI, ms, I, B, ==),
            LeI => copy_binop!(LeI, ms, I, B, <),
            LeqI => copy_binop!(LeqI, ms, I, B, <=),
            GeI => copy_binop!(GeI, ms, I, B, >),
            GeqI => copy_binop!(GeqI, ms, I, B, >=),
            IToS => {
                check_arity!(IToS, 2, &ms);
                check_channel!(IToS, &ms[1]);
                let ret = ms.pop().unwrap();
                let a = ms.pop().unwrap();
                if let ValueCons::I(x) = a {
                    let result = ValueCons::S(format!("{}", x));
                    ProcCons::Tell {
                        channel: ret,
                        messages: vec![result]
                    }
                } else {
                    panic!(
                        "IToS expected one integer, \
                         but got: [[[1]]]`{:?}`",
                        a
                    );
                }
            },
            AddI => copy_binop!(AddI, ms, I, I, +),
            NegI => copy_unop!(NegI, ms, I,  I, -),
            SubI => copy_binop!(SubI, ms, I, I, -),
            MulI => copy_binop!(MulI, ms, I, I, *),
            DivI => copy_binop!(DivI, ms, I, I, /),
            RemI => copy_binop!(RemI, ms, I, I, %),
            EqS => {
                check_binop_input!(EqS, &ms);
                take_args!(a, b, ret; ms);
                if let (S(x), S(y)) = (&a, &b) {
                    Tell { channel: ret, messages: vec![B(x == y)] }
                } else {
                    panic!(
                        "EqS expected two strings, but got \
                         [[[1]]]`{:?}` [[[2]]]`{:?}`.",
                         a,
                         b
                    )
                }
            },
            ConcatS => {
                check_arity!(ConcatS, 3, &ms);
                check_channel!(ConcatS, &ms[2]);
                let ret = ms.pop().unwrap();
                let b = ms.pop().unwrap(); // pop in reverse order!
                let a = ms.pop().unwrap();
                if let (ValueCons::S(x), ValueCons::S(y)) = (&a, &b) {
                    let result = ValueCons::S(format!("{}{}", x, y));
                    ProcCons::Tell {
                        channel: ret,
                        messages: vec![result]
                    }
                } else {
                    panic!(
                        "ConcatS expected two strings as arguments, \
                         but got: [[[1]]]`{:?}` [[[2]]]`{:?}`.",
                        a,
                        b
                    );
                }
            },
            StdOut => {
                check_arity!(StdOut, 1, &ms);
                let a = ms.pop().unwrap();
                if let S(s) = &a {
                    // not broken, desired side effect! ||
                    print!("{}", s); // NOGREP          ||
                    // not broken, desired side effect! ||
                    Parallel(vec![])
                } else {
                    panic!(
                        "StdOut expected single string, but got {:?}",
                        a
                    )
                }
            },
            StdErr => {
                check_arity!(StdOut, 1, &ms);
                take_args!(a; ms);
                if let S(s) = &a {
                    eprintln!("{}", s);
                    Parallel(vec![])
                } else {
                    panic!(
                        "StdErr expected single string, but got {:?}",
                        a
                    )
                }
            },
            StdIn => {
                eprintln!("WARNING, sent messages to stdin: {:?}.", ms);
                eprintln!(
                    "All messages sent to stdin will be simply dropped, \
                     that's probably not the intended behavior."
                );
                Parallel(vec![])
            },
            // TORESEARCH: not quite clear what to do with STDIN? What to do
            // with all the other "input-like" thingies, like opened files or
            // random number sources or sockets?
        }
    }
}

macro_rules! atom {
    ($s: expr) => {
        Atom($s.to_string())
    };
}

impl From<&BuiltInChannel> for PrettyPrintable {
    fn from(c: &BuiltInChannel) -> PrettyPrintable {
        match c {
            EqU => atom!("<eq_u>"),
            UToS => atom!("<u_to_s>"),
            BToS => atom!("<b_to_s>"),
            If => atom!("<if>"),
            EqB => atom!("<eq_b>"),
            AndB => atom!("<and_b>"),
            OrB => atom!("<or_b>"),
            NotB => atom!("<not_b>"),
            XorB => atom!("<xor_b>"),
            EqI => atom!("<eq_i>"),
            LeI => atom!("<le_i>"),
            LeqI => atom!("<leq_i>"),
            GeI => atom!("<ge_i>"),
            GeqI => atom!("<geq_i>"),
            AddI => atom!("<add_i>"),
            SubI => atom!("<sub_i>"),
            NegI => atom!("<neg_i>"),
            MulI => atom!("<mul_i>"),
            DivI => atom!("<div_i>"),
            RemI => atom!("<rem_i>"),
            IToS => atom!("<i_to_s>"),
            EqS => atom!("<eq_s>"),
            ConcatS => atom!("<concat_s>"),
            StdIn => atom!("<stdin>"),
            StdOut => atom!("<stdout>"),
            StdErr => atom!("<stderr>"),
        }
    }
}
