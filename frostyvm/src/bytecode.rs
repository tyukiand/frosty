use crate::built_in::BuiltInChannel::*;
use crate::process::ProcCons::*;
use crate::process::ValueCons::*;
use crate::process::{Path, Proc, ProcCons, Value, ValueCons};

use std::io::{Read, Result}; // TODO: Result is not a good return type here

/// Process that saves only the number of bound variables, used only during
/// deserialization.
pub type BcProc = ProcCons<u32>;

/// Value that contains processes that save only number of variables in the
/// binders.
pub type BcValue = ValueCons<u32>;

// this macro is used in the included `bytecode_format.rs` file.
macro_rules! term_constructor {
    ($name: ident, $number: expr) => {
        const $name: u8 = $number;
    };
}

macro_rules! channel_name {
    ($name: ident, $number: expr, $path: expr, $type: tt) => {
        const $name: u8 = $number;
    }
}

include!("../../bytecode_format.rs");

fn read_byte<R: Read>(buf: &mut [u8], reader: &mut R) -> Result<u8> {
    reader.read_exact(buf)?;
    return Ok(buf[0]);
}

// TODO: reading ints: is it necessary to care about endianness and to
// read it byte by byte?
// Where exactly is it specified how those numbers would be read by the usual
// api methods?

/// Reads a single `u32` in big-endian format.
fn read_u32<R: Read>(buf: &mut [u8], reader: &mut R) -> Result<u32> {
    reader.read_exact(buf)?;
    let mut res: u32 = buf[0] as u32;
    res = res << 8 | buf[1] as u32;
    res = res << 8 | buf[2] as u32;
    res = res << 8 | buf[3] as u32;
    return Ok(res);
}

fn read_i32<R: Read>(buf: &mut [u8], reader: &mut R) -> Result<i32> {
    reader.read_exact(buf)?;
    let mut res: i32 = buf[0] as i32;
    res = res << 8 | buf[1] as i32;
    res = res << 8 | buf[2] as i32;
    res = res << 8 | buf[3] as i32;
    return Ok(res);
}

fn read_proc_rec<R: Read>(
    label_buf: &mut [u8],
    buf_32: &mut [u8],
    reader: &mut R,
) -> Result<BcProc> {
    match read_byte(label_buf, reader)? {
        PARALLEL => {
            let n = read_u32(buf_32, reader)? as usize;
            let mut procs = Vec::with_capacity(n);
            for _i in 0..n {
                procs.push(read_proc_rec(label_buf, buf_32, reader)?);
            }
            Ok(Parallel(procs))
        },
        TELL => {
            let channel = read_value_rec(label_buf, buf_32, reader)?;
            let n = read_u32(buf_32, reader)? as usize;
            let mut messages = Vec::with_capacity(n);
            for _i in 0..n {
                messages.push(read_value_rec(label_buf, buf_32, reader)?);
            }
            Ok(Tell { channel, messages })
        },
        RECEIVE => {
            let channel = read_value_rec(label_buf, buf_32, reader)?;
            let num_vars = read_u32(buf_32, reader)?;
            let body = Box::new(read_proc_rec(label_buf, buf_32, reader)?);
            Ok(Receive {
                channel,
                binders: num_vars,
                body,
            })
        },
        NEW => {
            let num_vars = read_u32(buf_32, reader)?;
            let body = Box::new(read_proc_rec(label_buf, buf_32, reader)?);
            Ok(New {
                binders: num_vars,
                body,
            })
        },
        UNFREEZE => {
            let value = read_value_rec(label_buf, buf_32, reader)?;
            Ok(Unfreeze(value))
        },
        sth_else => {
            let mut context_bytes = vec![0u8; 32];
            let bytes_read = reader.read(&mut context_bytes).unwrap();
            panic!(
                "Unknown marker byte during process deserialization: {} \n\
                 Here are {} bytes (hex) of following context: \n{:?}\n",
                sth_else,
                bytes_read,
                context_bytes
                  .into_iter()
                  .map(|b| format!("{:02x}", b))
                  .collect::<Vec<String>>()
                  .join(" ")
            )
        }
    }
}

fn read_value_rec<R: Read>(
    label_buf: &mut [u8],
    buf_32: &mut [u8],
    reader: &mut R,
) -> Result<BcValue> {
    match read_byte(label_buf, reader)? {
        DE_BRUIJN_INDEX => Ok(DeBruijnIndex(read_u32(buf_32, reader)?)),
        UNIT => Ok(U),
        BOOLEAN => Ok(B(read_byte(label_buf, reader)? == 1u8)),
        STRING => {
            // String as utf8
            let str_len = read_u32(buf_32, reader)? as usize;
            let mut str_bytes = vec![0u8; str_len];
            reader.read_exact(&mut str_bytes)?;
            let s = String::from_utf8(str_bytes).expect("Valid UTF8");
            Ok(S(s))
        }
        INTEGER => Ok(I(read_i32(buf_32, reader)?)),
        FREEZE => {
            let p = read_proc_rec(label_buf, buf_32, reader)?;
            Ok(Freeze(Box::new(p)))
        }
        PATH_NAME => {
            let n = read_u32(buf_32, reader)? as usize;
            let mut v = Vec::with_capacity(n);
            for _ in 0..n {
                let str_len = read_u32(buf_32, reader)? as usize;
                let mut str_bytes = vec![0u8; str_len];
                reader.read_exact(&mut str_bytes)?;
                let s = String::from_utf8(str_bytes).expect("Valid UTF8");
                v.push(s);
            }
            Ok(PathName(Path(v)))
        }
        EQ_U => Ok(BuiltInChannelName(EqU)),
        U_TO_S => Ok(BuiltInChannelName(UToS)),
        IF => Ok(BuiltInChannelName(If)),
        EQ_B => Ok(BuiltInChannelName(EqB)),
        AND_B => Ok(BuiltInChannelName(AndB)),
        OR_B => Ok(BuiltInChannelName(OrB)),
        NOT_B => Ok(BuiltInChannelName(NotB)),
        XOR_B => Ok(BuiltInChannelName(XorB)),
        B_TO_S => Ok(BuiltInChannelName(BToS)),
        EQ_I => Ok(BuiltInChannelName(EqI)),
        LE_I => Ok(BuiltInChannelName(LeI)),
        LEQ_I => Ok(BuiltInChannelName(LeqI)),
        GR_I => Ok(BuiltInChannelName(GrI)),
        GEQ_I => Ok(BuiltInChannelName(GeqI)),
        ADD_I => Ok(BuiltInChannelName(AddI)),
        NEG_I => Ok(BuiltInChannelName(NegI)),
        SUB_I => Ok(BuiltInChannelName(SubI)),
        MUL_I => Ok(BuiltInChannelName(MulI)),
        DIV_I => Ok(BuiltInChannelName(DivI)),
        REM_I => Ok(BuiltInChannelName(RemI)),
        I_TO_S => Ok(BuiltInChannelName(IToS)),
        EQ_S => Ok(BuiltInChannelName(EqS)),
        CONCAT_S => Ok(BuiltInChannelName(ConcatS)),
        STD_OUT => Ok(BuiltInChannelName(StdOut)),
        STD_ERR => Ok(BuiltInChannelName(StdErr)),
        STD_IN => Ok(BuiltInChannelName(StdIn)),
        unknown_byte => panic!(
            "Unknown marker byte encountered during deserialization \
             of a value: {} (hex: {:x})",
            unknown_byte, unknown_byte
        ),
    }
}

// TORESEARCH: dismantling the deserialized structure, and rebuilding the
// new tree with variable occurrence counts seems unnecessary. It should be
// possible to fuse these two steps, and generate the tree with the correct
// variable occurrence counts right away.

fn count_proc_rec(p: BcProc, counts_stack: &mut Vec<u32>) -> Proc {
    match p {
        Parallel(ps) => {
            let mut acc = Vec::with_capacity(ps.len());
            for q in ps.into_iter() {
                acc.push(count_proc_rec(q, counts_stack));
            }
            Parallel(acc)
        }
        Unfreeze(v) => Unfreeze(count_value_rec(v, counts_stack)),
        Receive {
            channel: ch,
            binders: num_binders,
            body: b,
        } => {
            let modified_channel = count_value_rec(ch, counts_stack);
            for _ in 0..num_binders {
                counts_stack.push(0);
            }
            let modified_body = count_proc_rec(*b, counts_stack);
            let mut occ_counts = vec![0; num_binders as usize];
            for i in 0..num_binders {
                occ_counts[(num_binders - 1 - i) as usize] =
                    counts_stack.pop().unwrap();
            }
            Receive {
                channel: modified_channel,
                binders: occ_counts,
                body: Box::new(modified_body),
            }
        }
        New {
            binders: num_binders,
            body: b,
        } => {
            for _ in 0..num_binders {
                counts_stack.push(0);
            }
            let modified_body = count_proc_rec(*b, counts_stack);

            let mut occ_counts = vec![0; num_binders as usize];
            for i in 0..num_binders {
                occ_counts[(num_binders - 1 - i) as usize] =
                    counts_stack.pop().unwrap();
            }
            New {
                binders: occ_counts,
                body: Box::new(modified_body),
            }
        }
        Tell {
            channel: ch,
            messages: ms,
        } => {
            let modified_channel = count_value_rec(ch, counts_stack);
            let mut modified_messages = Vec::with_capacity(ms.len());
            for m in ms.into_iter() {
                modified_messages.push(count_value_rec(m, counts_stack));
            }
            Tell {
                channel: modified_channel,
                messages: modified_messages,
            }
        }
        BuiltInService(s) => panic!(
            "Unexpected: built in service {:?} during occurrence count phase.",
            s
        ),
        MemReplacement => {
            panic!("Unexpected MemReplacement during occurrence count phase.")
        }
    }
}

fn count_value_rec(v: BcValue, counts_stack: &mut Vec<u32>) -> Value {
    let depth = counts_stack.len();
    match v {
        DeBruijnIndex(idx) => {
            counts_stack[depth - idx as usize] += 1;
            DeBruijnIndex(idx)
        }
        Freeze(p) => Freeze(Box::new(count_proc_rec(*p, counts_stack))),
        PathName(p) => PathName(p),
        BuiltInChannelName(c) => BuiltInChannelName(c),
        U => U,
        B(b) => B(b),
        I(i) => I(i),
        S(s) => S(s),
        UnforgeableName(u) => UnforgeableName(u),

        PathSurrogate(s) => panic!(
            "Unexpected PathSurrogate {:?} during occurrence counting phase.",
            s
        ),
        Depleted => {
            panic!("Unexpected `Depleted` during occurrence counting phase.")
        }
    }
}

fn count_occurrences(p: BcProc) -> Proc {
    let mut stack = Vec::new();
    count_proc_rec(p, &mut stack)
}

pub fn read<R: Read>(reader: &mut R) -> Result<Proc> {
    let mut label_buf: [u8; 1] = [0u8];
    let mut buf_32: [u8; 4] = [0u8; 4];
    let bc_proc = read_proc_rec(&mut label_buf, &mut buf_32, reader)?;
    Ok(count_occurrences(bc_proc))
}

#[cfg(test)]
mod test {

    use super::*;

    /* TODO: reactivate as soon as we have "the real read"
    #[test]
    fn test_read_par_two_zero_procs() {
        let data: [u8; 15] = [
            0u8, 0u8, 0u8, 0u8, 2u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8,
            0u8, 0u8,
        ];
        let mut byte_source: &[u8] = &data;
        let p = read(&mut byte_source).unwrap();
        assert_eq!(
            p,
            proc![{
                {}
                {}
            }]
        );
    }
    */

    #[test]
    fn test_imported_consts() {
        assert_eq!(PATH_NAME, 7);
        assert_eq!(DE_BRUIJN_INDEX, 8);
    }

}
