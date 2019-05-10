// Byte constants for the serialization format.
//
// Sketch: 1 symbol corresponds to 4 code units
//
//    ZC  V   ?   B   I       S   Y   L       H       P       ?
//    ####****????####********####****________^^^^^^^^XXXXXXXX????????
//    ----------------------------------------------------------------
//    ||  |   |   |   |       |   |   |       |       |       |
//    01  16  32  48  64      96  112 128     160     192     224
// 
// Z(1):   zero is not used
// C(15):  core calculus (par, tell, receive, new, etc.)
// V(16):  basic primitive values (boolean, int, string etc.)
// ?(16):  not assigned yet
// B(16):  Unit and Boolean built-in channels
// I(32):  integer built-in channels
// S(16):  utf-8 string basic built-in channels
// Y(16):  basic system channels (stdin, stdout, stderr etc.)
// L(32):  low surrogates
// H(32):  high surrogates
// P(32):  private usage area (will *never* be assigned)
// ?(32):  unassigned
//
// The surrogates are there just in case someone needs more "opcodes" for the
// basic operations. In this case, one could easily get
// roughly 896 new code points encoded by two-byte combinations, at the
// expense of sacrificing 64 single-byte code units.
//
// The private use area has the same function as in Unicode: if someone
// wants to build a custom non-standard version of this virtual machine,
// they can add 32 new custom opcodes (or 80 opcodes via 8+8 sub-surrogates),
// this should be enough for any kind of debugger that anyone could possibly
// come up with.

// The following table is imported into Rust and also translated
// into Scala. The "table-start" marker is required, all offsets
// must agree with line number differences (every line starting
// with `// \d+` will be checked for that property).

// 0 <TABLE-START> (zero unused)
term_constructor!(PARALLEL, 1);
term_constructor!(RECEIVE, 2);
term_constructor!(TELL, 3);
term_constructor!(NEW, 4);
term_constructor!(UNFREEZE, 5);
term_constructor!(FREEZE, 6);
term_constructor!(PATH_NAME, 7);
term_constructor!(DE_BRUIJN_INDEX, 8);
// 9
//
//
//
//
//
// 15
term_constructor!(UNIT, 16);
term_constructor!(BOOLEAN, 17);
term_constructor!(INTEGER, 18);
term_constructor!(STRING, 19);
// 20
//
//
//
//
//
//
//
//
//
//
// 31
// 32 (unassigned)
//
//
//
//
//
//
//
//
//
//
//
//
//
//
// 47 (/unassigned)
channel_name!(EQ_U, 48, "/lang/eq_unit", {Fun[Unit, Unit, Boolean]});
channel_name!(U_TO_S, 49, "/lang/unit_to_string", {Fun[Unit, String]});
channel_name!(IF, 50, "/lang/if", {Chan[Boolean, Proc, Proc]});
channel_name!(EQ_B, 51, "/lang/eq_boolean", {Fun[Boolean, Boolean, Boolean]});
channel_name!(AND_B, 52, "/lang/and_boolean", {Fun[Boolean, Boolean, Boolean]});
channel_name!(OR_B, 53, "/lang/or_boolean", {Fun[Boolean, Boolean, Boolean]});
channel_name!(NOT_B, 54, "/lang/not_boolean", {Fun[Boolean, Boolean, Boolean]});
channel_name!(XOR_B, 55, "/lang/xor_boolean", {Fun[Boolean, Boolean, Boolean]});
channel_name!(B_TO_S, 56, "/lang/boolean_to_string", {Fun[Boolean, String]});
// 57
// 58
// 59
// 60
// 61
// 62
// 63
channel_name!(EQ_I, 64, "/lang/eq_int", {Fun[Int, Int, Boolean]});
channel_name!(LE_I, 65, "/lang/le_int", {Fun[Int, Int, Boolean]});
channel_name!(LEQ_I, 66, "/lang/leq_int", {Fun[Int, Int, Boolean]});
channel_name!(GR_I, 67, "/lang/gr_int", {Fun[Int, Int, Boolean]});
channel_name!(GEQ_I, 68, "/lang/geq_int", {Fun[Int, Int, Boolean]});
channel_name!(I_TO_S, 69, "/lang/int_to_string", {Fun[Int, String]});
// 70
// 71
channel_name!(ADD_I, 72, "/lang/add_int", {Fun[Int, Int, Int]});
channel_name!(NEG_I, 73, "/lang/neg_int", {Fun[Int, Int, Int]});
channel_name!(SUB_I, 74, "/lang/sub_int", {Fun[Int, Int, Int]});
channel_name!(MUL_I, 75, "/lang/mul_int", {Fun[Int, Int, Int]});
channel_name!(DIV_I, 76, "/lang/div_int", {Fun[Int, Int, Int]});
channel_name!(REM_I, 77, "/lang/rem_int", {Fun[Int, Int, Int]});
// 78
//
//
//
// 
//
//
//
// 
//
//
//
// 
//
//
//
// 
// 95
channel_name!(EQ_S, 96, "/lang/eq_string", {Fun[String, String, Boolean]});
channel_name!(CONCAT_S, 97, "/lang/concat_string", {Fun[String, String, String]});
// 98
//
//
//
//
//
//
//
//
//
//
//
//
// 111
channel_name!(STD_OUT, 112, "/sys/out", {Chan[String]});
channel_name!(STD_ERR, 113, "/sys/err", {Chan[String]});
channel_name!(STD_IN, 114, "/sys/in", {Chan[String]});
// 115 (unassigned-basic-system-channels)
//
//
//
//
//
//
//
//
//
//
//
// 127 (/unassigned-basic-system-channels)
// 128  (low-suggorates)
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//    b & 0x7F       LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
//                   LOW
// 159 (/low-surrogates)
// 160 (high-surrogates)
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
// (b & 0x7F) - 32  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
//                  HIGH
// 191 (/high-surrogates)
// 192 (private-use-area)
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1 
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
//                  PUA1
// 223 (/private-use-area)
// 224 (unassigned)
//
//
//
//
//
//
//
//
//
//
//
// 
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
// 255 (/unassigned)
