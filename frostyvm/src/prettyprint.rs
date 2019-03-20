/// A little pretty-printing module.
///
/// It doesn't matter whether there are already other crates that do roughly
/// the same. The only thing that matters is the fact that it's implemented
/// 1:1 in exactly the same way as the scala version used in the compiler.

/// An extremely simplified abstract-syntax-tree like data structure that
/// holds textual data together with hints that determine how it is
/// laid out when pretty printed.
pub enum PrettyPrintable {
    Atom(String),
    Delimited {
        start: String,
        content: Vec<PrettyPrintable>,
        end: String,
    },
    Juxtaposition(Vec<PrettyPrintable>),
    Chain(Vec<PrettyPrintable>),
}

use crate::prettyprint::PrettyPrintable::*;
static TAB_SIZE: usize = 2;

fn push_indent(num_spaces: usize, builder: &mut String) -> () {
    for _i in 0..num_spaces {
        builder.push(' ');
    }
}

impl PrettyPrintable {
    /// How much space it would take on a single line.
    fn single_line_width(&self) -> usize {
        match self {
            Atom(c) => c.len(),
            Delimited {
                start: s,
                content: c,
                end: e,
            } => {
                s.len()
                    + e.len()
                    + c.iter().map(|x| x.single_line_width()).sum::<usize>()
            }
            Juxtaposition(xs) => xs.iter().map(|x| x.single_line_width()).sum(),
            Chain(xs) => xs.iter().map(|x| x.single_line_width()).sum(),
        }
    }

    fn header_width(&self) -> usize {
        match self {
            Atom(c) => c.len(),
            Delimited { start: s, .. } => s.len(),
            Juxtaposition(xs) => xs[0].header_width(),
            Chain(xs) => xs[0].header_width(),
        }
    }

    fn push_as_single_line(&self, builder: &mut String) -> () {
        match self {
            Atom(c) => builder.push_str(&c),
            Delimited {
                start: s,
                content: c,
                end: e,
            } => {
                builder.push_str(&s);
                for x in c.iter() {
                    x.push_as_single_line(builder);
                }
                builder.push_str(&e);
            }
            Juxtaposition(xs) => {
                for x in xs.iter() {
                    x.push_as_single_line(builder);
                }
            }
            Chain(xs) => {
                for x in xs.iter() {
                    x.push_as_single_line(builder);
                }
            }
        }
    }

    fn pp_helper_rec(
        &self,
        col: usize,
        indent: usize,
        width: usize,
        builder: &mut String,
    ) -> usize {
        match self {
            Atom(content) => {
                builder.push_str(&content);
                col + content.len()
            }
            Delimited {
                start: s,
                content: c,
                end: e,
            } => {
                let remaining = width - col;
                let sing_line_w = self.single_line_width();
                if remaining >= sing_line_w {
                    self.push_as_single_line(builder);
                    col + sing_line_w
                } else {
                    builder.push_str(s);
                    builder.push('\n');
                    for x in c.iter() {
                        push_indent(indent + TAB_SIZE, builder);
                        x.pp_helper_rec(
                            indent + TAB_SIZE,
                            indent + TAB_SIZE,
                            width,
                            builder,
                        );
                        builder.push('\n');
                    }
                    push_indent(indent, builder);
                    builder.push_str(e);
                    let last_line_width = indent + e.len();
                    last_line_width
                }
            }
            Juxtaposition(xs) => {
                let mut curr_col = col;
                for x in xs.iter() {
                    if curr_col + x.header_width() > width {
                        builder.push('\n');
                        push_indent(indent, builder);
                        let c = x.pp_helper_rec(indent, indent, width, builder);
                        curr_col = c;
                    } else {
                        curr_col =
                            x.pp_helper_rec(curr_col, indent, width, builder);
                    }
                }
                curr_col
            }
            Chain(xs) => {
                let remaining = width - col;
                let sing_line_w = self.single_line_width();
                if remaining >= sing_line_w {
                    self.push_as_single_line(builder);
                    col + sing_line_w
                } else {
                    xs[0].pp_helper_rec(col, indent, width, builder);
                    let mut last_col = 0;
                    for x in xs.iter().skip(1) {
                        builder.push('\n');
                        push_indent(indent, builder);
                        last_col =
                            x.pp_helper_rec(indent, indent, width, builder);
                    }
                    last_col
                }
            }
        }
    }

    pub fn pretty_print(&self, width: usize) -> String {
        let mut builder = String::new();
        self.pp_helper_rec(0, 0, width, &mut builder);
        builder
    }
}

#[test]
fn test_pretty_print() {
    use trim_margin::MarginTrimmable;

    let expected_code: String = r###"
        |<script>
        |  function foo(
        |    too, 
        |    many, 
        |    arguments
        |  ){
        |    bar
        |    .baz
        |    .many
        |    .many
        |    .methods(x, y);
        |  }
        |  function baz(x){}
        |</script>
    "###
    .trim_margin()
    .unwrap();

    let cst = Delimited {
        start: "<script>".to_string(),
        content: vec![
            Juxtaposition(vec![
                Atom("function ".to_string()),
                Atom("foo".to_string()),
                Delimited {
                    start: "(".to_string(),
                    content: vec![
                        Atom("too, ".to_string()),
                        Atom("many, ".to_string()),
                        Atom("arguments".to_string()),
                    ],
                    end: ")".to_string(),
                },
                Delimited {
                    start: "{".to_string(),
                    content: vec![Chain(vec![
                        Atom("bar".to_string()),
                        Atom(".baz".to_string()),
                        Atom(".many".to_string()),
                        Atom(".many".to_string()),
                        Juxtaposition(vec![
                            Atom(".methods".to_string()),
                            Delimited {
                                start: "(".to_string(),
                                content: vec![
                                    Atom("x, ".to_string()),
                                    Atom("y".to_string()),
                                ],
                                end: ");".to_string(),
                            },
                        ]),
                    ])],
                    end: "}".to_string(),
                },
            ]),
            Juxtaposition(vec![
                Atom("function ".to_string()),
                Atom("baz".to_string()),
                Delimited {
                    start: "(".to_string(),
                    content: vec![Atom("x".to_string())],
                    end: ")".to_string(),
                },
                Delimited {
                    start: "{".to_string(),
                    content: Vec::new(),
                    end: "}".to_string(),
                },
            ]),
        ],
        end: "</script>".to_string(),
    };
    assert_eq!(expected_code, cst.pretty_print(20));
}
