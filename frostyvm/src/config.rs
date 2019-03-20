use clap::{App, Arg};

pub struct Config {
    pub dump_result: bool,
    pub dump_input: bool,
    pub input_file_path: String,
}

const AFTER_HELP: &'static str = r##"
Blah blah, this is some help for the Frosty-VM.
"##;

pub fn parse_command_line_args() -> Config {
    let arg_matches = App::new("frostyvm")
        .version("0.1")
        .about("Executes compiled binary Frosty processes.")
        .arg(
            Arg::with_name("dump_result")
                .long("dump-result")
                .help(
                    "\
                     Writes out all processes remaining \
                     in the queue after termination",
                )
                .takes_value(false)
                .multiple(false)
                .required(false),
        )
        .arg(
            Arg::with_name("dump_input")
                .long("dump-input")
                .help("Dumps deserialized input")
                .takes_value(false)
                .multiple(false)
                .required(false),
        )
        .arg(Arg::with_name("input_file_path").multiple(false))
        .after_help(AFTER_HELP)
        .get_matches();

    Config {
        dump_input: arg_matches.occurrences_of("dump_input") > 0,
        dump_result: arg_matches.occurrences_of("dump_result") > 0,
        input_file_path: arg_matches
            .value_of("input_file_path")
            .unwrap()
            .to_owned(),
    }
}
