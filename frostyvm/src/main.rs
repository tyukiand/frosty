#[cfg(test)]
#[macro_use]
mod edsl;

#[cfg(test)]
mod environment;

mod built_in;
mod bytecode;
mod comm_queue;
mod config;
mod linker;
mod prettyprint;
mod process;

use comm_queue::CommQueue;
use config::parse_command_line_args;

use linker::LinkerTable;
use process::Proc;
use std::error::Error;
use std::fs::File;

/// Loads the bytecode from the specified file, parses it into
/// a vector of terms. The terms will typically have path-names all
/// over the place.
fn load(input_file_name: &str) -> Result<Proc, Box<dyn Error>> {
    // TODO: there is something rotten in this error-handling... Why am I
    // only catching io-errors, but panic on invalid serialization format?
    let mut file = File::open(input_file_name)?;
    match bytecode::read(&mut file) {
        Ok(res) => Ok(res),
        Err(e) => Err(Box::new(e)),
    }
}

/// Reduces all terms to completion, executing side-effects along the way.
fn run_program(mut procs: Vec<Proc>, dump_result: bool) -> () {
    // link everything
    let mut linker = LinkerTable::new();
    linker.link(&mut procs);

    // setup `comm_queue` and insert all communicating processes into it.
    let mut comm_queue = CommQueue::new();

    for p in procs.into_iter() {
        let (rcv_snds, mut news) = p.explode();

        for r in rcv_snds.into_iter() {
            comm_queue.push_back(r);
        }

        // every `New` reachable by a chain of `New`s is pushed onto the
        // `news` stack, and this stack is cleared right here, before
        // the actual execution begins.
        while let Some(mut n) = news.pop() {
            n.supply_unforgeables();
            let (more_rcv_snds, subnews) = n.explode();
            for c in more_rcv_snds.into_iter() {
                comm_queue.push_back(c);
            }
            news.extend(subnews);
        }
        // TORESEARCH: Could the channel queue be made a bit
        // more general and the various beta-reduction-like operations a bit
        // more unified? `New` in particular looks just like sending a message
        // to a special built-in channel, except that it's vararg.
    }

    // main loop: iterate until completion
    while let Some((x, y)) = comm_queue.pop_front() {
        let mut acc = Vec::with_capacity(2);
        if !x.is_built_in() { acc.push(x); }
        if !y.is_built_in() { acc.push(y); }
        for p in Proc::reduce_until_local_convergence(acc).into_iter() {
            comm_queue.push_back(p);
        }
    }

    // print the content of comm_queue when it's over
    if dump_result {
        println!("Final state of the communication queue:");
        for p in comm_queue.harvest() {
            println!("{}", p);
        }
    }
}

fn main() {
    let conf = parse_command_line_args();
    match load(&conf.input_file_path) {
        Ok(p) => {
            if conf.dump_input {
                println!("Parsed input:\n{}", p);
            }
            run_program(vec![p], conf.dump_result);
        }
        Err(e) => println!("Corrupt input file, failed to load: {}", e),
    }
}
