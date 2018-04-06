//
// SimProc Machine
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate docopt;
#[macro_use] extern crate serde_derive;
extern crate simproc;

mod args;
mod machine;

use std::convert::From;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::{Read, stdout};

use machine::Machine;

enum Error {
    Io(io::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Error::Io(ref e) => write!(fmt, "{}", e),
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self { Error::Io(e) }
}

#[allow(dead_code)]
fn main() {
    match do_main() {
        Err(e) => {
            println!("Error: {}", e);
        }
        _ => {},
    }
}

fn do_main() -> Result<(), Error> {
    let args = args::parse_args();
    if args.action() == args::Action::Version {
        println!("SimProc Machine version 0.1.0");
        println!("Copyright (C) 2015 Alvaro Polo");
        println!("");
        return Ok(());
    }

    let romfile = &args.romfile();
    let rom = try!(File::open(romfile));
    let mut machine = Machine::with_rom(rom.bytes().map(|b| b.unwrap()));
    machine.attach_console(stdout());
    machine.run_until_halt();
    Ok(())
}
