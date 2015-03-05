//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(core)]
#![feature(fs)]
#![feature(io)]
#![feature(plugin)]
#![feature(rustc_private)]

#![plugin(regex_macros)]

extern crate docopt;
extern crate regex;
extern crate "rustc-serialize" as rustc_serialize;
extern crate serialize;

extern crate simproc;

mod args;
mod asm;

use std::io::stdout;

use asm::AssemblyError;
use asm::assembler::Assembler;
use asm::sp80;

fn main() {
    let args = args::parse_args();
    if args.flag_version {
        println!("SimProc Assembler version 0.1.0");
        println!("Copyright (C) 2015 Alvaro Polo");
        println!("");
    } else {
        assemble(&args.arg_input)
    }
}

fn assemble(input: &String) {
    let asmblr = sp80::Assembler::new();
    let asm = match asmblr.assemble(&input[..]) {
        Ok(asm) => asm,
        Err(AssemblyError::BadProgram(ref errors)) => {
            println!("Assembled with {} errors:", errors.len());
            for e in errors.iter() { println!("\t{}", e); }
            return;
        },
        Err(AssemblyError::Io(ref e)) =>  { 
            println!("{}", e);
            return;
        },
    };
    match asm.write_as_text(&mut stdout()) {
        Ok(_) => {},
        Err(e) => { println!("Unexpected error while writing output: {}", e); },
    }
}
