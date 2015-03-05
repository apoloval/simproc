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

use std::fs::File;
use std::io::stdout;

use simproc::inst::{Inst, Encode};

use asm::{Assembly, AssemblyError};
use asm::assembler::Assembler;
use asm::sp80;

fn main() {
    let args = args::parse_args();
    let asm = match args.action() {
        args::Action::Version => {
            println!("SimProc Assembler version 0.1.0");
            println!("Copyright (C) 2015 Alvaro Polo");
            println!("");
            return;
        },
        _ => {
            match assemble::<sp80::Assembler>(&args.input_file()) {
                Some(asm) => asm,
                None => { return; },
            }
        },
    };
    match args.action() {
        args::Action::Text => { write_as_text(&asm); },
        args::Action::Bin => { write_as_bin(&asm, &args.output_file().unwrap()[..]); },
        _ => {},
    }
}

fn assemble<A: Assembler>(input: &String) -> Option<Assembly<A::RuntimeInst>> {
    let asmblr = A::new();
    match asmblr.assemble(&input[..]) {
        Ok(asm) => Some(asm),
        Err(AssemblyError::BadProgram(ref errors)) => {
            println!("Assembled with {} errors:", errors.len());
            for e in errors.iter() { println!("\t{}", e); }
            None
        },
        Err(AssemblyError::Io(ref e)) =>  { 
            println!("{}", e);
            None
        },
    }
}

fn write_as_text<RI: Inst + Encode>(asm: &Assembly<RI>) {
    match asm.write_as_text(&mut stdout()) {
        Ok(_) => {},
        Err(e) => { println!("Unexpected error while writing output: {}", e); },
    };
}

fn write_as_bin<RI: Inst + Encode>(asm: &Assembly<RI>, output_file: &str) {
    let mut output = match File::create(output_file) {
        Ok(f) => f,
        Err(e) => { 
            println!("Unexpected error while opening output: {}", e); 
            return;
        },
    };
    match asm.write_as_bin(&mut output) {
        Ok(_) => {},
        Err(e) => { println!("Unexpected error while writing output: {}", e); },
    };
}
