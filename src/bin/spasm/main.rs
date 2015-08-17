//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate byteorder;
extern crate docopt;
extern crate regex;
extern crate rustc_serialize;

extern crate simproc;

#[cfg(test)]
extern crate quickcheck;

mod args;
mod asm;

use std::char;
use std::fs::File;
use std::io::{Read, stdout};

use asm::*;

#[allow(dead_code)]
fn main() {
    let args = args::parse_args();
    if args.action() == args::Action::Version {
        println!("SimProc Assembler version 0.1.0");
        println!("Copyright (C) 2015 Alvaro Polo");
        println!("");
        return;
    }

    let filename = &args.input_file();
    let file = match File::open(filename) {
        Ok(f) => f,
        Err(e) => {
            println!("Error: couldn't read '{}': {}", filename, e);
            return;
        },
    };
    let chars = file.bytes().map(|b| char::from_u32(b.ok().unwrap() as u32).unwrap());
    let asm = Assembly::assemble(chars);
    if asm.has_errors() { write_errors(asm) }
    else {
        match args.action() {
            args::Action::Text => { write_as_text(asm); },
            args::Action::Bin => { write_as_bin(asm, &args.output_file().unwrap()); },
            _ => {},
        }
    }
}

#[allow(dead_code)]
fn write_errors(asm: Assembly) {
    for e in asm.errors() { println!("{}", e); }
}

#[allow(dead_code)]
fn write_as_text(asm: Assembly) {
    if let Err(e) = asm.dump_text(&mut stdout()) {
        println!("Error: cannot write to stdout: {}", e);
    }
}

#[allow(dead_code)]
fn write_as_bin(asm: Assembly, output_file: &str) {
    let mut output = match File::create(output_file) {
        Ok(f) => f,
        Err(e) => {
            println!("Error: couldn't open '{}' for writing: {}", output_file, e);
            return;
        },
    };
    if let Err(e) = asm.write(&mut output) {
        println!("Error: couldn't write '{}': {}", output_file, e);
    }
}
