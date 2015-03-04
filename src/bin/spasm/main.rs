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

use simproc::inst::Encode;

use asm::{Assembled, AssemblyError};
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
        Err(AssemblyError::BadProgram(errors)) => {
            println!("Assembled with {} errors:", errors.len());
            for e in errors.iter() { println!("\t{}", e); }
            return;
        },
        Err(AssemblyError::Io(e)) =>  {
            println!("IO error while reading file {}: {}", &input[..], e);
            return;
        },
    };

    assemble_txt(&asm);
}

fn assemble_txt(asm: &sp80::RuntimeAssembly) {
    for line in asm.assembled().iter() {
        match line {
            &Assembled::Inst(ref line, place, ref inst) => {
                let mut buff: Vec<u8> = Vec::new();
                let nbytes = inst.encode(&mut buff).unwrap();
                print!("0x{:04x} : ", place as u16);
                for b in buff.iter() {            
                    print!("{:02x} ", b);
                }
                for _ in 0..(10 - 3*nbytes) { print!(" "); }
                println!("{}", line);
            },
            &Assembled::Ignored(ref line) => println!("                   {}", line),
        }
    }

    let symbols = asm.symbols();
    println!("\nSymbol table:");
    if symbols.is_empty() { println!("  Empty"); }
    else {
        for (sym, val) in symbols.iter() {
            println!("  {} : 0x
                {:04x}", sym, val);
        }
    }
}
