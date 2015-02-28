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
#![feature(os)]
#![feature(plugin)]
#![feature(rustc_private)]

#![plugin(regex_macros)]

extern crate regex;
extern crate serialize;

extern crate simproc;

mod args;
mod asm;

use std::fs::File;

use simproc::inst::Encode;

use asm::{Assembler, Assembled, AssemblyError};
use asm::sp80;

fn main() {
	let args = args::parse_args().unwrap();
	let ifile = File::open(&args.input[..]).unwrap();

	let asmblr = sp80::Asm80::new();
	let asm = match asmblr.assemble(ifile) {
		Ok(asm) => asm,
		Err(AssemblyError::BadProgram(errors)) => {
			println!("Assembled with {} errors:", errors.len());
			for e in errors.iter() { println!("\t{}", e); }
			return;
		},
		Err(AssemblyError::Io(e)) =>  {
			println!("IO error while reading file {}: {}", args.input, e);
			return;
		},
	};

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
}
