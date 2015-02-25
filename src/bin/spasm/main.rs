//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(collections)]
#![feature(core)]
#![feature(fs)]
#![feature(io)]
#![feature(os)]
#![feature(plugin)]

#![plugin(regex_macros)]

extern crate regex;

extern crate simproc;

mod args;
mod asm;

use std::fs::File;

use simproc::Inst;

use asm::{Assembler, AssemblyError};
use asm::sp80;

fn main() {
	let args = args::parse_args().unwrap();
	let ifile = File::open(&args.input[..]).unwrap();

	let asmblr = sp80::Asm80::new();
	let asm = match asmblr.assemble(ifile) {
		Ok(asm) => asm,
		Err(AssemblyError::BadProgram(errors)) => {
			println!("Assembled with {} errors:", errors.len());
			for e in errors.iter() { println!("{}", e); }
			return;
		},
		Err(AssemblyError::Io(e)) =>  {
			println!("IO error while reading file {}: {}", args.input, e);
			return;
		},
	};

	for blk in asm.blocks().iter() {
		println!("Code for block at origin 0x{:x}", blk.begin());
		let code = blk.code();
		for i in code.iter() {
			let mut buff: Vec<u8> = Vec::new();
			i.encode(&mut buff).unwrap();
			for b in buff.iter() {			
				print!("{:02x} ", b);
			}
			println!("");
		}
	}
}
