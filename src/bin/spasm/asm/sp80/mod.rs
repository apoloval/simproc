//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

mod args;
mod inst;
mod mnemo;

use std::io;
use std::slice::SliceExt;

use simproc::{Encode, Inst};
use simproc::sp80;
use simproc::sp80::{AssemblyArgs, RuntimeArgs};

use asm::{Assembler, Assembly, AssemblyError, Assembled, ProgramError};
use asm::mnemo::*;
use asm::parser;
use asm::parser::Token;

pub struct Asm80;

impl Asm80 {

	pub fn new() -> Asm80 { Asm80 }
}

impl Assembler<sp80::Inst<sp80::RuntimeArgs>> for Asm80 {

	fn assemble<R : io::Read>(&self, input: R) -> Result<Assembly<sp80::Inst<RuntimeArgs>>, AssemblyError> {
		let lines = try!(parser::read_lines(input));
		let mut asm = Assembly::new();
		let mut placement = 0 as usize;
		let mut errors: Vec<ProgramError> = Vec::new();

		let tokens = parser::tokenize(&lines);

		// First loop, lexical error & symbol lookup
		for i in 0..tokens.len() {
			let tk = &tokens[i];
			let line = &lines[i];
			match tk {
				&Token::Mnemonic(ref mnemo, ref args) => {
					match inst::assemble_inst(mnemo, args) {
						Ok(inst) => { 
							placement += inst.len(); 
						},
						Err(err) => { errors.push(ProgramError::new(i, &line[..], &err[..])) },
					}
				},
				&Token::LexicalError => 
					errors.push(ProgramError::new_lexical_error(i, &line[..])),
				&Token::Label(ref label) => { 
					asm.decl_symbol(&label[..], placement); 
				},
				_ => (),
			};
		}
		
		// Second loop, assemble elements
		placement = 0;
		for i in 0..tokens.len() {
			let line = &lines[i];
			let tk = &tokens[i];
			match tk {
				&Token::Mnemonic(ref mnemo, ref args) => {
					match inst::assemble_inst(mnemo, args) {
						Ok(inst) => { 
							placement += inst.len(); 

						},
						Err(err) => { errors.push(ProgramError::new(i, &line[..], &err[..])) },
					}
				},
				_ => asm.push(Assembled::Ignored(line.clone())),
			};
		}

		if errors.is_empty() { Ok(asm) }
		else { Err(AssemblyError::BadProgram(errors)) }
	}
}
