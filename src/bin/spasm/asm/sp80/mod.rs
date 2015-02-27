//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

mod args;
mod inst;

use std::io;
use std::slice::SliceExt;

use simproc::Inst;
use simproc::sp80;
use simproc::sp80::{AssemblyArgs, RuntimeArgs};

use asm::{Assembler, Assembly, AssemblyError, Assembled, ProgramError, SymbolTable};
use asm::parser;
use asm::parser::Token;

pub struct Asm80;

impl Asm80 {

	pub fn new() -> Asm80 { Asm80 }
}

impl Assembler<sp80::Inst<sp80::RuntimeArgs>> for Asm80 {

	fn assemble<R : io::Read>(&self, input: R) -> Result<Assembly<sp80::Inst<RuntimeArgs>>, AssemblyError> {
		let lines = try!(parser::read_lines(input));
		let mut symbols: SymbolTable = SymbolTable::new();
		let mut placement = 0 as usize;
		let mut errors: Vec<ProgramError> = Vec::new();
		let mut assembled: Vec<Assembled<sp80::Inst<AssemblyArgs>>> = Vec::new();

		let tokens = parser::tokenize(&lines);

		// First loop, gather assembled elements and errors
		for i in 0..tokens.len() {
			let tk = &tokens[i];
			let line = &lines[i];
			match tk {
				&Token::Label(ref label) => { 
					symbols.insert(label.clone(), placement);
					assembled.push(Assembled::Ignored(line.clone()));
				},
				&Token::Mnemonic(ref mnemo, ref args) => {
					match inst::assemble_inst(mnemo, args) {
						Ok(inst) => { 
							let next_placement = placement + inst.len(); 
							assembled.push(Assembled::Inst(line.clone(), placement, inst));
							placement = next_placement;
						},
						Err(err) => { 
							errors.push(ProgramError::new(i, &line[..], &err[..]));
							assembled.push(Assembled::Ignored(line.clone()));
						},
					}
				},
				&Token::Blank => {
					assembled.push(Assembled::Ignored(line.clone()));
				},
				&Token::LexicalError => {
					errors.push(ProgramError::new_lexical_error(i, &line[..]));
					assembled.push(Assembled::Ignored(line.clone()));
				},
			};
		}

		// Second loop, encode assembled instructions
		let arg_asmblr = args::ArgAssembler::with_symbols(&symbols);
		let mut assembly = Assembly::with_symbols(&symbols);
		for i in 0..assembled.len() {
			let a = &assembled[i];
			match a {
				&Assembled::Inst(ref l, p, ref inst) => {
					match inst.assemble(&arg_asmblr) {
						Ok(asm_inst) => 
							assembly.push(Assembled::Inst(l.clone(), p, asm_inst)),
						Err(e) => errors.push(
							ProgramError::new(i, l.trim(), &format!("{}", e)[..])),
					}
				},
				&Assembled::Ignored(ref ign) => {
					assembly.push(Assembled::Ignored(ign.clone()));
				},
			}
		}


		if errors.is_empty() { Ok(assembly) }
		else { Err(AssemblyError::BadProgram(errors)) }
	}
}
