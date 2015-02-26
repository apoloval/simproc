//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

mod mnemo;

use std::io;
use std::slice::SliceExt;

use simproc::Encode;
use simproc::sp80::*;

use asm::*;
use asm::mnemo::*;
use asm::parser;
use asm::parser::Token;

pub struct Asm80;

impl Asm80 {

	pub fn new() -> Asm80 { Asm80 }

	/// Convert the given mnemonic into a SP80 instruction
	fn assemble_mnemonic(mnemonic: &str, 
						 args: &[String], 
						 symbols: &SymbolTable,
						 placement: usize) -> Result<Inst, String> {
		match mnemonic {
			"add" | "ADD" => 
				assemble_mnemo!(Inst::Add => binary from args, symbols, placement),
			"addw" | "ADDW" => 
				assemble_mnemo!(Inst::Addw => binary from args, symbols, placement),
			"addi" | "ADDI" => 
				assemble_mnemo!(Inst::Addi => binary from args, symbols, placement),
			"sub" | "SUB" => 
				assemble_mnemo!(Inst::Sub => binary from args, symbols, placement),
			"subw" | "SUBW" => 
				assemble_mnemo!(Inst::Subw => binary from args, symbols, placement),
			"subi" | "SUBI" => 
				assemble_mnemo!(Inst::Subi => binary from args, symbols, placement),
			"mulw" | "MULW" => 
				assemble_mnemo!(Inst::Mulw => binary from args, symbols, placement),
			"and" | "AND" => 
				assemble_mnemo!(Inst::And => binary from args, symbols, placement),
			"or" | "OR" => 
				assemble_mnemo!(Inst::Or => binary from args, symbols, placement),
			"xor" | "XOR" => 
				assemble_mnemo!(Inst::Xor => binary from args, symbols, placement),
			"lsl" | "LSL" => 
				assemble_mnemo!(Inst::Lsl => binary from args, symbols, placement),
			"lsr" | "LSR" => 
				assemble_mnemo!(Inst::Lsr => binary from args, symbols, placement),
			"asr" | "ASR" => 
				assemble_mnemo!(Inst::Asr => binary from args, symbols, placement),
			"not" | "NOT" => 
				assemble_mnemo!(Inst::Not => unary from args, symbols, placement),
			"comp" | "COMP" => 
				assemble_mnemo!(Inst::Comp => unary from args, symbols, placement),
			"inc" | "INC" => 
				assemble_mnemo!(Inst::Inc => unary from args, symbols, placement),
			"incw" | "INCW" => 
				assemble_mnemo!(Inst::Incw => unary from args, symbols, placement),
			"dec" | "DEC" => 
				assemble_mnemo!(Inst::Dec => unary from args, symbols, placement),
			"decw" | "DECW" => 
				assemble_mnemo!(Inst::Decw => unary from args, symbols, placement),
			"mov" | "MOV" => 
				assemble_mnemo!(Inst::Mov => binary from args, symbols, placement),
			"ld" | "LD" => 
				assemble_mnemo!(Inst::Ld => binary from args, symbols, placement),
			"st" | "ST" => 
				assemble_mnemo!(Inst::St => binary from args, symbols, placement),
			"ldd" | "LDD" => 
				assemble_mnemo!(Inst::Ldd => binary from args, symbols, placement),
			"std" | "STD" => 
				assemble_mnemo!(Inst::Std => binary from args, symbols, placement),
			"ldi" | "LDI" => 
				assemble_mnemo!(Inst::Ldi => binary from args, symbols, placement),
			"ldsp" | "LDSP" => 
				assemble_mnemo!(Inst::Ldsp => unary from args, symbols, placement),
			"push" | "PUSH" => 
				assemble_mnemo!(Inst::Push => unary from args, symbols, placement),
			"pop" | "POP" => 
				assemble_mnemo!(Inst::Pop => unary from args, symbols, placement),
			"je" | "JE" => 
				assemble_mnemo!(Inst::Je => unary from args, symbols, placement),
			"jne" | "JNE" => 
				assemble_mnemo!(Inst::Jne => unary from args, symbols, placement),
			"jl" | "JL" => 
				assemble_mnemo!(Inst::Jl => unary from args, symbols, placement),
			"jge" | "JGE" => 
				assemble_mnemo!(Inst::Jge => unary from args, symbols, placement),
			"jcc" | "JCC" => 
				assemble_mnemo!(Inst::Jcc => unary from args, symbols, placement),
			"jcs" | "JCS" => 
				assemble_mnemo!(Inst::Jcs => unary from args, symbols, placement),
			"jvc" | "JVC" => 
				assemble_mnemo!(Inst::Jvc => unary from args, symbols, placement),
			"jvs" | "JVS" => 
				assemble_mnemo!(Inst::Jvs => unary from args, symbols, placement),
			"jmp" | "JMP" => 
				assemble_mnemo!(Inst::Jmp => unary from args, symbols, placement),
			"rjmp" | "RJMP" => 
				assemble_mnemo!(Inst::Rjmp => unary from args, symbols, placement),
			"ijmp" | "IJMP" => 
				assemble_mnemo!(Inst::Ijmp => unary from args, symbols, placement),
			"call" | "CALL" => 
				assemble_mnemo!(Inst::Call => unary from args, symbols, placement),
			"rcall" | "RCALL" => 
				assemble_mnemo!(Inst::Rcall => unary from args, symbols, placement),
			"icall" | "ICALL" => 
				assemble_mnemo!(Inst::Icall => unary from args, symbols, placement),
			"ret" | "RET" => 
				assemble_mnemo!(Inst::Ret => nullary from args),
			"reti" | "RETI" => 
				assemble_mnemo!(Inst::Reti => nullary from args),
			"nop" | "NOP" => 
				assemble_mnemo!(Inst::Nop => nullary from args),
			"halt" | "HALT" => 
				assemble_mnemo!(Inst::Halt => nullary from args),
			_ => Err(format!("unknown mnemonic: `{}`", mnemonic))
		}
	}	
}

impl Assembler<Inst> for Asm80 {

	fn assemble<R : io::Read>(&self, input: R) -> Result<Assembly<Inst>, AssemblyError> {
		let lines = try!(parser::read_lines(input));
		let mut asm = Assembly::new();
		let mut placement = 0 as usize;
		let mut errors: Vec<ProgramError> = Vec::new();

		let tokens = parser::tokenize(&lines);

		// First loop, lexical error & symbol lookup
		for i in 0..tokens.len() {
			let tk = &tokens[i];
			match tk {
				&Token::LexicalError => 
					errors.push(ProgramError::new_lexical_error(i, &lines[i][..])),
				&Token::Label(ref label) => { 
					asm.decl_symbol(&label[..], placement); 
				},
				_ => (),
			};
		}
		
		// Second loop, assemble elements
		for i in 0..tokens.len() {
			let line = &lines[i];
			let tk = &tokens[i];
			match tk {
				&Token::Mnemonic(ref mnemo, ref args) => {
					let assembled = Asm80::assemble_mnemonic(
						mnemo.as_slice(), &args[..], asm.symbols(), placement);
					match assembled {
						Ok(inst) => {
							let next_place = placement + inst.len();
							asm.push(Assembled::Inst(line.clone(), placement, inst));
							placement = next_place;
						},
						Err(e) => 
							errors.push(ProgramError::new(i, &line[..], &e[..])),
					}
				},
				&Token::Label(_) => {
					asm.push(Assembled::Ignored(line.clone()));
				},
				_ => asm.push(Assembled::Ignored(line.clone())),
			};
		}

		if errors.is_empty() { Ok(asm) }
		else { Err(AssemblyError::BadProgram(errors)) }
	}
}
