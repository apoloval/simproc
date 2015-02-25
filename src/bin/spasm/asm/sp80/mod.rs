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

use simproc::Inst;
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
						 placement: usize) -> Result<Sp80Inst, String> {
		match mnemonic {
			"add" | "ADD" => 
				assemble_mnemo!(Sp80Inst::Add => binary from args, symbols, placement),
			"addw" | "ADDW" => 
				assemble_mnemo!(Sp80Inst::Addw => binary from args, symbols, placement),
			"addi" | "ADDI" => 
				assemble_mnemo!(Sp80Inst::Addi => binary from args, symbols, placement),
			"sub" | "SUB" => 
				assemble_mnemo!(Sp80Inst::Sub => binary from args, symbols, placement),
			"subw" | "SUBW" => 
				assemble_mnemo!(Sp80Inst::Subw => binary from args, symbols, placement),
			"subi" | "SUBI" => 
				assemble_mnemo!(Sp80Inst::Subi => binary from args, symbols, placement),
			"mulw" | "MULW" => 
				assemble_mnemo!(Sp80Inst::Mulw => binary from args, symbols, placement),
			"and" | "AND" => 
				assemble_mnemo!(Sp80Inst::And => binary from args, symbols, placement),
			"or" | "OR" => 
				assemble_mnemo!(Sp80Inst::Or => binary from args, symbols, placement),
			"xor" | "XOR" => 
				assemble_mnemo!(Sp80Inst::Xor => binary from args, symbols, placement),
			"lsl" | "LSL" => 
				assemble_mnemo!(Sp80Inst::Lsl => binary from args, symbols, placement),
			"lsr" | "LSR" => 
				assemble_mnemo!(Sp80Inst::Lsr => binary from args, symbols, placement),
			"asr" | "ASR" => 
				assemble_mnemo!(Sp80Inst::Asr => binary from args, symbols, placement),
			"not" | "NOT" => 
				assemble_mnemo!(Sp80Inst::Not => unary from args, symbols, placement),
			"comp" | "COMP" => 
				assemble_mnemo!(Sp80Inst::Comp => unary from args, symbols, placement),
			"inc" | "INC" => 
				assemble_mnemo!(Sp80Inst::Inc => unary from args, symbols, placement),
			"incw" | "INCW" => 
				assemble_mnemo!(Sp80Inst::Incw => unary from args, symbols, placement),
			"dec" | "DEC" => 
				assemble_mnemo!(Sp80Inst::Dec => unary from args, symbols, placement),
			"decw" | "DECW" => 
				assemble_mnemo!(Sp80Inst::Decw => unary from args, symbols, placement),
			"mov" | "MOV" => 
				assemble_mnemo!(Sp80Inst::Mov => binary from args, symbols, placement),
			"ld" | "LD" => 
				assemble_mnemo!(Sp80Inst::Ld => binary from args, symbols, placement),
			"st" | "ST" => 
				assemble_mnemo!(Sp80Inst::St => binary from args, symbols, placement),
			"ldd" | "LDD" => 
				assemble_mnemo!(Sp80Inst::Ldd => binary from args, symbols, placement),
			"std" | "STD" => 
				assemble_mnemo!(Sp80Inst::Std => binary from args, symbols, placement),
			"ldi" | "LDI" => 
				assemble_mnemo!(Sp80Inst::Ldi => binary from args, symbols, placement),
			"ldsp" | "LDSP" => 
				assemble_mnemo!(Sp80Inst::Ldsp => unary from args, symbols, placement),
			"push" | "PUSH" => 
				assemble_mnemo!(Sp80Inst::Push => unary from args, symbols, placement),
			"pop" | "POP" => 
				assemble_mnemo!(Sp80Inst::Pop => unary from args, symbols, placement),
			"je" | "JE" => 
				assemble_mnemo!(Sp80Inst::Je => unary from args, symbols, placement),
			"jne" | "JNE" => 
				assemble_mnemo!(Sp80Inst::Jne => unary from args, symbols, placement),
			"jl" | "JL" => 
				assemble_mnemo!(Sp80Inst::Jl => unary from args, symbols, placement),
			"jge" | "JGE" => 
				assemble_mnemo!(Sp80Inst::Jge => unary from args, symbols, placement),
			"jcc" | "JCC" => 
				assemble_mnemo!(Sp80Inst::Jcc => unary from args, symbols, placement),
			"jcs" | "JCS" => 
				assemble_mnemo!(Sp80Inst::Jcs => unary from args, symbols, placement),
			"jvc" | "JVC" => 
				assemble_mnemo!(Sp80Inst::Jvc => unary from args, symbols, placement),
			"jvs" | "JVS" => 
				assemble_mnemo!(Sp80Inst::Jvs => unary from args, symbols, placement),
			"jmp" | "JMP" => 
				assemble_mnemo!(Sp80Inst::Jmp => unary from args, symbols, placement),
			"rjmp" | "RJMP" => 
				assemble_mnemo!(Sp80Inst::Rjmp => unary from args, symbols, placement),
			"ijmp" | "IJMP" => 
				assemble_mnemo!(Sp80Inst::Ijmp => unary from args, symbols, placement),
			"call" | "CALL" => 
				assemble_mnemo!(Sp80Inst::Call => unary from args, symbols, placement),
			"rcall" | "RCALL" => 
				assemble_mnemo!(Sp80Inst::Rcall => unary from args, symbols, placement),
			"icall" | "ICALL" => 
				assemble_mnemo!(Sp80Inst::Icall => unary from args, symbols, placement),
			"ret" | "RET" => 
				assemble_mnemo!(Sp80Inst::Ret => nullary from args),
			"reti" | "RETI" => 
				assemble_mnemo!(Sp80Inst::Reti => nullary from args),
			"nop" | "NOP" => 
				assemble_mnemo!(Sp80Inst::Nop => nullary from args),
			"halt" | "HALT" => 
				assemble_mnemo!(Sp80Inst::Halt => nullary from args),
			_ => Err(format!("unknown mnemonic: `{}`", mnemonic))
		}
	}	
}

impl Assembler<Sp80Inst> for Asm80 {

	fn assemble<R : io::Read>(&self, input: R) -> Result<Assembly<Sp80Inst>, AssemblyError> {
		let lines = try!(parser::read_lines(input));
		let mut asm = Assembly::new();
		let mut placement = 0 as usize;
		let mut errors: Vec<ProgramError> = Vec::new();

		let tokens = parser::tokenize(&lines);
		
		for i in 0..tokens.len() {
			let line = &lines[i];
			let tk = &tokens[i];
			match tk {
				&Token::LexicalError => 
					errors.push(ProgramError::new_lexical_error(i, &lines[i][..])),
				&Token::Mnemonic(ref mnemo, ref args) => {
					let assembled = Asm80::assemble_mnemonic(
						mnemo.as_slice(), &args[..], asm.symbols(), placement);
					match assembled {
						Ok(inst) => {
							placement += inst.len();
							asm.push(Assembled::Inst(line.clone(), inst));
						},
						Err(e) => 
							errors.push(ProgramError::new(i, &line[..], &e[..])),
					}
				},
				_ => asm.push(Assembled::Ignored(line.clone())),
			};
		}

		if errors.is_empty() { Ok(asm) }
		else { Err(AssemblyError::BadProgram(errors)) }
	}
}
