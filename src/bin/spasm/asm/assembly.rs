//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashMap;
use std::error::FromError;
use std::io;
use std::fmt::{Display, Formatter, Error};

use simproc::Inst;

pub type SymbolTable = HashMap<String, usize>;

pub struct CodeBlock<I: Inst> {
	begin: usize,
	code: Vec<I>,
}

impl<I: Inst> CodeBlock<I> {
	pub fn new() -> CodeBlock<I> {
		CodeBlock { begin: 0, code: Vec::new(), }
	}

	pub fn begin(&self) -> usize { self.begin }
	pub fn code(&self) -> &[I] { &self.code[..] }
	pub fn push(&mut self, inst: I) { self.code.push(inst) }
}

#[derive(Debug)]
pub struct ProgramError {
	pub line: usize,
	pub content: String,
	pub reason: String,
}

impl ProgramError {
	pub fn new(line: usize, content: &str, reason: &str) -> ProgramError {
		ProgramError {
			line: line, 
			content: content.to_string(), 
			reason: reason.to_string()
		}
	}

	pub fn new_lexical_error(line: usize, content: &str) -> ProgramError {
		ProgramError::new(line, content, "invalid lexical expression")
	}	
}

impl Display for ProgramError {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
		write!(fmt, "in line {}, `{}`: {}", self.line, self.content, self.reason)
	}
}

pub struct Assembly<I: Inst> {
	code: Vec<CodeBlock<I>>,
	symbols: SymbolTable,
}

#[derive(Debug)]
pub enum AssemblyError {
	Io(io::Error),
	BadProgram(Vec<ProgramError>)
}

impl FromError<io::Error> for AssemblyError {
	fn from_error(err: io::Error) -> AssemblyError {
		AssemblyError::Io(err)
	}
}

impl<I: Inst> Assembly<I> {

	pub fn new() -> Assembly<I> {
		Assembly {
			code: Vec::new(),
			symbols: HashMap::new(),
		}
	}

	pub fn blocks(&self) -> &[CodeBlock<I>] { &self.code[..] }

	pub fn symbols(&self) -> &SymbolTable { &self.symbols }

	pub fn push_code(&mut self, code: CodeBlock<I>) { self.code.push(code) }
}
