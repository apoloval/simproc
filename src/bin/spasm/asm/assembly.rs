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


pub enum Assembled<I: Inst> {
	Inst(String, usize, I), // (line: String, placement: usize, inst: I)
	Ignored(String), // (line: String)
}


pub struct Assembly<I: Inst> {
	symbols: SymbolTable,
	assembled: Vec<Assembled<I>>,
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
			symbols: HashMap::new(),
			assembled: Vec::new(),
		}
	}

	pub fn symbols(&self) -> &SymbolTable { &self.symbols }

	pub fn decl_symbol(&mut self, name: &str, value: usize) -> Option<usize> {
		self.symbols.insert(name.to_string(), value)
	}

	pub fn push(&mut self, code: Assembled<I>) { self.assembled.push(code) }

	pub fn assembled(&self) -> &[Assembled<I>] { &self.assembled[..] }
}
