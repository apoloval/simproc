//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::io;
use std::io::BufRead;

pub fn read_lines<R : io::Read>(input: R) -> io::Result<Vec<String>> {
	let mut i = io::BufReader::new(input);
	let mut result: Vec<String> = Vec::new();

	loop {
		let mut line = String::new();
		try!(i.read_line(&mut line));
		if line.is_empty() { return Ok(result); }
		else { result.push(line.trim_right().to_string()) }
	}
}

#[derive(PartialEq, Debug)]
pub enum Token {
	Label(String),
	Mnemonic(String, Vec<String>),
	Blank,
	LexicalError,
}

pub fn parse(line: &str) -> Option<Token> {	

	macro_rules! cap(
		($c:ident, $n:expr) => ($c.at($n).unwrap().to_string())
	);

	match regex!(r"^\s*([:alpha:][:alnum:]*)\s*:\s*(?:;.*)?$").captures(line) {
		Some(caps) => return Some(Token::Label(cap!(caps, 1))),
		None => (),
	}
	match regex!(r"^\s*([:alpha:][:alnum:]*)\s*(?:;.*)?$").captures(line) {
		Some(caps) => return Some(Token::Mnemonic(cap!(caps, 1), vec!())),
		None => (),
	}
	match regex!(r"^\s*([:alpha:]+)\s*([:alnum:]+)\s*(?:;.*)?$").captures(line) {
		Some(caps) => return Some(Token::Mnemonic(cap!(caps, 1), vec!(cap!(caps, 2)))),
		None => (),
	}
	match regex!(r"^\s*([:alpha:]+)\s*([:alnum:]+)\s*,\s*([:alnum:]+)\s*(?:;.*)?$").captures(line) {
		Some(caps) => 
			return Some(Token::Mnemonic(cap!(caps, 1), vec!(cap!(caps, 2), cap!(caps, 3)))),
		None => (),
	}
	match regex!(r"^\s*(?:;.*)?$").captures(line) {
		Some(_) => return Some(Token::Blank),
		None => (),
	}
	None
}

pub fn tokenize(lines: &Vec<String>) -> Vec<Token> {
	let mut tokens: Vec<Token> = Vec::with_capacity(lines.len());
	for line in lines.iter() {
		match parse(line.as_slice()) {
			Some(tk) => tokens.push(tk),
			None => tokens.push(Token::LexicalError),
		}
	}
	tokens
}
