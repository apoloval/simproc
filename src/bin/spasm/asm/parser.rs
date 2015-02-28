//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::io;
use std::io::BufRead;
use std::num::Int;
use std::str::FromStr;

use serialize::hex::FromHex;

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

	match regex!(r"^\s*([:alpha:][:word:]*)\s*:\s*(?:;.*)?$").captures(line) {
		Some(caps) => return Some(Token::Label(cap!(caps, 1))),
		None => (),
	}
	match regex!(r"^\s*([:alpha:]+)\s*(?:;.*)?$").captures(line) {
		Some(caps) => return Some(Token::Mnemonic(cap!(caps, 1), vec!())),
		None => (),
	}
	match regex!(r"^\s*([:alpha:]+)\s*([:word:]+)\s*(?:;.*)?$").captures(line) {
		Some(caps) => return Some(Token::Mnemonic(cap!(caps, 1), vec!(cap!(caps, 2)))),
		None => (),
	}
	match regex!(r"^\s*([:alpha:]+)\s*([:word:]+)\s*,\s*([:word:]+)\s*(?:;.*)?$").captures(line) {
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

pub fn parse_dec_num(s: &str) -> Option<i64> { FromStr::from_str(s).ok()}

pub fn parse_hex_num(s: &str) -> Option<i64> {
	if s.trim().starts_with("-0x") { parse_hex_num(&s[1..]).map(|n| -n) }
	else if s.trim().starts_with("0x") {
		let hex = format!("{:0>16}", &s[2..]);
		match hex.from_hex() {
			Ok(buff) => {
				let mut accum = 0 as i64;
				for i in 0..buff.len() { accum |= (buff[i] as i64) << 8*i; }
				Some(Int::from_be(accum))
			},
			Err(_) => None,
		}
	} else { None }
}

pub fn parse_num(s: &str) -> Option<i64> {
	parse_hex_num(s).or(parse_dec_num(s))
}
