//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::{Display, Error, Formatter};
use std::io;
use std::io::BufRead;
use std::str::FromStr;

use byteorder::{BigEndian, ReadBytesExt};
use regex::Regex;
use rustc_serialize::hex::FromHex;

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

#[derive(Clone, Debug, PartialEq)]
pub struct Parameterized {
    elem: String,
    params: Vec<String>,
}

impl Parameterized {

    pub fn from_strings(elem: String, params: Vec<String>) -> Parameterized {
        Parameterized { elem: elem, params: params.clone() }
    }

    pub fn elem(&self) -> &str { &self.elem[..] }

    pub fn params(&self) -> &Vec<String> { &self.params }
}

impl Display for Parameterized {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        try!(write!(fmt, "{}", self.elem));
        for (i, p) in self.params.iter().enumerate() {
            try!(write!(fmt, "{}", p));
            if i < self.params.len() - 1 {
                try!(write!(fmt, ", "));
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Parsed {
    Label(String),
    Mnemonic(Parameterized),
    Directive(Parameterized),
    Blank,
    LexicalError,
}

macro_rules! mnemo {
    ($dirname:expr) => {
        Parsed::Mnemonic(Parameterized::from_strings($dirname.to_string(), Vec::new()))
    };
    ($dirname:expr, $( $param:expr ),*) => {
        {
            let mut params: Vec<String> = Vec::new();
            $( params.push($param.to_string()); )*
            Parsed::Mnemonic(Parameterized::from_strings($dirname.to_string(), params))
        }
    };
}

macro_rules! dir {
    ($dirname:expr, $( $param:expr ),*) => {
        {
            let mut params: Vec<String> = Vec::new();
            $( params.push($param.to_string()); )*
            Parsed::Directive(Parameterized::from_strings($dirname.to_string(), params))
        }
    }
}

const LABEL_REGEX: &'static str = r"^\s*([:alpha:][:word:]*)\s*:\s*(?:;.*)?$";
const MNEMONIC1_REGEX: &'static str = r"^\s*([:alpha:]+)\s*(?:;.*)?$";
const MNEMONIC2_REGEX: &'static str = r"^\s*([:alpha:]+)\s*([:word:]+)\s*(?:;.*)?$";
const MNEMONIC3_REGEX: &'static str =
    r"^\s*([:alpha:]+)\s*([:word:]+)\s*,\s*([:word:]+)\s*(?:;.*)?$";
const DIRECTIVE_REGEX: &'static str = r"^\s*\.([:alpha:]+)\s*([:word:]+)\s*(?:;.*)?$";
const BLANK_REGEX: &'static str = r"^\s*(?:;.*)?$";

pub fn parse_line(line: &str) -> Option<Parsed> {

    macro_rules! cap(
        ($c:ident, $n:expr) => ($c.at($n).unwrap().to_string())
    );

    match Regex::new(LABEL_REGEX).unwrap().captures(line) {
        Some(caps) => return Some(Parsed::Label(cap!(caps, 1))),
        None => (),
    }
    match Regex::new(MNEMONIC1_REGEX).unwrap().captures(line) {
        Some(caps) => {
            let par = Parameterized::from_strings(cap!(caps, 1), vec!());
            return Some(Parsed::Mnemonic(par))
        },
        None => (),
    }
    match Regex::new(MNEMONIC2_REGEX).unwrap().captures(line) {
        Some(caps) => {
            let par = Parameterized::from_strings(cap!(caps, 1), vec!(cap!(caps, 2)));
            return Some(Parsed::Mnemonic(par))
        },
        None => (),
    }
    match Regex::new(MNEMONIC3_REGEX).unwrap().captures(line) {
        Some(caps) => {
            let par = Parameterized::from_strings(cap!(caps, 1), vec!(cap!(caps, 2), cap!(caps, 3)));
            return Some(Parsed::Mnemonic(par))
        },
        None => (),
    }
    match Regex::new(DIRECTIVE_REGEX).unwrap().captures(line) {
        Some(caps) => {
            let par = Parameterized::from_strings(cap!(caps, 1), vec!(cap!(caps, 2)));
            return Some(Parsed::Directive(par));
        },
        None => (),
    }
    match Regex::new(BLANK_REGEX).unwrap().captures(line) {
        Some(_) => return Some(Parsed::Blank),
        None => (),
    }
    None
}

pub fn parse(lines: &Vec<String>) -> Vec<Parsed> {
    let mut tokens: Vec<Parsed> = Vec::with_capacity(lines.len());
    for line in lines.iter() {
        match parse_line(&line[..]) {
            Some(tk) => tokens.push(tk),
            None => tokens.push(Parsed::LexicalError),
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
            Ok(buff) => (&buff[..]).read_i64::<BigEndian>().ok(),
            Err(_) => None,
        }
    } else { None }
}

pub fn parse_num(s: &str) -> Option<i64> {
    parse_hex_num(s).or(parse_dec_num(s))
}

#[cfg(test)]
mod test {

    use super::*;

    macro_rules! read_lines {
        ($input:expr) => (read_lines(&$input[..]))
    }

    #[test]
    fn should_read_lines() {
        let input = b"hello\nworld\n";
        let lines = read_lines!(input).unwrap();
        assert_eq!(lines[0], "hello");
        assert_eq!(lines[1], "world");
    }

    #[test]
    fn should_read_lines_on_missing_blankend() {
        let input = b"hello\nworld";
        let lines = read_lines!(input).unwrap();
        assert_eq!(lines[0], "hello");
        assert_eq!(lines[1], "world");
    }

    fn should_parse(line: &str, expected: &Parsed) {
        let actual = parse_line(line).unwrap();
        assert_eq!(actual, *expected);
    }

    #[test]
    fn should_parse_label() {
        should_parse("main:", &Parsed::Label("main".to_string()));
    }

    #[test]
    fn should_parse_label_with_comment() {
        should_parse("main: ; comment", &Parsed::Label("main".to_string()));
    }

    #[test]
    fn should_parse_label_with_extra_spaces() {
        should_parse("   \tmain  \t : \t  ", &Parsed::Label("main".to_string()));
    }

    #[test]
    fn should_parse_mnemonic0() {
        should_parse("reti", &mnemo!("reti"));
    }

    #[test]
    fn should_parse_mnemonic0_with_comment() {
        should_parse("reti ; comment", &mnemo!("reti"));
    }

    #[test]
    fn should_parse_mnemonic0_with_extra_spaces() {
        should_parse("   \t reti   \t  ", &mnemo!("reti"));
    }

    #[test]
    fn should_parse_mnemonic1() {
        should_parse("push r1", &mnemo!("push", "r1"));
    }

    #[test]
    fn should_parse_mnemonic1_with_comment() {
        should_parse("push r1 ; comment", &mnemo!("push", "r1"));
    }

    #[test]
    fn should_parse_mnemonic1_with_extra_spaces() {
        should_parse(" \t push \t r1 \t ", &mnemo!("push", "r1"));
    }

    #[test]
    fn should_parse_mnemonic2() {
        should_parse("add r1, r2", &mnemo!("add", "r1", "r2"));
    }

    #[test]
    fn should_parse_mnemonic2_with_comment() {
        should_parse("add r1, r2 ; comment", &mnemo!("add", "r1", "r2"));
    }

    #[test]
    fn should_parse_mnemonic2_with_extra_spaces() {
        should_parse(" \t add \t r1 \t ,  \t  r2  \t ", &mnemo!("add", "r1", "r2"));
    }

    #[test]
    fn should_parse_directive() {
        should_parse(".org 0x8000", &dir!("org", "0x8000"))
    }

    #[test]
    fn should_parse_directive_with_comment() {
        should_parse(".org 0x8000 ; here starts our program", &dir!("org", "0x8000"))
    }

    #[test]
    fn should_parse_directive_with_extra_spaces() {
        should_parse(" \t  .org   \t     0x8000  \t   \t   ", &dir!("org", "0x8000"))
    }

    #[test]
    fn should_parse_blank() {
        should_parse("", &Parsed::Blank);
    }

    #[test]
    fn should_parse_blank_with_comments() {
        should_parse("   ; comment", &Parsed::Blank);
    }

    #[test]
    fn should_parse_blank_with_extra_spaces() {
        should_parse("       ", &Parsed::Blank);
    }

    #[test]
    fn should_parse_lines() {
        let lines = vec!(
            "; This is a test program".to_string(),
            "main:    ; this is the entry point of the program".to_string(),
            "   ldi r0, 5".to_string(),
            "   ldi r1, 4".to_string(),
            "   add r0, r1".to_string(),
        );
        let tokens = parse(&lines);
        assert_eq!(5, tokens.len());
    }

    #[test]
    fn should_detect_syntax_errors() {
        let lines = vec!(
            "; This is a test program".to_string(),
            "main:    ; this is the entry point of the program".to_string(),
            " +- ; this is a syntax error".to_string(),
            "++".to_string(),
            "   add r0, r1".to_string(),
        );
        let tokens = parse(&lines);
        assert_eq!(Parsed::LexicalError, tokens[2]);
        assert_eq!(Parsed::LexicalError, tokens[3]);
    }

    #[test]
    fn should_parse_dec_num() {
        assert_eq!(Some(100), parse_num("100"));
    }

    #[test]
    fn should_parse_neg_dec_num() {
        assert_eq!(Some(-100), parse_num("-100"));
    }

    #[test]
    fn should_parse_hex_num() {
        assert_eq!(Some(0x100), parse_num("0x100"));
    }

    #[test]
    fn should_parse_neg_hex_num() {
        assert_eq!(Some(-0x100), parse_num("-0x100"));
    }

}
