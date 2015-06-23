//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

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

#[derive(PartialEq, Debug)]
pub enum Token {
    Label(String),
    Mnemonic(String, Vec<String>),
    Blank,
    LexicalError,
}

const LABEL_REGEX: &'static str = r"^\s*([:alpha:][:word:]*)\s*:\s*(?:;.*)?$";
const MNEMONIC1_REGEX: &'static str = r"^\s*([:alpha:]+)\s*(?:;.*)?$";
const MNEMONIC2_REGEX: &'static str = r"^\s*([:alpha:]+)\s*([:word:]+)\s*(?:;.*)?$";
const MNEMONIC3_REGEX: &'static str =
    r"^\s*([:alpha:]+)\s*([:word:]+)\s*,\s*([:word:]+)\s*(?:;.*)?$";
const BLANK_REGEX: &'static str = r"^\s*(?:;.*)?$";

pub fn parse(line: &str) -> Option<Token> {

    macro_rules! cap(
        ($c:ident, $n:expr) => ($c.at($n).unwrap().to_string())
    );

    match Regex::new(LABEL_REGEX).unwrap().captures(line) {
        Some(caps) => return Some(Token::Label(cap!(caps, 1))),
        None => (),
    }
    match Regex::new(MNEMONIC1_REGEX).unwrap().captures(line) {
        Some(caps) => return Some(Token::Mnemonic(cap!(caps, 1), vec!())),
        None => (),
    }
    match Regex::new(MNEMONIC2_REGEX).unwrap().captures(line) {
        Some(caps) => return Some(Token::Mnemonic(cap!(caps, 1), vec!(cap!(caps, 2)))),
        None => (),
    }
    match Regex::new(MNEMONIC3_REGEX).unwrap().captures(line) {
        Some(caps) =>
            return Some(Token::Mnemonic(cap!(caps, 1), vec!(cap!(caps, 2), cap!(caps, 3)))),
        None => (),
    }
    match Regex::new(BLANK_REGEX).unwrap().captures(line) {
        Some(_) => return Some(Token::Blank),
        None => (),
    }
    None
}

pub fn tokenize(lines: &Vec<String>) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::with_capacity(lines.len());
    for line in lines.iter() {
        match parse(&line[..]) {
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

    fn should_parse(line: &str, expected: &Token) {
        let actual = parse(line).unwrap();
        assert_eq!(actual, *expected);
    }

    #[test]
    fn should_parse_label() {
        should_parse("main:", &Token::Label("main".to_string()));
    }

    #[test]
    fn should_parse_label_with_comment() {
        should_parse("main: ; comment", &Token::Label("main".to_string()));
    }

    #[test]
    fn should_parse_label_with_extra_spaces() {
        should_parse("   \tmain  \t : \t  ", &Token::Label("main".to_string()));
    }

    #[test]
    fn should_parse_mnemonic0() {
        should_parse("reti", &Token::Mnemonic("reti".to_string(), vec!()));
    }

    #[test]
    fn should_parse_mnemonic0_with_comment() {
        should_parse("reti ; comment", &Token::Mnemonic("reti".to_string(), vec!()));
    }

    #[test]
    fn should_parse_mnemonic0_with_extra_spaces() {
        should_parse("   \t reti   \t  ", &Token::Mnemonic("reti".to_string(), vec!()));
    }

    #[test]
    fn should_parse_mnemonic1() {
        should_parse("push r1", &Token::Mnemonic("push".to_string(), vec!("r1".to_string())));
    }

    #[test]
    fn should_parse_mnemonic1_with_comment() {
        should_parse("push r1 ; comment", &Token::Mnemonic("push".to_string(), vec!("r1".to_string())));
    }

    #[test]
    fn should_parse_mnemonic1_with_extra_spaces() {
        should_parse(" \t push \t r1 \t ",
            &Token::Mnemonic("push".to_string(), vec!("r1".to_string())));
    }

    #[test]
    fn should_parse_mnemonic2() {
        should_parse("add r1, r2",
            &Token::Mnemonic("add".to_string(), vec!("r1".to_string(), "r2".to_string())));
    }

    #[test]
    fn should_parse_mnemonic2_with_comment() {
        should_parse("add r1, r2 ; comment",
            &Token::Mnemonic("add".to_string(), vec!("r1".to_string(), "r2".to_string())));
    }

    #[test]
    fn should_parse_mnemonic2_with_extra_spaces() {
        should_parse(" \t add \t r1 \t ,  \t  r2  \t ",
            &Token::Mnemonic("add".to_string(), vec!("r1".to_string(), "r2".to_string())));
    }

    #[test]
    fn should_parse_blank() {
        should_parse("", &Token::Blank);
    }

    #[test]
    fn should_parse_blank_with_comments() {
        should_parse("   ; comment", &Token::Blank);
    }

    #[test]
    fn should_parse_blank_with_extra_spaces() {
        should_parse("       ", &Token::Blank);
    }

    #[test]
    fn should_tokenize() {
        let lines = vec!(
            "; This is a test program".to_string(),
            "main:    ; this is the entry point of the program".to_string(),
            "   ldi r0, 5".to_string(),
            "   ldi r1, 4".to_string(),
            "   add r0, r1".to_string(),
        );
        let tokens = tokenize(&lines);
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
        let tokens = tokenize(&lines);
        assert_eq!(Token::LexicalError, tokens[2]);
        assert_eq!(Token::LexicalError, tokens[3]);
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
