//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::fmt;
use std::iter::{IntoIterator, Peekable};
use std::str::FromStr;

use byteorder::{BigEndian, ReadBytesExt};
use rustc_serialize::hex::FromHex;

use simproc::inst::{AddrReg, Reg};

#[derive(Clone, Debug, PartialEq)]
pub struct Line {
    pub row: usize,
    pub content: String,
}

macro_rules! sline {
    ($r:expr, $c:expr) => ($crate::asm::lexer::Line { row: $r, content: $c.to_string() });
}

impl Line {
    pub fn new() -> Self { Line { row: 1, content: String::with_capacity(1024) }}

    fn append(&mut self, s: &str) { self.content.push_str(s); }

    fn next(&self) -> Self { Line {
        row: self.row + 1,
        content: String::with_capacity(1024),
    }}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    AddrReg(AddrReg),
    Colon,
    Comma,
    Direct(String),
    Eol(Line),
    Ident(String),
    Minus,
    Number(i64),
    Reg(Reg),
    String(String),
    Unknown(String),
}

macro_rules! direct { ($i:expr) => (Token::Direct($i.to_string())) }
macro_rules! eol { ($r:expr, $l:expr) => (Token::Eol(sline!($r, $l))) }
macro_rules! ident { ($i:expr) => (Token::Ident($i.to_string())) }

impl fmt::Display for Token {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Token::AddrReg(reg) => write!(fmt, "address register {}", reg),
            &Token::Colon => write!(fmt, ":"),
            &Token::Comma => write!(fmt, ","),
            &Token::Direct(ref name) => write!(fmt, "directive {}", name),
            &Token::Eol(_) => write!(fmt, "end of line"),
            &Token::Ident(ref name) => write!(fmt, "identifier {}", name),
            &Token::Minus => write!(fmt, "-"),
            &Token::Number(n) => write!(fmt, "number {}", n),
            &Token::Reg(reg) => write!(fmt, "register {}", reg),
            &Token::String(ref s) => write!(fmt, "string \"{}\"", s),
            &Token::Unknown(ref token) => write!(fmt, "unknown token `{}`", token),
        }
    }
}

pub type ScannerInput = char;
pub type ScannerOutput = Token;

pub struct Scanner<I : Iterator<Item=ScannerInput>> {
    input: Peekable<I>,
    line: Line,
    exhausted: bool,
}

impl<I : Iterator<Item=ScannerInput>> Scanner<I> {

    pub fn scan<T>(input: T) -> Self where T: IntoIterator<Item=ScannerInput, IntoIter=I> { Scanner {
        input: input.into_iter().peekable(),
        line: Line::new(),
        exhausted: false,
    }}

    fn new_line(&mut self) -> Line {
        let next = self.line.next();
        let prev = self.line.clone();
        self.line = next;
        prev
    }

    fn line_append(&mut self, s: String) -> String {
        self.line.append(&s);
        s
    }

    fn next_char(&mut self) -> Option<ScannerInput> {
        self.input.peek().cloned()
    }

    fn consume(&mut self, n: usize) -> String {
        self.input.by_ref().take(n).collect()
    }

    fn take(&mut self, n: usize) -> String {
        let taken = self.consume(n);
        self.line_append(taken)
    }

    fn take_while<F>(&mut self, f: F) -> String where F: FnMut(&ScannerInput) -> bool {
        let mut pred = f;
        let mut txt = String::with_capacity(256);
        loop {
            match self.next_char() {
                Some(next) => {
                    if (pred)(&next) {
                        txt.push(next);
                        self.input.next();
                    }
                    else { return self.line_append(txt) }
                },
                None => return self.line_append(txt),
            }
        }
    }

    fn take_while_id(&mut self) -> String {
        self.take_while(|c| c.is_alphanumeric() || *c == '_')
    }

    fn skip_whitespaces(&mut self) {
        loop {
            match self.next_char() {
                Some(c) => {
                    if c.is_whitespace() && c != '\n' { self.take(1); }
                    else { return; }
                },
                None => return,
            }
        }
    }

    fn to_addr_reg(r: &str) -> Option<AddrReg> {
        match r.to_ascii_lowercase().trim() {
            "a0" => Some(AddrReg::A0),
            "a1" => Some(AddrReg::A1),
            "a2" => Some(AddrReg::A2),
            "a3" => Some(AddrReg::A3),
            _ => None,
        }
    }

    fn to_reg(r: &str) -> Option<Reg> {
        match r.to_ascii_lowercase().trim() {
            "r0" => Some(Reg::R0),
            "r1" => Some(Reg::R1),
            "r2" => Some(Reg::R2),
            "r3" => Some(Reg::R3),
            "r4" => Some(Reg::R4),
            "r5" => Some(Reg::R5),
            "r6" => Some(Reg::R6),
            "r7" => Some(Reg::R7),
            _ => None,
        }
    }

    fn decode_hex(s: &str) -> i64 {
        if s.starts_with("0x") { return Self::decode_hex(&s[2..]) }
        let buff = format!("{:0>16}", s).from_hex().ok().unwrap();
        (&buff[..]).read_i64::<BigEndian>().ok().unwrap()
    }

    fn decode_dec(s: &str) -> i64 {
        FromStr::from_str(s).ok().unwrap()
    }
}

impl<I : Iterator<Item=ScannerInput>> Iterator for Scanner<I> {

    type Item = ScannerOutput;

    fn next(&mut self) -> Option<Token> {
        self.skip_whitespaces();
        match self.next_char().unwrap_or('\x1a') {
            ';' => {
                self.take_while(|c| *c != '\n');
                match self.next_char() {
                    Some('\n') => { self.consume(1); },
                    Some(_) => unreachable!(),
                    None => { self.exhausted = true; },
                }
                Some(Token::Eol(self.new_line()))
            },
            '\n' => { self.consume(1); Some(Token::Eol(self.new_line())) },
            ':' => { self.take(1); Some(Token::Colon) },
            ',' => { self.take(1); Some(Token::Comma) },
            '-' => { self.take(1); Some(Token::Minus) },
            '"' => {
                self.take(1);
                let s = self.take_while(|c| *c != '\n' && *c != '"');
                if self.next_char() == Some('"') { self.take(1); }
                Some(Token::String(s))
            },
            '0' => {
                let zero = self.take(1);
                let num = match self.next_char() {
                    Some('x') | Some('X') => {
                        let scanned = zero + &self.take(1) + &self.take_while(|c| c.is_digit(16));
                        Self::decode_hex(&scanned)
                    },
                    _ => {
                        let scanned = zero + &self.take_while(|c| c.is_digit(10));
                        Self::decode_dec(&scanned)
                    },
                };
                Some(Token::Number(num))
            },
            '1' ... '9' => {
                let scanned = self.take_while(|c| c.is_digit(10));
                let num = Self::decode_dec(&scanned);
                Some(Token::Number(num))
            },
            'a' ... 'z' | 'A' ... 'Z' | '_' => {
                let scanned = self.take_while_id();
                Self::to_reg(&scanned).map(|r| Token::Reg(r))
                    .or_else(|| Self::to_addr_reg(&scanned).map(|r| Token::AddrReg(r)))
                    .or_else(|| Some(Token::Ident(scanned)))
            },
            '.' => {
                let dot = self.take(1);
                let dirname = self.take_while_id();
                if dirname.is_empty() { Some(Token::Unknown(dot)) }
                else { Some(Token::Direct(dirname)) }
            },
            '\x1a' if !self.exhausted => {
                self.exhausted = true;
                Some(Token::Eol(self.new_line()))
            },
            '\x1a' if self.exhausted => None,
            _ => Some(Token::Unknown(self.take(1))),
        }
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::{AddrReg, Reg};

    use super::*;

    #[test]
    fn should_scan_empty() {
        let mut scanner = Scanner::scan("".chars());
        assert_eq!(Some(Token::Eol(sline!(1, ""))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_comment() {
        let mut scanner = Scanner::scan("; this is a comment".chars());
        assert_eq!(Some(Token::Eol(sline!(1, "; this is a comment"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_comment_with_eol() {
        let mut scanner = Scanner::scan("; this is a comment\n".chars());
        assert_eq!(Some(Token::Eol(sline!(1, "; this is a comment"))), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(2, ""))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_eol() {
        let mut scanner = Scanner::scan("\n".chars());
        assert_eq!(Some(Token::Eol(sline!(1, ""))), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(2, ""))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_colon() {
        let mut scanner = Scanner::scan(":".chars());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, ":"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_comma() {
        let mut scanner = Scanner::scan(",".chars());
        assert_eq!(Some(Token::Comma), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, ","))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_minus() {
        let mut scanner = Scanner::scan("-".chars());
        assert_eq!(Some(Token::Minus), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, "-"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_string() {
        let mut scanner = Scanner::scan("\"foobar\"".chars());
        assert_eq!(Some(Token::String("foobar".to_string())), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, "\"foobar\""))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_string_with_eol_ending() {
        let mut scanner = Scanner::scan("\"foobar\n".chars());
        assert_eq!(Some(Token::String("foobar".to_string())), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, "\"foobar"))), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(2, ""))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_string_with_eof_ending() {
        let mut scanner = Scanner::scan("\"foobar".chars());
        assert_eq!(Some(Token::String("foobar".to_string())), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, "\"foobar"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_register() {
        let mut scanner = Scanner::scan("r0 R0".chars());
        assert_eq!(Some(Token::Reg(Reg::R0)), scanner.next());
        assert_eq!(Some(Token::Reg(Reg::R0)), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, "r0 R0"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_addr_register() {
        let mut scanner = Scanner::scan("a0 A0".chars());
        assert_eq!(Some(Token::AddrReg(AddrReg::A0)), scanner.next());
        assert_eq!(Some(Token::AddrReg(AddrReg::A0)), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, "a0 A0"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_decimal() {
        let mut scanner = Scanner::scan("0 00 1234 01234".chars());
        assert_eq!(Some(Token::Number(0)), scanner.next());
        assert_eq!(Some(Token::Number(0)), scanner.next());
        assert_eq!(Some(Token::Number(1234)), scanner.next());
        assert_eq!(Some(Token::Number(1234)), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, "0 00 1234 01234"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_hexadecimal() {
        let mut scanner = Scanner::scan("0x 0x0 0x00 0x12ab 0x12AB 0x0012ab".chars());
        assert_eq!(Some(Token::Number(0)), scanner.next());
        assert_eq!(Some(Token::Number(0)), scanner.next());
        assert_eq!(Some(Token::Number(0)), scanner.next());
        assert_eq!(Some(Token::Number(4779)), scanner.next());
        assert_eq!(Some(Token::Number(4779)), scanner.next());
        assert_eq!(Some(Token::Number(4779)), scanner.next());
        assert_eq!(
            Some(Token::Eol(sline!(1, "0x 0x0 0x00 0x12ab 0x12AB 0x0012ab"))),
            scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_ident() {
        let mut scanner = Scanner::scan("x abc v0 _ _1 r r8 r10".chars());
        assert_eq!(Some(Token::Ident("x".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident("abc".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident("v0".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident("_".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident("_1".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident("r".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident("r8".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident("r10".to_string())), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, "x abc v0 _ _1 r r8 r10"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_directive() {
        let mut scanner = Scanner::scan(".x .abc .v0 ._ ._1 .r .r8 .r10 .".chars());
        assert_eq!(Some(Token::Direct("x".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct("abc".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct("v0".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct("_".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct("_1".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct("r".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct("r8".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct("r10".to_string())), scanner.next());
        assert_eq!(Some(Token::Unknown(".".to_string())), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, ".x .abc .v0 ._ ._1 .r .r8 .r10 ."))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_ignore_blanks() {
        let mut scanner = Scanner::scan(":\t:\r:".chars());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, ":\t:\r:"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_compute_line_numbers() {
        let mut scanner = Scanner::scan(":\n :\n  :\n".chars());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(1, ":"))), scanner.next());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(2, " :"))), scanner.next());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(3, "  :"))), scanner.next());
        assert_eq!(Some(Token::Eol(sline!(4, ""))), scanner.next());
        assert_eq!(None, scanner.next());
    }
}
