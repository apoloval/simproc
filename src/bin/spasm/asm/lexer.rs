//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::iter::{IntoIterator, Peekable};
use std::str::FromStr;

use byteorder::{BigEndian, ReadBytesExt};
use rustc_serialize::hex::FromHex;
use simproc::inst::*;

#[derive(Debug, PartialEq)]
pub enum Token {
    AddrRegister(String, AddrReg),
    Colon,
    Comma,
    Direct(String),
    Eol,
    Ident(String),
    Minus,
    Number(String, i64),
    Register(String, Reg),
    Unknown(String),
}

pub struct Scanner<I : Iterator<Item=char>> {
    input: Peekable<I>
}

impl<I : Iterator<Item=char>> Scanner<I> {

    pub fn scan<T>(input: T) -> Self where T: IntoIterator<Item=char, IntoIter=I> { Scanner {
        input: input.into_iter().peekable()
    }}

    fn next_char(&mut self) -> Option<char> {
        self.input.peek().cloned()
    }

    fn take(&mut self, n: usize) -> String {
        self.input.by_ref().take(n).collect()
    }

    fn take_while<F>(&mut self, f: F) -> String where F: FnMut(&char) -> bool {
        self.input.by_ref().take_while(f).collect()
    }

    fn take_while_id(&mut self) -> String {
        self.take_while(|c| c.is_alphanumeric() || *c == '_')
    }

    fn skip_whitespaces(&mut self) {
        match self.input.peek().cloned() {
            Some(c) => if c.is_whitespace() && c != '\n' {
                self.input.next();
                self.skip_whitespaces();
            },
            None => {},
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

impl<I : Iterator<Item=char>> Iterator for Scanner<I> {

    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.skip_whitespaces();
        match self.next_char().unwrap_or('\x1a') {
            ';' => {
                self.take_while(|c| *c != '\n');
                self.next()
            },
            '\n' => { self.take(1); Some(Token::Eol) },
            ':' => { self.take(1); Some(Token::Colon) },
            ',' => { self.take(1); Some(Token::Comma) },
            '-' => { self.take(1); Some(Token::Minus) },
            '0' => {
                self.take(1);
                let (txt, num) = match self.next_char() {
                    Some('x') | Some('X') => {
                        let txt = "0".to_string() +
                            &self.take(1) +
                            &self.take_while(|c| c.is_digit(16));
                        let num = Self::decode_hex(&txt);
                        (txt, num)
                    },
                    _ => {
                        let txt = "0".to_string() + &self.take_while(|c| c.is_digit(10));
                        let num = Self::decode_dec(&txt);
                        (txt, num)
                    },
                };
                Some(Token::Number(txt, num))
            },
            '1' ... '9' => {
                let txt = self.take_while(|c| c.is_digit(10));
                let num = Self::decode_dec(&txt);
                Some(Token::Number(txt, num))
            },
            'a' ... 'z' | 'A' ... 'Z' | '_' => {
                let txt = self.take_while_id();
                Self::to_reg(&txt).map(|r| Token::Register(txt.clone(), r))
                    .or_else(|| Self::to_addr_reg(&txt).map(|r| Token::AddrRegister(txt.clone(), r)))
                    .or_else(|| Some(Token::Ident(txt)))
            },
            '.' => {
                self.take(1);
                let txt = self.take_while_id();
                if txt.is_empty() { Some(Token::Unknown(".".to_string())) }
                else { Some(Token::Direct(txt)) }
            },
            '\x1a' => None,
            _ => Some(Token::Unknown(self.take(1))),
        }
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use super::*;

    #[test]
    fn should_scan_empty() {
        let mut scanner = Scanner::scan("".chars());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_comment() {
        let mut scanner = Scanner::scan("; this is a comment".chars());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_eol() {
        let mut scanner = Scanner::scan("\n".chars());
        assert_eq!(Some(Token::Eol), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_colon() {
        let mut scanner = Scanner::scan(":".chars());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_comma() {
        let mut scanner = Scanner::scan(",".chars());
        assert_eq!(Some(Token::Comma), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_minus() {
        let mut scanner = Scanner::scan("-".chars());
        assert_eq!(Some(Token::Minus), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_register() {
        let mut scanner = Scanner::scan("r0 R0".chars());
        assert_eq!(Some(Token::Register("r0".to_string(), Reg::R0)), scanner.next());
        assert_eq!(Some(Token::Register("R0".to_string(), Reg::R0)), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_addr_register() {
        let mut scanner = Scanner::scan("a0 A0".chars());
        assert_eq!(Some(Token::AddrRegister("a0".to_string(), AddrReg::A0)), scanner.next());
        assert_eq!(Some(Token::AddrRegister("A0".to_string(), AddrReg::A0)), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_decimal() {
        let mut scanner = Scanner::scan("0 00 1234 01234".chars());
        assert_eq!(Some(Token::Number("0".to_string(), 0)), scanner.next());
        assert_eq!(Some(Token::Number("00".to_string(), 0)), scanner.next());
        assert_eq!(Some(Token::Number("1234".to_string(), 1234)), scanner.next());
        assert_eq!(Some(Token::Number("01234".to_string(), 1234)), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_hexadecimal() {
        let mut scanner = Scanner::scan("0x 0x0 0x00 0x12ab 0x12AB 0x0012ab".chars());
        assert_eq!(Some(Token::Number("0x".to_string(), 0)), scanner.next());
        assert_eq!(Some(Token::Number("0x0".to_string(), 0)), scanner.next());
        assert_eq!(Some(Token::Number("0x00".to_string(), 0)), scanner.next());
        assert_eq!(Some(Token::Number("0x12ab".to_string(), 4779)), scanner.next());
        assert_eq!(Some(Token::Number("0x12AB".to_string(), 4779)), scanner.next());
        assert_eq!(Some(Token::Number("0x0012ab".to_string(), 4779)), scanner.next());
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
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_ignore_blanks() {
        let mut scanner = Scanner::scan(":\t:\r:".chars());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(Some(Token::Colon), scanner.next());
        assert_eq!(None, scanner.next());
    }
}
