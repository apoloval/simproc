//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::iter::{IntoIterator, Peekable};
use std::ops::Add;
use std::str::FromStr;

use byteorder::{BigEndian, ReadBytesExt};
use rustc_serialize::hex::FromHex;
use simproc::inst::*;

#[derive(Clone, Debug, PartialEq)]
pub struct TextLoc {
    pub line: usize,
    pub col: usize,
    pub txt: String,
}

macro_rules! loc {
    ($l:expr, $c:expr, $t:expr) => (TextLoc { line: $l, col: $c, txt: $t.to_string() })
}

impl TextLoc {
    pub fn undef() -> TextLoc {
        TextLoc { line: 0, col: 0, txt: "".to_string() }
    }

    pub fn is_undef(&self) -> bool {
        self.line == 0 && self.col == 0 && self.txt == ""
    }
}

impl<'a> Add<&'a TextLoc> for TextLoc {

    type Output = Self;

    fn add(self, rhs: &TextLoc) -> Self {
        if rhs.is_undef() { return self }
        let ns = rhs.col - (self.col + self.txt.len());
        let spaces: String = (0..ns).map(|_| ' ').collect();
        let ntext = self.txt + &spaces + &rhs.txt;
        TextLoc {
            line: self.line,
            col: self.col,
            txt: ntext,
        }
    }
}

pub trait TextLocate {
    fn loc(&self) -> &TextLoc;
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    AddrRegister(TextLoc, AddrReg),
    Colon(TextLoc),
    Comma(TextLoc),
    Direct(TextLoc, String),
    Eol(TextLoc),
    Ident(TextLoc, String),
    Minus(TextLoc),
    Number(TextLoc, i64),
    Register(TextLoc, Reg),
    Unknown(TextLoc),
}

impl TextLocate for Token {
    fn loc(&self) -> &TextLoc {
        match self {
            &Token::AddrRegister(ref loc, _) => loc,
            &Token::Colon(ref loc) => loc,
            &Token::Comma(ref loc) => loc,
            &Token::Direct(ref loc, _) => loc,
            &Token::Eol(ref loc) => loc,
            &Token::Ident(ref loc, _) => loc,
            &Token::Minus(ref loc) => loc,
            &Token::Number(ref loc, _) => loc,
            &Token::Register(ref loc, _) => loc,
            &Token::Unknown(ref loc) => loc,
        }
    }
}

macro_rules! colon { ($l:expr, $c:expr) => (Token::Colon(loc!($l, $c, ":"))) }
macro_rules! comma { ($l:expr, $c:expr) => (Token::Comma(loc!($l, $c, ","))) }
macro_rules! direct {
    ($l:expr, $c:expr, $i:expr) => (Token::Direct(loc!($l, $c, format!(".{}", $i)), $i.to_string()))
}
macro_rules! eol { ($l:expr, $c:expr) => (Token::Eol(loc!($l, $c, "\n"))) }
macro_rules! ident {
    ($l:expr, $c:expr, $i:expr) => (Token::Ident(loc!($l, $c, $i), $i.to_string()))
}
macro_rules! number {
    ($l:expr, $c:expr, $n:expr) => (Token::Number(loc!($l, $c, format!("{}", $n)), $n))
}
macro_rules! reg {
    ($l:expr, $c:expr, $r:expr) => (Token::Register(loc!($l, $c, format!("{}", $r)), $r))
}

pub struct Scanner<I : Iterator<Item=char>> {
    input: Peekable<I>,
    line: usize,
    col: usize,
}

impl<I : Iterator<Item=char>> Scanner<I> {

    pub fn scan<T>(input: T) -> Self where T: IntoIterator<Item=char, IntoIter=I> { Scanner {
        input: input.into_iter().peekable(),
        line: 1,
        col: 1,
    }}

    fn new_line(&mut self) { self.line += 1; self.col = 1; }

    fn new_col(&mut self) { self.col += 1; }

    fn next_char(&mut self) -> Option<char> {
        self.input.peek().cloned()
    }

    fn take(&mut self, n: usize) -> TextLoc {
        let txt = self.input.by_ref().take(n).collect();
        self.loc_from(txt)
    }

    fn take_while<F>(&mut self, f: F) -> TextLoc where F: FnMut(&char) -> bool {
        let mut pred = f;
        let mut txt = String::with_capacity(256);
        loop {
            match self.next_char() {
                Some(next) => {
                    if (pred)(&next) {
                        txt.push(next);
                        self.input.next();
                    }
                    else { return self.loc_from(txt) }
                },
                None => return self.loc_from(txt),
            }
        }
    }

    fn take_while_id(&mut self) -> TextLoc {
        self.take_while(|c| c.is_alphanumeric() || *c == '_')
    }

    fn loc_from(&mut self, txt: String) -> TextLoc {
        let result = TextLoc {
            line: self.line,
            col: self.col,
            txt: txt,
        };
        for s in result.txt.chars() {
            if s == '\n' { self.new_line() }
            else { self.new_col() }
        }
        result
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

impl<I : Iterator<Item=char>> Iterator for Scanner<I> {

    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.skip_whitespaces();
        match self.next_char().unwrap_or('\x1a') {
            ';' => {
                self.take_while(|c| *c != '\n');
                self.next()
            },
            '\n' => { Some(Token::Eol(self.take(1))) },
            ':' => { Some(Token::Colon(self.take(1))) },
            ',' => { Some(Token::Comma(self.take(1))) },
            '-' => { Some(Token::Minus(self.take(1))) },
            '0' => {
                let zero = self.take(1);
                let (loc, num) = match self.next_char() {
                    Some('x') | Some('X') => {
                        let loc = zero + &self.take(1) + &self.take_while(|c| c.is_digit(16));
                        let num = Self::decode_hex(&loc.txt);
                        (loc, num)
                    },
                    _ => {
                        let loc = zero + &self.take_while(|c| c.is_digit(10));
                        let num = Self::decode_dec(&loc.txt);
                        (loc, num)
                    },
                };
                Some(Token::Number(loc, num))
            },
            '1' ... '9' => {
                let loc = self.take_while(|c| c.is_digit(10));
                let num = Self::decode_dec(&loc.txt);
                Some(Token::Number(loc, num))
            },
            'a' ... 'z' | 'A' ... 'Z' | '_' => {
                let loc = self.take_while_id();
                Self::to_reg(&loc.txt).map(|r| Token::Register(loc.clone(), r))
                    .or_else(|| Self::to_addr_reg(&loc.txt).map(|r| Token::AddrRegister(loc.clone(), r)))
                    .or_else(|| {
                        let id = loc.txt.clone();
                        Some(Token::Ident(loc, id))
                    })
            },
            '.' => {
                let dot = self.take(1);
                let dirname = self.take_while_id();
                if dirname.txt.is_empty() { Some(Token::Unknown(dot)) }
                else {
                    let dir = dirname.txt.clone();
                    Some(Token::Direct(dot + &dirname, dir))
                }
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
    fn should_add_locs() {
        assert_eq!(
            loc!(1, 1, "a") + &loc!(1, 2, "b"),
            loc!(1, 1, "ab"));
    }

    #[test]
    fn should_add_locs_considering_spaces() {
        assert_eq!(
            loc!(1, 1, "a") + &loc!(1, 3, "b"),
            loc!(1, 1, "a b"));
    }

    #[test]
    fn should_add_undefined_locs() {
        assert_eq!(
            loc!(1, 1, "a") + &TextLoc::undef(),
            loc!(1, 1, "a"));
    }

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
        assert_eq!(Some(Token::Eol(loc!(1, 1, "\n"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_colon() {
        let mut scanner = Scanner::scan(":".chars());
        assert_eq!(Some(Token::Colon(loc!(1, 1, ":"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_comma() {
        let mut scanner = Scanner::scan(",".chars());
        assert_eq!(Some(Token::Comma(loc!(1, 1, ","))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_minus() {
        let mut scanner = Scanner::scan("-".chars());
        assert_eq!(Some(Token::Minus(loc!(1, 1, "-"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_register() {
        let mut scanner = Scanner::scan("r0 R0".chars());
        assert_eq!(Some(Token::Register(loc!(1, 1, "r0"), Reg::R0)), scanner.next());
        assert_eq!(Some(Token::Register(loc!(1, 4, "R0"), Reg::R0)), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_addr_register() {
        let mut scanner = Scanner::scan("a0 A0".chars());
        assert_eq!(Some(Token::AddrRegister(loc!(1, 1, "a0"), AddrReg::A0)), scanner.next());
        assert_eq!(Some(Token::AddrRegister(loc!(1, 4, "A0"), AddrReg::A0)), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_decimal() {
        let mut scanner = Scanner::scan("0 00 1234 01234".chars());
        assert_eq!(Some(Token::Number(loc!(1, 1, "0"), 0)), scanner.next());
        assert_eq!(Some(Token::Number(loc!(1, 3, "00"), 0)), scanner.next());
        assert_eq!(Some(Token::Number(loc!(1, 6, "1234"), 1234)), scanner.next());
        assert_eq!(Some(Token::Number(loc!(1, 11, "01234"), 1234)), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_hexadecimal() {
        let mut scanner = Scanner::scan("0x 0x0 0x00 0x12ab 0x12AB 0x0012ab".chars());
        assert_eq!(Some(Token::Number(loc!(1, 1, "0x"), 0)), scanner.next());
        assert_eq!(Some(Token::Number(loc!(1, 4, "0x0"), 0)), scanner.next());
        assert_eq!(Some(Token::Number(loc!(1, 8, "0x00"), 0)), scanner.next());
        assert_eq!(Some(Token::Number(loc!(1, 13, "0x12ab"), 4779)), scanner.next());
        assert_eq!(Some(Token::Number(loc!(1, 20, "0x12AB"), 4779)), scanner.next());
        assert_eq!(Some(Token::Number(loc!(1, 27, "0x0012ab"), 4779)), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_ident() {
        let mut scanner = Scanner::scan("x abc v0 _ _1 r r8 r10".chars());
        assert_eq!(Some(Token::Ident(loc!(1, 1, "x"), "x".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident(loc!(1, 3, "abc"), "abc".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident(loc!(1, 7, "v0"), "v0".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident(loc!(1, 10, "_"), "_".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident(loc!(1, 12, "_1"), "_1".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident(loc!(1, 15, "r"), "r".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident(loc!(1, 17, "r8"), "r8".to_string())), scanner.next());
        assert_eq!(Some(Token::Ident(loc!(1, 20, "r10"), "r10".to_string())), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_scan_directive() {
        let mut scanner = Scanner::scan(".x .abc .v0 ._ ._1 .r .r8 .r10 .".chars());
        assert_eq!(Some(Token::Direct(loc!(1, 1, ".x"), "x".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct(loc!(1, 4, ".abc"), "abc".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct(loc!(1, 9, ".v0"), "v0".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct(loc!(1, 13, "._"), "_".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct(loc!(1, 16, "._1"), "_1".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct(loc!(1, 20, ".r"), "r".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct(loc!(1, 23, ".r8"), "r8".to_string())), scanner.next());
        assert_eq!(Some(Token::Direct(loc!(1, 27, ".r10"), "r10".to_string())), scanner.next());
        assert_eq!(Some(Token::Unknown(loc!(1, 32, "."))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_ignore_blanks() {
        let mut scanner = Scanner::scan(":\t:\r:".chars());
        assert_eq!(Some(Token::Colon(loc!(1, 1, ":"))), scanner.next());
        assert_eq!(Some(Token::Colon(loc!(1, 3, ":"))), scanner.next());
        assert_eq!(Some(Token::Colon(loc!(1, 5, ":"))), scanner.next());
        assert_eq!(None, scanner.next());
    }

    #[test]
    fn should_compute_line_numbers() {
        let mut scanner = Scanner::scan(":\n :\n  :\n".chars());
        assert_eq!(Some(Token::Colon(loc!(1, 1, ":"))), scanner.next());
        assert_eq!(Some(Token::Eol(loc!(1, 2, "\n"))), scanner.next());
        assert_eq!(Some(Token::Colon(loc!(2, 2, ":"))), scanner.next());
        assert_eq!(Some(Token::Eol(loc!(2, 3, "\n"))), scanner.next());
        assert_eq!(Some(Token::Colon(loc!(3, 3, ":"))), scanner.next());
        assert_eq!(Some(Token::Eol(loc!(3, 4, "\n"))), scanner.next());
        assert_eq!(None, scanner.next());
    }
}
