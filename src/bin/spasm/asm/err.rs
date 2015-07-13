//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::convert::From;
use std::fmt;
use std::io;

#[derive(Debug, PartialEq)]
pub enum Error {
    BadAddrReg(String),
    BadLexic,
    BadNumber(String),
    BadOperandsCount(usize),
    BadReg(String),
    NoSuchSymbol(String),
    OutOfRange(String),
    UnknownDirective(String),
    UnknownMnemo(String),
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Error::BadAddrReg(ref expr) =>
                write!(fmt, "invalid address register `{}`", expr),
            &Error::BadLexic =>
                write!(fmt, "invalid lexical expression"),
            &Error::BadNumber(ref expr) =>
                write!(fmt, "invalid numeric expression in `{}`", expr),
            &Error::BadOperandsCount(expected) =>
                write!(fmt, "{} operand(s) expected", expected),
            &Error::BadReg(ref expr) =>
                write!(fmt, "invalid register `{}`", expr),
            &Error::NoSuchSymbol(ref expr) =>
                write!(fmt, "undeclared symbol `{}`", expr),
            &Error::OutOfRange(ref expr) =>
                write!(fmt, "number `{}` is out of range", expr),
            &Error::UnknownDirective(ref dir) =>
                write!(fmt, "unknown directive `.{}`", dir),
            &Error::UnknownMnemo(ref mnemo) =>
                write!(fmt, "unknown mnemonic `{}`", mnemo),
        }
    }
}

#[derive(Debug)]
pub struct ProgramError {
    pub line: usize,
    pub content: String,
    pub reason: Error,
}

impl ProgramError {
    pub fn new(line: usize, content: &str, reason: Error) -> ProgramError {
        ProgramError {
            line: line,
            content: content.to_string(),
            reason: reason,
        }
    }

    pub fn new_lexical_error(line: usize, content: &str) -> ProgramError {
        ProgramError::new(line, content, Error::BadLexic)
    }
}

impl fmt::Display for ProgramError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "in line {}, `{}`: {}", self.line, self.content, self.reason)
    }
}

#[derive(Debug)]
pub enum AssemblyError {
    Io(io::Error),
    BadProgram(Vec<ProgramError>)
}

impl From<io::Error> for AssemblyError {
    fn from(err: io::Error) -> AssemblyError { AssemblyError::Io(err) }
}
