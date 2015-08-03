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

use asm::lexer::TextLoc;
use asm::new_parser::{Expr, ExprList, SyntaxError};

#[derive(Debug, PartialEq)]
pub enum Error {
    BadAddrReg(TextLoc, Expr),
    BadNumber(TextLoc, Expr),
    BadOperandsCount(TextLoc, ExprList, usize),
    BadReg(TextLoc, Expr),
    NoSuchSymbol(TextLoc, String),
    OutOfRange(TextLoc, Expr),
    Syntax(SyntaxError),
    UnknownDirective(TextLoc, String),
    UnknownMnemo(TextLoc, String),
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Error::BadAddrReg(ref loc, ref expr) =>
                write!(fmt, "{}: invalid address register `{}`", loc, expr),
            &Error::BadNumber(ref loc, ref expr) =>
                write!(fmt, "{}: invalid numeric expression in `{}`", loc, expr),
            &Error::BadOperandsCount(ref loc, _, expected) =>
                write!(fmt, "{}: {} operand(s) expected", loc, expected),
            &Error::BadReg(ref loc, ref expr) =>
                write!(fmt, "{}: invalid register `{}`", loc, expr),
            &Error::NoSuchSymbol(ref loc, ref expr) =>
                write!(fmt, "{}: undeclared symbol `{}`", loc, expr),
            &Error::OutOfRange(ref loc, ref expr) =>
                write!(fmt, "{}: number `{}` is out of range", loc, expr),
            &Error::Syntax(ref e) =>
                write!(fmt, "syntax error ({})", e),
            &Error::UnknownDirective(ref loc, ref dir) =>
                write!(fmt, "{}: unknown directive `.{}`", loc, dir),
            &Error::UnknownMnemo(ref loc, ref mnemo) =>
                write!(fmt, "{}: unknown mnemonic `{}`", loc, mnemo),
        }
    }
}
