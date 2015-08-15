//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::fmt;

use simproc::inst::*;

use asm::expr::*;

#[derive(Debug, PartialEq)]
pub enum Direct {
    Org(Addr),
    Db(ExprList),
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectAssembleError {
    BadArgumentCount { expected: usize, given: usize },
    InvalidAddress(i64),
    TypeMismatch { expected: String },
    UnknownDirect(String),
}

impl fmt::Display for DirectAssembleError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &DirectAssembleError::BadArgumentCount { expected, given } =>
                write!(fmt, "expected {} arguments, {} given", expected, given),
            &DirectAssembleError::InvalidAddress(addr) =>
                write!(fmt, "value {} is not a valid address", addr),
            &DirectAssembleError::TypeMismatch { ref expected } =>
                write!(fmt, "type mismatch ({} expected)", expected),
            &DirectAssembleError::UnknownDirect(ref direct) =>
                write!(fmt, "unknown directive .{}", direct),
        }
    }
}

pub fn pre_assemble_direct(
    direct: &str,
    args: ExprList) -> Result<Direct, DirectAssembleError>
{
    match direct.to_ascii_lowercase().trim() {
        "org" => pre_assemble_org(args),
        "db" => Ok(Direct::Db(args)),
        _ => Err(DirectAssembleError::UnknownDirect(direct.to_string())),
    }
}

fn pre_assemble_org(args: ExprList) -> Result<Direct, DirectAssembleError> {
    let argc = args.len();
    if argc != 1 {
        return Err(DirectAssembleError::BadArgumentCount {
            expected: 1,
            given: argc,
        });
    }
    match args[0] {
        Expr::Number(n) => {
            Addr::from_i64(n)
                .ok_or(DirectAssembleError::InvalidAddress(n))
                .and_then(|addr| Ok(Direct::Org(addr)))
        },
        _ => Err(DirectAssembleError::TypeMismatch {
            expected: "literal memory address".to_string()
        }),
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::expr::*;

    use super::*;

    #[test]
    fn should_preassemble_org() {
        assert_eq!(
            pre_assemble_direct("org", vec![Expr::Number(0x1000)]),
            Ok(Direct::Org(Addr(0x1000))));
        assert_eq!(
            pre_assemble_direct("org", vec![Expr::Number(0x100000)]),
            Err(DirectAssembleError::InvalidAddress(0x100000)));
        assert_eq!(
            pre_assemble_direct("org", vec![Expr::id("foobar")]),
            Err(DirectAssembleError::TypeMismatch {
                expected: "literal memory address".to_string(),
            }));
    }

    #[test]
    fn should_preassemble_db() {
        assert_eq!(
            pre_assemble_direct("db", vec![]),
            Ok(Direct::Db(vec![])));
        assert_eq!(
            pre_assemble_direct("db", vec![Expr::Number(1)]),
            Ok(Direct::Db(vec![Expr::Number(1)])));
        assert_eq!(
            pre_assemble_direct("db", vec![Expr::Number(1), Expr::id("foobar")]),
            Ok(Direct::Db(vec![Expr::Number(1), Expr::id("foobar")])));
    }

    #[test]
    fn should_preassemble_unknown_direct() {
        assert_eq!(
            pre_assemble_direct("foobar", vec![]),
            Err(DirectAssembleError::UnknownDirect("foobar".to_string())));
    }
}
