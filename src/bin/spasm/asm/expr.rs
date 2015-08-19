//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::ops::Range;

use simproc::cpu::*;
use simproc::inst::*;
use simproc::mem::*;

use asm::symbol::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Number(i64),
    Reg(Reg),
    AddrReg(AddrReg),
    Ident(String),
}

impl Expr {
    #[cfg(test)]
    pub fn id(s: &str) -> Self { Expr::Ident(s.to_string()) }
}

pub type ExprList = Vec<Expr>;

#[derive(Clone, Debug, PartialEq)]
pub enum ExprAssembleError {
    TypeMismatch { expected: String, given: Expr },
    Undefined { symbol: String },
    TooFar { from: Addr, to: Addr },
    OutOfRange { range: Range<i64>, given: i64 },
}

impl fmt::Display for ExprAssembleError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &ExprAssembleError::TypeMismatch { ref expected, ref given } =>
                write!(fmt, "type mismatch in {:?} ({} expected)", given, expected),
            &ExprAssembleError::Undefined { ref symbol } =>
                write!(fmt, "'{}' is undefined", symbol),
            &ExprAssembleError::TooFar { from, to } =>
                write!(fmt, "'0x{:x}' is too far from base address 0x{:x}", to, from),
            &ExprAssembleError::OutOfRange { ref range, given } =>
                write!(fmt, "'{}' is out of expected range [{}, {}]",
                    given, range.start, range.end),
        }
    }
}

pub trait ExprAssembler {
    fn to_reg(&mut self, e: Expr) -> Result<Reg, ExprAssembleError>;
    fn to_areg(&mut self, e: Expr) -> Result<AddrReg, ExprAssembleError>;
    fn to_immediate(&mut self, e: Expr) -> Result<Immediate, ExprAssembleError>;
    fn to_addr(&mut self, e: Expr) -> Result<Addr, ExprAssembleError>;
    fn to_raddr(&mut self, e: Expr, base: Addr) -> Result<RelAddr, ExprAssembleError>;
}

pub struct StdExprAssembler<'a> {
    symbols: &'a SymbolTable,
}

impl<'a> StdExprAssembler<'a> {
    pub fn from_symbols(symbols: &'a SymbolTable) -> Self {
        StdExprAssembler { symbols: symbols }
    }
}

impl<'a> ExprAssembler for StdExprAssembler<'a> {

    fn to_reg(&mut self, e: Expr) -> Result<Reg, ExprAssembleError> {
        match e {
            Expr::Reg(reg) => Ok(reg),
            other => Err(ExprAssembleError::TypeMismatch {
                expected: "register name".to_string(),
                given: other,
            })
        }
    }

    fn to_areg(&mut self, e: Expr) -> Result<AddrReg, ExprAssembleError> {
        match e {
            Expr::AddrReg(reg) => Ok(reg),
            other => Err(ExprAssembleError::TypeMismatch {
                expected: "address register name".to_string(),
                given: other,
            })
        }
    }

    fn to_immediate(&mut self, e: Expr) -> Result<Immediate, ExprAssembleError> {
        fn validate(n: i64) -> Result<Immediate, ExprAssembleError> {
            Immediate::from_i64(n).ok_or(ExprAssembleError::OutOfRange {
                range: Immediate::range(),
                given: n,
            })
        }

        match e {
            Expr::Number(n) => validate(n),
            Expr::Ident(id) => {
                match self.symbols.get(&id) {
                    Some(n) => validate(*n),
                    _ => Err(ExprAssembleError::Undefined { symbol: id }),
                }
            },
            other => Err(ExprAssembleError::TypeMismatch {
                expected: "immediate value".to_string(),
                given: other,
            })
        }
    }

    fn to_addr(&mut self, e: Expr) -> Result<Addr, ExprAssembleError> {
        fn validate(dest: i64) -> Result<Addr, ExprAssembleError> {
            addr_from_i64(dest).ok_or(ExprAssembleError::OutOfRange {
                range: ADDR_RANGE,
                given: dest,
            })
        }

        match e {
            Expr::Number(n) => validate(n),
            Expr::Ident(id) => {
                match self.symbols.get(&id) {
                    Some(n) => validate(*n),
                    _ => Err(ExprAssembleError::Undefined { symbol: id }),
                }
            },
            other => Err(ExprAssembleError::TypeMismatch {
                expected: "memory address".to_string(),
                given: other,
            })
        }
    }

    fn to_raddr(&mut self, e: Expr, base: Addr) -> Result<RelAddr, ExprAssembleError> {
        fn validate(from: Addr, dest: i64) -> Result<RelAddr, ExprAssembleError> {
            match addr_from_i64(dest) {
                Some(to) => {
                    addr_dist(from, to).ok_or(ExprAssembleError::TooFar { from: from, to: to, })
                },
                None => Err(ExprAssembleError::OutOfRange {
                    range: ADDR_RANGE,
                    given: dest,
                }),
            }
        }

        match e {
            Expr::Number(n) => validate(base, n),
            Expr::Ident(id) => {
                match self.symbols.get(&id) {
                    Some(n) => validate(base, *n),
                    _ => Err(ExprAssembleError::Undefined { symbol: id }),
                }
            },
            other => Err(ExprAssembleError::TypeMismatch {
                expected: "memory address".to_string(),
                given: other,
            })
        }
    }
}

#[cfg(test)]
mod test {

    use simproc::cpu::*;
    use simproc::inst::*;
    use simproc::mem::*;

    use asm::symbol::*;

    use super::*;

    #[test]
    fn should_asm_expr_to_reg() {
        let symbols = SymbolTable::new();
        let mut asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_reg(Expr::Reg(Reg::R0)),
            Ok(Reg::R0));
        assert_eq!(
            asm.to_reg(Expr::Number(100)),
            Err(ExprAssembleError::TypeMismatch {
                expected: "register name".to_string(),
                given: Expr::Number(100),
            }));
    }

    #[test]
    fn should_asm_expr_to_areg() {
        let symbols = SymbolTable::new();
        let mut asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_areg(Expr::AddrReg(AddrReg::A0)),
            Ok(AddrReg::A0));
        assert_eq!(
            asm.to_areg(Expr::Number(100)),
            Err(ExprAssembleError::TypeMismatch {
                expected: "address register name".to_string(),
                given: Expr::Number(100),
            }));
    }

    #[test]
    fn should_asm_expr_to_immediate() {
        let mut symbols = SymbolTable::new();
        symbols.insert("foobar".to_string(), 100);
        symbols.insert("toobig".to_string(), 1000);
        let mut asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_immediate(Expr::Number(100)),
            Ok(Immediate(100)));
        assert_eq!(
            asm.to_immediate(Expr::id("foobar")),
            Ok(Immediate(100)));
        assert_eq!(
            asm.to_immediate(Expr::Number(1000)),
            Err(ExprAssembleError::OutOfRange {
                range: Immediate::range(),
                given: 1000,
            }));
        assert_eq!(
            asm.to_immediate(Expr::id("toobig")),
            Err(ExprAssembleError::OutOfRange {
                range: Immediate::range(),
                given: 1000,
            }));
        assert_eq!(
            asm.to_immediate(Expr::id("undefined")),
            Err(ExprAssembleError::Undefined {
                symbol: "undefined".to_string()
            }));
        assert_eq!(
            asm.to_immediate(Expr::Reg(Reg::R0)),
            Err(ExprAssembleError::TypeMismatch {
                expected: "immediate value".to_string(),
                given: Expr::Reg(Reg::R0),
            }));
    }

    #[test]
    fn should_asm_expr_to_addr() {
        let mut symbols = SymbolTable::new();
        symbols.insert("foobar".to_string(), 0x1000);
        symbols.insert("toobig".to_string(), 0x100000);
        let mut asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_addr(Expr::Number(100)),
            Ok(100));
        assert_eq!(
            asm.to_addr(Expr::id("foobar")),
            Ok(0x1000));
        assert_eq!(
            asm.to_addr(Expr::Number(-100)),
            Err(ExprAssembleError::OutOfRange { range: ADDR_RANGE, given: -100, }));
        assert_eq!(
            asm.to_addr(Expr::id("toobig")),
            Err(ExprAssembleError::OutOfRange { range: ADDR_RANGE, given: 0x100000, }));
        assert_eq!(
            asm.to_addr(Expr::id("undefined")),
            Err(ExprAssembleError::Undefined { symbol: "undefined".to_string() }));
        assert_eq!(
            asm.to_addr(Expr::Reg(Reg::R0)),
            Err(ExprAssembleError::TypeMismatch {
                expected: "memory address".to_string(),
                given: Expr::Reg(Reg::R0),
            }));
    }

    #[test]
    fn should_asm_expr_to_raddr() {
        let mut symbols = SymbolTable::new();
        symbols.insert("foobar".to_string(), 0x1000);
        let mut asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_raddr(Expr::Number(100), 75),
            Ok(25));
        assert_eq!(
            asm.to_raddr(Expr::id("foobar"), 0xf00),
            Ok(0x100));
        assert_eq!(
            asm.to_raddr(Expr::Number(-10), 0),
            Err(ExprAssembleError::OutOfRange {
                range: ADDR_RANGE,
                given: -10,
            }));
        assert_eq!(
            asm.to_raddr(Expr::Number(1024), 2048),
            Err(ExprAssembleError::TooFar { from: 2048, to: 1024, }));
        assert_eq!(
            asm.to_raddr(Expr::id("foobar"), 0x2000),
            Err(ExprAssembleError::TooFar { from: 0x2000, to: 0x1000, }));
        assert_eq!(
            asm.to_raddr(Expr::id("undefined"), 75),
            Err(ExprAssembleError::Undefined {
                symbol: "undefined".to_string()
            }));
        assert_eq!(
            asm.to_raddr(Expr::Reg(Reg::R0), 75),
            Err(ExprAssembleError::TypeMismatch {
                expected: "memory address".to_string(),
                given: Expr::Reg(Reg::R0),
            }));
    }
}
