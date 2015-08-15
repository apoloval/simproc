//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;

use asm::number::*;
use asm::parser::{Expr, ExprList};
use asm::symbol::*;

#[derive(Debug, PartialEq)]
pub enum DataSize { Byte }

impl fmt::Display for DataSize {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &DataSize::Byte => write!(fmt, "byte"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct PreAssembledData {
    pub size: DataSize,
    pub content: ExprList,
}

macro_rules! pdata {
    ($s:expr) => ($crate::asm::data::PreAssembledData {
        size: $s,
        content: vec![],
    });
    ($s:expr, $($c:expr),*) => ($crate::asm::data::PreAssembledData {
        size: $s,
        content: vec!($($c),*),
    });
}

#[derive(Debug, PartialEq)]
pub enum DataAssemblyError {
    Overflow { expected: DataSize, given: i64 },
    TypeMismatch { given: Expr },
    Undefined(String),
}

impl fmt::Display for DataAssemblyError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &DataAssemblyError::Overflow { ref expected, given } =>
                write!(fmt, "data overflow in {} (expected {} value)", given, expected),
            &DataAssemblyError::TypeMismatch { ref given } =>
                write!(fmt, "type mismatch in {:?}", given),
            &DataAssemblyError::Undefined(ref symbol) =>
                write!(fmt, "{} is undefined", symbol),
        }
    }
}

impl PreAssembledData {
    pub fn full_assemble(self, symbols: &SymbolTable) -> Result<RuntimeData, DataAssemblyError> {
        fn full_assemble_number(
            size: &DataSize,
            n: i64,
            data: &mut RuntimeData) -> Result<(), DataAssemblyError>
        {
            match size {
                &DataSize::Byte => {
                    let byte = try!(to_u8(n).ok_or(DataAssemblyError::Overflow {
                        expected: DataSize::Byte,
                        given: n,
                    }));
                    data.push(byte);
                    Ok(())
                },
            }
        }

        let mut result = Vec::new();
        for expr in self.content {
            match expr {
                Expr::Number(n) => try!(full_assemble_number(&self.size, n, &mut result)),
                Expr::Ident(ref id) => {
                    let n = *try!(symbols.get(id).ok_or(DataAssemblyError::Undefined(id.clone())));
                    try!(full_assemble_number(&self.size, n, &mut result));
                },
                other => return Err(DataAssemblyError::TypeMismatch { given: other }),
            }
        }
        Ok(result)
    }
}

pub type RuntimeData = Vec<u8>;

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::parser::*;
    use asm::symbol::*;

    use super::*;

    #[test]
    fn should_assemble_empty_data() {
        let symbols = SymbolTable::new();
        let data = pdata!(DataSize::Byte);
        assert_eq!(
            data.full_assemble(&symbols),
            Ok(vec![]));
    }

    #[test]
    fn should_assemble_data_from_numbers() {
        let symbols = SymbolTable::new();
        let data = pdata!(DataSize::Byte, Expr::Number(1), Expr::Number(2));
        assert_eq!(
            data.full_assemble(&symbols),
            Ok(vec![1, 2]));
    }

    #[test]
    fn should_fail_assemble_data_from_overflow_numbers() {
        let symbols = SymbolTable::new();
        let data = pdata!(DataSize::Byte, Expr::Number(1), Expr::Number(2000));
        assert_eq!(
            data.full_assemble(&symbols),
            Err(DataAssemblyError::Overflow { expected: DataSize::Byte, given: 2000 }));
    }

    #[test]
    fn should_assemble_data_from_ids() {
        let symbols = symbols!("foo" => 2);
        let data = pdata!(DataSize::Byte, Expr::Number(1), Expr::id("foo"));
        assert_eq!(
            data.full_assemble(&symbols),
            Ok(vec![1, 2]));
    }

    #[test]
    fn should_fail_assemble_data_from_overflow_id() {
        let symbols = symbols!("foo" => 2000);
        let data = pdata!(DataSize::Byte, Expr::Number(1), Expr::id("foo"));
        assert_eq!(
            data.full_assemble(&symbols),
            Err(DataAssemblyError::Overflow { expected: DataSize::Byte, given: 2000 }));
    }

    #[test]
    fn should_fail_assemble_data_from_invalid_type() {
        let symbols = symbols!("foo" => 2000);
        let data = pdata!(DataSize::Byte, Expr::Number(1), Expr::Reg(Reg::R0));
        assert_eq!(
            data.full_assemble(&symbols),
            Err(DataAssemblyError::TypeMismatch { given: Expr::Reg(Reg::R0) }));
    }
}