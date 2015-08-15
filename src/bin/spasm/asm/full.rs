//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;

use simproc::inst::*;

use asm::data::*;
use asm::inst::full::*;
use asm::lexer::*;
use asm::expr::*;
use asm::pre::*;
use asm::symbol::*;

#[derive(Debug, PartialEq)]
pub enum FullAssembled {
    Empty { line: Line, base_addr: Addr },
    Inst { line: Line, base_addr: Addr, inst: RuntimeInst },
    Data { line: Line, base_addr: Addr, data: RuntimeData },
}

#[derive(Debug, PartialEq)]
pub enum FullAssembleError {
    Data { line: Line, error: DataAssemblyError },
    Pre(PreAssembleError),
    Expr { line: Line, error: ExprAssembleError },
}

impl fmt::Display for FullAssembleError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &FullAssembleError::Data { ref line, ref error } =>
                write!(fmt, "in line {}: {}\n\t{}", line.row, error, line.content),
            &FullAssembleError::Pre(ref error) => write!(fmt, "{}", error),
            &FullAssembleError::Expr { ref line, ref error } =>
                write!(fmt, "in line {}: {}\n\t{}", line.row, error, line.content),
        }
    }
}

pub type FullAssemblerInput = PreAssemblerOutput;
pub type FullAssemblerOutput = Result<FullAssembled, FullAssembleError>;

pub struct FullAssembler<'a, I: Iterator<Item=FullAssemblerInput>> {
    input: I,
    inst_asm: StdInstAssembler<'a>,
    symbols: &'a SymbolTable,
}

impl<'a, I: Iterator<Item=FullAssemblerInput>> FullAssembler<'a, I> {

    pub fn from<P>(pre: P, symbols: &'a SymbolTable) -> Self
        where P: IntoIterator<Item=FullAssemblerInput, IntoIter=I>
    {
        FullAssembler {
            input: pre.into_iter(),
            inst_asm: StdInstAssembler::from_expr_asm(
                StdExprAssembler::from_symbols(symbols)),
            symbols: symbols,
        }
    }
}

impl<'a, I: Iterator<Item=FullAssemblerInput>> Iterator for FullAssembler<'a, I> {

    type Item = FullAssemblerOutput;

    fn next(&mut self) -> Option<FullAssemblerOutput> {
        match self.input.next() {
            Some(Ok(PreAssembled::Empty { line, base_addr })) => {
                Some(Ok(FullAssembled::Empty {
                    line: line, base_addr: base_addr,
                }))
            },
            Some(Ok(PreAssembled::Inst { line, base_addr, inst })) => {
                Some(match self.inst_asm.assemble(inst, base_addr) {
                    Ok(inst) => Ok(FullAssembled::Inst {
                        line: line, base_addr: base_addr, inst: inst
                    }),
                    Err(e) => Err(FullAssembleError::Expr { line: line, error: e }),
                })
            },
            Some(Ok(PreAssembled::Data { line, base_addr, data })) => {
                match data.full_assemble(self.symbols) {
                    Ok(rtdata) =>
                        Some(Ok(FullAssembled::Data {
                            line: line, base_addr: base_addr, data: rtdata
                        })),
                    Err(e) =>
                        Some(Err(FullAssembleError::Data {
                            line: line, error: e,
                        })),
                }
            },
            Some(Err(e)) => Some(Err(FullAssembleError::Pre(e))),
            None => None,
        }
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::data::*;
    use asm::expr::*;
    use asm::pre::*;
    use asm::symbol::*;

    use super::*;

    #[test]
    fn should_assemble_empty_program() {
        let input = Vec::new();
        let symbols = SymbolTable::new();
        let mut full = FullAssembler::from(input, &symbols);
        assert_eq!(full.next(), None);
    }

    #[test]
    fn should_assemble_non_empty() {
        let input = vec![
            Ok(PreAssembled::Empty {
                line: sline!(1, ""),
                base_addr: Addr(0x100),
            }),
            Ok(PreAssembled::Inst {
                line: sline!(2, "nop"),
                base_addr: Addr(0x100),
                inst: Inst::Nop,
            }),
            Ok(PreAssembled::Data {
                line: sline!(3, ".db 1, 2"),
                base_addr: Addr(0x200),
                data: pdata!(DataSize::Byte, Expr::Number(1), Expr::Number(2)),
            }),
            Err(PreAssembleError::DuplicatedLabel(sline!(4, "foobar"), "foobar".to_string())),
        ];
        let symbols = SymbolTable::new();
        let mut full = FullAssembler::from(input, &symbols);
        assert_eq!(full.next(), Some(Ok(FullAssembled::Empty {
            line: sline!(1, ""),
            base_addr: Addr(0x100),
        })));
        assert_eq!(full.next(), Some(Ok(FullAssembled::Inst {
            line: sline!(2, "nop"),
            base_addr: Addr(0x100),
            inst: Inst::Nop,
        })));
        assert_eq!(full.next(), Some(Ok(FullAssembled::Data {
            line: sline!(3, ".db 1, 2"),
            base_addr: Addr(0x200),
            data: vec![1, 2],
        })));
        assert_eq!(full.next(), Some(Err(FullAssembleError::Pre(
            PreAssembleError::DuplicatedLabel(sline!(4, "foobar"), "foobar".to_string())))));
    }
}
