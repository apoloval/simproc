//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::iter::FromIterator;

use simproc::inst::*;
use simproc::mem::*;

use asm::data::*;
use asm::dir::*;
use asm::expr::*;
use asm::inst::pre::*;
use asm::lexer::Line;
use asm::parser::*;
use asm::symbol::*;

#[derive(Debug, PartialEq)]
pub enum PreAssembled {
    Empty { line: Line, base_addr: Addr },
    Inst { line: Line, base_addr: Addr, inst: PreAssembledInst },
    Data { line: Line, base_addr: Addr, data: PreAssembledData },
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreAssembleError {
    Direct(Line, DirectAssembleError),
    DuplicatedLabel(Line, String),
    Mnemo(Line, MnemoAssembleError),
}

impl fmt::Display for PreAssembleError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &PreAssembleError::Direct(ref line, ref error) =>
                write!(fmt, "in line {}: {}\n\t{}", line.row, error, line.content),
            &PreAssembleError::DuplicatedLabel(ref line, ref label) =>
                write!(fmt, "in line {}: label {} is duplicated\n\t{}",
                    line.row, label, line.content),
            &PreAssembleError::Mnemo(ref line, ref error) =>
                write!(fmt, "in line {}: {}\n\t{}", line.row, error, line.content),
        }
    }
}

pub type PreAssemblerInput = ParserOutput;
pub type PreAssemblerOutput = Result<PreAssembled, PreAssembleError>;

pub struct PreAssembler<I: Iterator<Item=PreAssemblerInput>> {
    input: I,
}

impl<I: Iterator<Item=PreAssemblerInput>> PreAssembler<I> {

    pub fn from_parser<P>(parser: P) -> Self
        where P: IntoIterator<Item=PreAssemblerInput, IntoIter=I>
    {
        PreAssembler {
            input: parser.into_iter()
        }
    }

    pub fn pre_assemble<T>(self, symbols: &mut SymbolTable) -> T
        where T: FromIterator<PreAssemblerOutput>
    {
        let mut output = Vec::with_capacity(65536);
        let mut memptr: usize = 0;
        for entry in self.input {
            match entry {
                Ok(Statement::Empty(line, lab)) => {
                    let lab_decl = Self::decl_label(line, lab, symbols, memptr);
                    let empty = |line| Ok(PreAssembled::Empty {
                        line: line,
                        base_addr: Addr(memptr as u16)
                    });
                    output.push(lab_decl.and_then(empty));
                },
                Ok(Statement::Mnemo(line, lab, mnemo, args)) => {
                    let lab_decl = Self::decl_label(line, lab, symbols, memptr);
                    let pre_asm = |line| Self::pre_assemble_inst(line, &mnemo, args, &mut memptr);
                    output.push(lab_decl.and_then(pre_asm));
                },
                Ok(Statement::Direct(line, lab, direct, args)) => {
                    let lab_decl = Self::decl_label(line, lab, symbols, memptr);
                    let pre_asm = |line| Self::pre_assemble_direct(line, &direct, args, &mut memptr);
                    output.push(lab_decl.and_then(pre_asm));
                },
                _ => {},
            }
        }
        T::from_iter(output)
    }

    /// Declare `label` if defined, returning `line` if not used to generate a
    /// `PreAssembleError::DuplicatedLabel` on error.
    fn decl_label(
        line: Line,
        label: Option<String>,
        symbols: &mut SymbolTable,
        memptr: usize) -> Result<Line, PreAssembleError>
    {
        if let Some(l) = label {
            if symbols.contains_key(&l) {
                return Err(PreAssembleError::DuplicatedLabel(line.clone(), l));
            }
            symbols.insert(l, memptr as i64);
        }
        Ok(line)
    }

    /// Pre-assemble the instruction represented by the given mnemo.
    /// It also updates `memptr` by adding the pre assembled instruction length in memory.
    pub fn pre_assemble_inst(
        line: Line,
        mnemo: &str,
        args: ExprList,
        memptr: &mut usize) -> Result<PreAssembled, PreAssembleError>
    {
        match pre_assemble_inst(mnemo, args) {
            Ok(inst) => {
                let base = Addr(*memptr as u16);
                *memptr += inst.len();
                Ok(PreAssembled::Inst { line: line, base_addr: base, inst: inst })
            },
            Err(e) => Err(PreAssembleError::Mnemo(line, e)),
        }
    }

    /// Pre-assemble the directive represented by the given mnemo.
    /// It also updates `memptr` according to what the directive represents.
    pub fn pre_assemble_direct(
        line: Line,
        direct: &str,
        args: ExprList,
        memptr: &mut usize) -> Result<PreAssembled, PreAssembleError>
    {
        match pre_assemble_direct(direct, args) {
            Ok(Direct::Org(Addr(addr))) => {
                *memptr = addr as usize;
                Ok(PreAssembled::Empty { line: line, base_addr: Addr(addr), })
            },
            Ok(Direct::Db(args)) => {
                let base = Addr(*memptr as u16);
                *memptr += args.len();
                Ok(PreAssembled::Data {
                    line: line,
                    base_addr: base,
                    data: PreAssembledData { size: DataSize::Byte, content: args },
                })
            },
            Err(e) => Err(PreAssembleError::Direct(line, e)),
        }
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;
    use simproc::mem::*;

    use asm::data::*;
    use asm::expr::*;
    use asm::parser::*;
    use asm::symbol::*;

    use super::*;

    #[test]
    fn should_pre_assemble_empty_prog() {
        let lines = vec![];
        let mut symbols = SymbolTable::new();
        let pre = PreAssembler::from_parser(lines);
        let result: Vec<PreAssemblerOutput> = pre.pre_assemble(&mut symbols);
        assert!(result.is_empty());
    }

    #[test]
    fn should_pre_assemble_declaring_inst_labels() {
        let lines = vec![
            Ok(Statement::Mnemo(
                sline!(1, "lab1: nop"),
                Some("lab1".to_string()),
                "nop".to_string(),
                vec![])),
            Ok(Statement::Mnemo(
                sline!(2, "lab2: nop"),
                Some("lab2".to_string()),
                "nop".to_string(),
                vec![])),
        ];
        let mut symbols = SymbolTable::new();
        let pre = PreAssembler::from_parser(lines);
        let _: Vec<PreAssemblerOutput> = pre.pre_assemble(&mut symbols);
        assert_eq!(symbols["lab1"], 0);
        assert_eq!(symbols["lab2"], 1);
    }

    #[test]
    fn should_pre_assemble_dup_inst_label_as_error() {
        let lines = vec![
            Ok(Statement::Mnemo(
                sline!(1, "lab1: nop"),
                Some("lab1".to_string()),
                "nop".to_string(),
                vec![])),
            Ok(Statement::Mnemo(
                sline!(2, "lab1: nop"),
                Some("lab1".to_string()),
                "nop".to_string(),
                vec![])),
        ];
        let mut symbols = SymbolTable::new();
        let pre = PreAssembler::from_parser(lines);
        let result: Vec<PreAssemblerOutput> = pre.pre_assemble(&mut symbols);
        assert_eq!(symbols["lab1"], 0);
        assert!(!symbols.contains_key("lab2"));
        assert_eq!(
            result[1],
            Err(PreAssembleError::DuplicatedLabel(sline!(2, "lab1: nop"), "lab1".to_string())));
    }

    #[test]
    fn should_pre_assemble_program() {
        let lines = vec![
            Ok(Statement::Empty(sline!(1, ""), None)),
            Ok(Statement::Mnemo(
                sline!(2, "halt"),
                None,
                "halt".to_string(),
                vec![])),
            Ok(Statement::Direct(
                sline!(3, ".org 0x1000"),
                None,
                "org".to_string(),
                vec![Expr::Number(0x1000)])),
            Ok(Statement::Direct(
                sline!(4, ".db 1, foobar"),
                None,
                "db".to_string(),
                vec![Expr::Number(1), Expr::id("foobar")])),
        ];
        let mut symbols = SymbolTable::new();
        let pre = PreAssembler::from_parser(lines);
        let result: Vec<PreAssemblerOutput> = pre.pre_assemble(&mut symbols);
        assert_eq!(
            result[0],
            Ok(PreAssembled::Empty {
                line: sline!(1, ""),
                base_addr: Addr(0),
            }));
        assert_eq!(
            result[1],
            Ok(PreAssembled::Inst {
                line: sline!(2, "halt"),
                base_addr: Addr(0),
                inst: Inst::Halt
            }));
        assert_eq!(
            result[2],
            Ok(PreAssembled::Empty {
                line: sline!(3, ".org 0x1000"),
                base_addr: Addr(0x1000),
            }));
        assert_eq!(
            result[3],
            Ok(PreAssembled::Data {
                line: sline!(4, ".db 1, foobar"),
                base_addr: Addr(0x1000),
                data: PreAssembledData {
                    size: DataSize::Byte,
                    content: vec![Expr::Number(1), Expr::id("foobar")]
                },
            }));
    }
}