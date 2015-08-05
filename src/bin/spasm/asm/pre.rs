//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::collections::HashMap;
use std::iter::FromIterator;

use asm::lexer::TextLoc;
use asm::new_parser::*;

use simproc::inst::*;

pub type SymbolTable = HashMap<String, i64>;

#[derive(Debug, PartialEq)]
pub struct PreAssembledOperands;

impl Operands for PreAssembledOperands {
    type Immediate = Expr;
    type Addr = Expr;
    type RelAddr = Expr;
    type Reg = Expr;
    type AddrReg = Expr;
}

pub type PreAssembledInst = Inst<PreAssembledOperands>;

#[derive(Debug, PartialEq)]
pub enum PreAssembled {
    Inst { loc: TextLoc, base_addr: Addr, inst: PreAssembledInst },
}

#[derive(Debug, PartialEq)]
pub enum PreAssembleError {
    BadArgumentCount(usize, usize),
    DuplicatedLabel(TextLoc, String),
    UnknownMnemo(String),
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
                Ok(Statement::Mnemo(loc, lab, mnemo, args)) => {
                    let lab_decl = Self::decl_label(loc, lab, symbols, memptr);
                    let pre_asm = |loc| Self::pre_assemble_inst(loc, &mnemo, &args, &mut memptr);
                    output.push(lab_decl.and_then(pre_asm));
                },
                _ => {},
            }
        }
        T::from_iter(output)
    }

    /// Declare `label` if defined, returning `loc` if not used to generate a
    /// `PreAssembleError::DuplicatedLabel` on error.
    fn decl_label(
        loc: TextLoc,
        label: Option<String>,
        symbols: &mut SymbolTable,
        memptr: usize) -> Result<TextLoc, PreAssembleError>
    {
        if let Some(l) = label {
            if symbols.contains_key(&l) {
                return Err(PreAssembleError::DuplicatedLabel(loc.clone(), l));
            }
            symbols.insert(l, memptr as i64);
        }
        Ok(loc)
    }

    /// Pre-assemble the instruction represented by the given mnemo.
    /// It also updates `memptr` by adding the pre assembled instruction length in memory.
    pub fn pre_assemble_inst(
        loc: TextLoc,
        mnemo: &str,
        args: &ExprList,
        memptr: &mut usize) -> Result<PreAssembled, PreAssembleError>
    {
        match pre_assemble_inst(&mnemo, &args) {
            Ok(inst) => {
                let base = Addr(*memptr as u16);
                *memptr += inst.len();
                Ok(PreAssembled::Inst { loc: loc, base_addr: base, inst: inst })
            },
            Err(e) => Err(e),
        }
    }
}

pub fn pre_assemble_inst(
    mnemo: &str,
    args: &ExprList) -> Result<PreAssembledInst, PreAssembleError>
{
    match mnemo.to_ascii_lowercase().trim() {
        "add" => pre_assemble_binary(args, Inst::Add),
        "adc" => pre_assemble_binary(args, Inst::Adc),
        "addi" => pre_assemble_binary(args, Inst::Addi),
        "sub" => pre_assemble_binary(args, Inst::Sub),
        "sbc" => pre_assemble_binary(args, Inst::Sbc),
        "subi" => pre_assemble_binary(args, Inst::Subi),
        "mulw" => pre_assemble_binary(args, Inst::Mulw),
        "and" => pre_assemble_binary(args, Inst::And),
        "or" => pre_assemble_binary(args, Inst::Or),
        "xor" => pre_assemble_binary(args, Inst::Xor),
        "lsl" => pre_assemble_binary(args, Inst::Lsl),
        "lsr" => pre_assemble_binary(args, Inst::Lsr),
        "asr" => pre_assemble_binary(args, Inst::Asr),
        "not" => pre_assemble_unary(args, Inst::Not),
        "comp" => pre_assemble_unary(args, Inst::Comp),
        "inc" => pre_assemble_unary(args, Inst::Inc),
        "incw" => pre_assemble_unary(args, Inst::Incw),
        "dec" => pre_assemble_unary(args, Inst::Dec),
        "decw" => pre_assemble_unary(args, Inst::Decw),
        "mov" => pre_assemble_binary(args, Inst::Mov),
        "ld" => pre_assemble_binary(args, Inst::Ld),
        "st" => pre_assemble_binary(args, Inst::St),
        "ldd" => pre_assemble_binary(args, Inst::Ldd),
        "std" => pre_assemble_binary(args, Inst::Std),
        "ldi" => pre_assemble_binary(args, Inst::Ldi),
        "ldsp" => pre_assemble_unary(args, Inst::Ldsp),
        "push" => pre_assemble_unary(args, Inst::Push),
        "pop" => pre_assemble_unary(args, Inst::Pop),
        "je" => pre_assemble_unary(args, Inst::Je),
        "jne" => pre_assemble_unary(args, Inst::Jne),
        "jl" => pre_assemble_unary(args, Inst::Jl),
        "jge" => pre_assemble_unary(args, Inst::Jge),
        "jcc" => pre_assemble_unary(args, Inst::Jcc),
        "jcs" => pre_assemble_unary(args, Inst::Jcs),
        "jvc" => pre_assemble_unary(args, Inst::Jvc),
        "jvs" => pre_assemble_unary(args, Inst::Jvs),
        "jmp" => pre_assemble_unary(args, Inst::Jmp),
        "rjmp" => pre_assemble_unary(args, Inst::Rjmp),
        "ijmp" => pre_assemble_unary(args, Inst::Ijmp),
        "call" => pre_assemble_unary(args, Inst::Call),
        "rcall" => pre_assemble_unary(args, Inst::Rcall),
        "icall" => pre_assemble_unary(args, Inst::Icall),
        "ret" => pre_assemble_nullary(args, Inst::Ret),
        "reti" => pre_assemble_nullary(args, Inst::Reti),
        "nop" => pre_assemble_nullary(args, Inst::Nop),
        "halt" => pre_assemble_nullary(args, Inst::Halt),
        _ => Err(PreAssembleError::UnknownMnemo(mnemo.to_string()))
    }
}

fn pre_assemble_nullary(args: &ExprList, inst: PreAssembledInst) -> Result<PreAssembledInst, PreAssembleError> {
    if args.len() != 0 { Err(PreAssembleError::BadArgumentCount(0, args.len())) }
    else { Ok(inst) }
}

fn pre_assemble_unary<F>(args: &ExprList, inst: F) -> Result<PreAssembledInst, PreAssembleError>
    where F: FnOnce(Expr) -> PreAssembledInst
{
    if args.len() != 1 { Err(PreAssembleError::BadArgumentCount(1, args.len())) }
    else { Ok(inst(args[0].clone())) }
}

fn pre_assemble_binary<F>(args: &ExprList, inst: F) -> Result<PreAssembledInst, PreAssembleError>
    where F: FnOnce(Expr, Expr) -> PreAssembledInst
{
    if args.len() != 2 { Err(PreAssembleError::BadArgumentCount(2, args.len())) }
    else { Ok(inst(args[0].clone(), args[1].clone())) }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::lexer::*;
    use asm::new_parser::*;

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
                loc!(1, 1, "lab1: nop"),
                Some("lab1".to_string()),
                "nop".to_string(),
                exprlist![])),
            Ok(Statement::Mnemo(
                loc!(2, 1, "lab2: nop"),
                Some("lab2".to_string()),
                "nop".to_string(),
                exprlist![])),
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
                loc!(1, 1, "lab1: nop"),
                Some("lab1".to_string()),
                "nop".to_string(),
                exprlist![])),
            Ok(Statement::Mnemo(
                loc!(2, 1, "lab1: nop"),
                Some("lab1".to_string()),
                "nop".to_string(),
                exprlist![])),
        ];
        let mut symbols = SymbolTable::new();
        let pre = PreAssembler::from_parser(lines);
        let result: Vec<PreAssemblerOutput> = pre.pre_assemble(&mut symbols);
        assert_eq!(symbols["lab1"], 0);
        assert!(!symbols.contains_key("lab2"));
        assert_eq!(
            result[1],
            Err(PreAssembleError::DuplicatedLabel(loc!(2, 1, "lab1: nop"), "lab1".to_string())));
    }

    #[test]
    fn should_pre_assemble_insts() {
        let lines = vec![
            Ok(Statement::Mnemo(
                loc!(1, 1, "nop"),
                None,
                "nop".to_string(),
                exprlist![])),
            Ok(Statement::Mnemo(
                loc!(2, 1, "halt"),
                None,
                "halt".to_string(),
                exprlist![])),
        ];
        let mut symbols = SymbolTable::new();
        let pre = PreAssembler::from_parser(lines);
        let result: Vec<PreAssemblerOutput> = pre.pre_assemble(&mut symbols);
        assert_eq!(
            result[0],
            Ok(PreAssembled::Inst {
                loc: loc!(1, 1, "nop"),
                base_addr: Addr(0),
                inst: Inst::Nop
            }));
        assert_eq!(
            result[1],
            Ok(PreAssembled::Inst {
                loc: loc!(2, 1, "halt"),
                base_addr: Addr(1),
                inst: Inst::Halt
            }));
    }

    #[test]
    fn should_pre_assemble_add() { should_pre_assemble_binary_inst("add", Inst::Add) }

    #[test]
    fn should_pre_assemble_adc() { should_pre_assemble_binary_inst("adc", Inst::Adc) }

    #[test]
    fn should_pre_assemble_addi() { should_pre_assemble_binary_inst("addi", Inst::Addi) }

    #[test]
    fn should_pre_assemble_sub() { should_pre_assemble_binary_inst("sub", Inst::Sub) }

    #[test]
    fn should_pre_assemble_sbc() { should_pre_assemble_binary_inst("sbc", Inst::Sbc) }

    #[test]
    fn should_pre_assemble_subi() { should_pre_assemble_binary_inst("subi", Inst::Subi) }

    #[test]
    fn should_pre_assemble_mulw() { should_pre_assemble_binary_inst("mulw", Inst::Mulw) }

    #[test]
    fn should_pre_assemble_and() { should_pre_assemble_binary_inst("and", Inst::And) }

    #[test]
    fn should_pre_assemble_or() { should_pre_assemble_binary_inst("or", Inst::Or) }

    #[test]
    fn should_pre_assemble_xor() { should_pre_assemble_binary_inst("xor", Inst::Xor) }

    #[test]
    fn should_pre_assemble_lsl() { should_pre_assemble_binary_inst("lsl", Inst::Lsl) }

    #[test]
    fn should_pre_assemble_lsr() { should_pre_assemble_binary_inst("lsr", Inst::Lsr) }

    #[test]
    fn should_pre_assemble_asr() { should_pre_assemble_binary_inst("asr", Inst::Asr) }

    #[test]
    fn should_pre_assemble_not() { should_pre_assemble_unary_inst("not", Inst::Not) }

    #[test]
    fn should_pre_assemble_comp() { should_pre_assemble_unary_inst("comp", Inst::Comp) }

    #[test]
    fn should_pre_assemble_inc() { should_pre_assemble_unary_inst("inc", Inst::Inc) }

    #[test]
    fn should_pre_assemble_incw() { should_pre_assemble_unary_inst("incw", Inst::Incw) }

    #[test]
    fn should_pre_assemble_dec() { should_pre_assemble_unary_inst("dec", Inst::Dec) }

    #[test]
    fn should_pre_assemble_decw() { should_pre_assemble_unary_inst("decw", Inst::Decw) }

    #[test]
    fn should_pre_assemble_mov() { should_pre_assemble_binary_inst("mov", Inst::Mov) }

    #[test]
    fn should_pre_assemble_ld() { should_pre_assemble_binary_inst("ld", Inst::Ld) }

    #[test]
    fn should_pre_assemble_st() { should_pre_assemble_binary_inst("st", Inst::St) }

    #[test]
    fn should_pre_assemble_ldd() { should_pre_assemble_binary_inst("ldd", Inst::Ldd) }

    #[test]
    fn should_pre_assemble_std() { should_pre_assemble_binary_inst("std", Inst::Std) }

    #[test]
    fn should_pre_assemble_ldi() { should_pre_assemble_binary_inst("ldi", Inst::Ldi) }

    #[test]
    fn should_pre_assemble_ldsp() { should_pre_assemble_unary_inst("ldsp", Inst::Ldsp) }

    #[test]
    fn should_pre_assemble_push() { should_pre_assemble_unary_inst("push", Inst::Push) }

    #[test]
    fn should_pre_assemble_pop() { should_pre_assemble_unary_inst("pop", Inst::Pop) }

    #[test]
    fn should_pre_assemble_je() { should_pre_assemble_unary_inst("je", Inst::Je) }

    #[test]
    fn should_pre_assemble_jne() { should_pre_assemble_unary_inst("jne", Inst::Jne) }

    #[test]
    fn should_pre_assemble_jl() { should_pre_assemble_unary_inst("jl", Inst::Jl) }

    #[test]
    fn should_pre_assemble_jge() { should_pre_assemble_unary_inst("jge", Inst::Jge) }

    #[test]
    fn should_pre_assemble_jcc() { should_pre_assemble_unary_inst("jcc", Inst::Jcc) }

    #[test]
    fn should_pre_assemble_jcs() { should_pre_assemble_unary_inst("jcs", Inst::Jcs) }

    #[test]
    fn should_pre_assemble_jvc() { should_pre_assemble_unary_inst("jvc", Inst::Jvc) }

    #[test]
    fn should_pre_assemble_jvs() { should_pre_assemble_unary_inst("jvs", Inst::Jvs) }

    #[test]
    fn should_pre_assemble_jmp() { should_pre_assemble_unary_inst("jmp", Inst::Jmp) }

    #[test]
    fn should_pre_assemble_rjmp() { should_pre_assemble_unary_inst("rjmp", Inst::Rjmp) }

    #[test]
    fn should_pre_assemble_ijmp() { should_pre_assemble_unary_inst("ijmp", Inst::Ijmp) }

    #[test]
    fn should_pre_assemble_call() { should_pre_assemble_unary_inst("call", Inst::Call) }

    #[test]
    fn should_pre_assemble_rcall() { should_pre_assemble_unary_inst("rcall", Inst::Rcall) }

    #[test]
    fn should_pre_assemble_icall() { should_pre_assemble_unary_inst("icall", Inst::Icall) }

    #[test]
    fn should_pre_assemble_ret() { should_pre_assemble_nullary_inst("ret", Inst::Ret) }

    #[test]
    fn should_pre_assemble_reti() { should_pre_assemble_nullary_inst("reti", Inst::Reti) }

    #[test]
    fn should_pre_assemble_nop() { should_pre_assemble_nullary_inst("nop", Inst::Nop) }

    #[test]
    fn should_pre_assemble_halt() { should_pre_assemble_nullary_inst("halt", Inst::Halt) }

    #[test]
    fn should_fail_pre_assemble_with_unknown_mnemo() {
        assert_eq!(
            pre_assemble_inst("foobar", &exprlist!()),
            Err(PreAssembleError::UnknownMnemo("foobar".to_string())));
    }

    fn should_pre_assemble_nullary_inst(mnemo: &str, inst: PreAssembledInst) {
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!()),
            Ok(inst));
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0))),
            Err(PreAssembleError::BadArgumentCount(0, 1)));
    }

    fn should_pre_assemble_unary_inst<F>(mnemo: &str, inst: F)
        where F: FnOnce(Expr) -> PreAssembledInst
    {
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0))),
            Ok(inst(
                Expr::reg(1, 5, Reg::R0))));
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!()),
            Err(PreAssembleError::BadArgumentCount(1, 0)));
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0),
                Expr::reg(1, 9, Reg::R1))),
            Err(PreAssembleError::BadArgumentCount(1, 2)));
    }

    fn should_pre_assemble_binary_inst<F>(mnemo: &str, inst: F)
        where F: FnOnce(Expr, Expr) -> PreAssembledInst
    {
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0),
                Expr::reg(1, 9, Reg::R1))),
            Ok(inst(
                Expr::reg(1, 5, Reg::R0),
                Expr::reg(1, 9, Reg::R1))));
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!()),
            Err(PreAssembleError::BadArgumentCount(2, 0)));
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0))),
            Err(PreAssembleError::BadArgumentCount(2, 1)));
        assert_eq!(
            pre_assemble_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0),
                Expr::reg(1, 9, Reg::R1),
                Expr::reg(1, 13, Reg::R2))),
            Err(PreAssembleError::BadArgumentCount(2, 3)));
    }
}