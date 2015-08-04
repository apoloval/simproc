//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::convert::From;

use simproc::inst::*;

use asm::new_parser::{Expr, ExprList};
use asm::new_err::*;

#[derive(Debug, PartialEq)]
pub struct SyntheticOperands;

impl Operands for SyntheticOperands {
    type Immediate = Expr;
    type Addr = Expr;
    type RelAddr = Expr;
    type Reg = Expr;
    type AddrReg = Expr;
}

pub type SyntheticInst = Inst<SyntheticOperands>;

#[derive(Debug, PartialEq)]
pub enum SynthError {
    BadArgumentCount(usize, usize),
    UnknownMnemo(String),
}

pub fn synth_inst(mnemo: &str, args: &ExprList) -> Result<SyntheticInst, SynthError> {
    match mnemo.to_ascii_lowercase().trim() {
        "add" => synth_binary(args, Inst::Add),
        "adc" => synth_binary(args, Inst::Adc),
        "addi" => synth_binary(args, Inst::Addi),
        "sub" => synth_binary(args, Inst::Sub),
        "sbc" => synth_binary(args, Inst::Sbc),
        "subi" => synth_binary(args, Inst::Subi),
        "mulw" => synth_binary(args, Inst::Mulw),
        "and" => synth_binary(args, Inst::And),
        "or" => synth_binary(args, Inst::Or),
        "xor" => synth_binary(args, Inst::Xor),
        "lsl" => synth_binary(args, Inst::Lsl),
        "lsr" => synth_binary(args, Inst::Lsr),
        "asr" => synth_binary(args, Inst::Asr),
        "not" => synth_unary(args, Inst::Not),
        "comp" => synth_unary(args, Inst::Comp),
        "inc" => synth_unary(args, Inst::Inc),
        "incw" => synth_unary(args, Inst::Incw),
        "dec" => synth_unary(args, Inst::Dec),
        "decw" => synth_unary(args, Inst::Decw),
        "mov" => synth_binary(args, Inst::Mov),
        "ld" => synth_binary(args, Inst::Ld),
        "st" => synth_binary(args, Inst::St),
        "ldd" => synth_binary(args, Inst::Ldd),
        "std" => synth_binary(args, Inst::Std),
        "ldi" => synth_binary(args, Inst::Ldi),
        "ldsp" => synth_unary(args, Inst::Ldsp),
        "push" => synth_unary(args, Inst::Push),
        "pop" => synth_unary(args, Inst::Pop),
        "je" => synth_unary(args, Inst::Je),
        "jne" => synth_unary(args, Inst::Jne),
        "jl" => synth_unary(args, Inst::Jl),
        "jge" => synth_unary(args, Inst::Jge),
        "jcc" => synth_unary(args, Inst::Jcc),
        "jcs" => synth_unary(args, Inst::Jcs),
        "jvc" => synth_unary(args, Inst::Jvc),
        "jvs" => synth_unary(args, Inst::Jvs),
        "jmp" => synth_unary(args, Inst::Jmp),
        "rjmp" => synth_unary(args, Inst::Rjmp),
        "ijmp" => synth_unary(args, Inst::Ijmp),
        "call" => synth_unary(args, Inst::Call),
        "rcall" => synth_unary(args, Inst::Rcall),
        "icall" => synth_unary(args, Inst::Icall),
        "ret" => synth_nullary(args, Inst::Ret),
        "reti" => synth_nullary(args, Inst::Reti),
        "nop" => synth_nullary(args, Inst::Nop),
        "halt" => synth_nullary(args, Inst::Halt),
        _ => Err(SynthError::UnknownMnemo(mnemo.to_string()))
    }
}

fn synth_nullary(args: &ExprList, inst: SyntheticInst) -> Result<SyntheticInst, SynthError> {
    if args.len() != 0 { Err(SynthError::BadArgumentCount(0, args.len())) }
    else { Ok(inst) }
}

fn synth_unary<F>(args: &ExprList, inst: F) -> Result<SyntheticInst, SynthError>
    where F: FnOnce(Expr) -> SyntheticInst
{
    if args.len() != 1 { Err(SynthError::BadArgumentCount(1, args.len())) }
    else { Ok(inst(args[0].clone())) }
}

fn synth_binary<F>(args: &ExprList, inst: F) -> Result<SyntheticInst, SynthError>
    where F: FnOnce(Expr, Expr) -> SyntheticInst
{
    if args.len() != 2 { Err(SynthError::BadArgumentCount(2, args.len())) }
    else { Ok(inst(args[0].clone(), args[1].clone())) }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::lexer::*;
    use asm::new_parser::{Expr, ExprList};

    use super::*;

    #[test]
    fn should_synth_add() { should_synth_binary_inst("add", Inst::Add) }

    #[test]
    fn should_synth_adc() { should_synth_binary_inst("adc", Inst::Adc) }

    #[test]
    fn should_synth_addi() { should_synth_binary_inst("addi", Inst::Addi) }

    #[test]
    fn should_synth_sub() { should_synth_binary_inst("sub", Inst::Sub) }

    #[test]
    fn should_synth_sbc() { should_synth_binary_inst("sbc", Inst::Sbc) }

    #[test]
    fn should_synth_subi() { should_synth_binary_inst("subi", Inst::Subi) }

    #[test]
    fn should_synth_mulw() { should_synth_binary_inst("mulw", Inst::Mulw) }

    #[test]
    fn should_synth_and() { should_synth_binary_inst("and", Inst::And) }

    #[test]
    fn should_synth_or() { should_synth_binary_inst("or", Inst::Or) }

    #[test]
    fn should_synth_xor() { should_synth_binary_inst("xor", Inst::Xor) }

    #[test]
    fn should_synth_lsl() { should_synth_binary_inst("lsl", Inst::Lsl) }

    #[test]
    fn should_synth_lsr() { should_synth_binary_inst("lsr", Inst::Lsr) }

    #[test]
    fn should_synth_asr() { should_synth_binary_inst("asr", Inst::Asr) }

    #[test]
    fn should_synth_not() { should_synth_unary_inst("not", Inst::Not) }

    #[test]
    fn should_synth_comp() { should_synth_unary_inst("comp", Inst::Comp) }

    #[test]
    fn should_synth_inc() { should_synth_unary_inst("inc", Inst::Inc) }

    #[test]
    fn should_synth_incw() { should_synth_unary_inst("incw", Inst::Incw) }

    #[test]
    fn should_synth_dec() { should_synth_unary_inst("dec", Inst::Dec) }

    #[test]
    fn should_synth_decw() { should_synth_unary_inst("decw", Inst::Decw) }

    #[test]
    fn should_synth_mov() { should_synth_binary_inst("mov", Inst::Mov) }

    #[test]
    fn should_synth_ld() { should_synth_binary_inst("ld", Inst::Ld) }

    #[test]
    fn should_synth_st() { should_synth_binary_inst("st", Inst::St) }

    #[test]
    fn should_synth_ldd() { should_synth_binary_inst("ldd", Inst::Ldd) }

    #[test]
    fn should_synth_std() { should_synth_binary_inst("std", Inst::Std) }

    #[test]
    fn should_synth_ldi() { should_synth_binary_inst("ldi", Inst::Ldi) }

    #[test]
    fn should_synth_ldsp() { should_synth_unary_inst("ldsp", Inst::Ldsp) }

    #[test]
    fn should_synth_push() { should_synth_unary_inst("push", Inst::Push) }

    #[test]
    fn should_synth_pop() { should_synth_unary_inst("pop", Inst::Pop) }

    #[test]
    fn should_synth_je() { should_synth_unary_inst("je", Inst::Je) }

    #[test]
    fn should_synth_jne() { should_synth_unary_inst("jne", Inst::Jne) }

    #[test]
    fn should_synth_jl() { should_synth_unary_inst("jl", Inst::Jl) }

    #[test]
    fn should_synth_jge() { should_synth_unary_inst("jge", Inst::Jge) }

    #[test]
    fn should_synth_jcc() { should_synth_unary_inst("jcc", Inst::Jcc) }

    #[test]
    fn should_synth_jcs() { should_synth_unary_inst("jcs", Inst::Jcs) }

    #[test]
    fn should_synth_jvc() { should_synth_unary_inst("jvc", Inst::Jvc) }

    #[test]
    fn should_synth_jvs() { should_synth_unary_inst("jvs", Inst::Jvs) }

    #[test]
    fn should_synth_jmp() { should_synth_unary_inst("jmp", Inst::Jmp) }

    #[test]
    fn should_synth_rjmp() { should_synth_unary_inst("rjmp", Inst::Rjmp) }

    #[test]
    fn should_synth_ijmp() { should_synth_unary_inst("ijmp", Inst::Ijmp) }

    #[test]
    fn should_synth_call() { should_synth_unary_inst("call", Inst::Call) }

    #[test]
    fn should_synth_rcall() { should_synth_unary_inst("rcall", Inst::Rcall) }

    #[test]
    fn should_synth_icall() { should_synth_unary_inst("icall", Inst::Icall) }

    #[test]
    fn should_synth_ret() { should_synth_nullary_inst("ret", Inst::Ret) }

    #[test]
    fn should_synth_reti() { should_synth_nullary_inst("reti", Inst::Reti) }

    #[test]
    fn should_synth_nop() { should_synth_nullary_inst("nop", Inst::Nop) }

    #[test]
    fn should_synth_halt() { should_synth_nullary_inst("halt", Inst::Halt) }

    #[test]
    fn should_fail_synth_with_unknown_mnemo() {
        assert_eq!(
            synth_inst("foobar", &exprlist!()),
            Err(SynthError::UnknownMnemo("foobar".to_string())));
    }

    fn should_synth_nullary_inst(mnemo: &str, inst: SyntheticInst) {
        assert_eq!(
            synth_inst(mnemo, &exprlist!()),
            Ok(inst));
        assert_eq!(
            synth_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0))),
            Err(SynthError::BadArgumentCount(0, 1)));
    }

    fn should_synth_unary_inst<F>(mnemo: &str, inst: F)
        where F: FnOnce(Expr) -> SyntheticInst
    {
        assert_eq!(
            synth_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0))),
            Ok(inst(
                Expr::reg(1, 5, Reg::R0))));
        assert_eq!(
            synth_inst(mnemo, &exprlist!()),
            Err(SynthError::BadArgumentCount(1, 0)));
        assert_eq!(
            synth_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0),
                Expr::reg(1, 9, Reg::R1))),
            Err(SynthError::BadArgumentCount(1, 2)));
    }

    fn should_synth_binary_inst<F>(mnemo: &str, inst: F)
        where F: FnOnce(Expr, Expr) -> SyntheticInst
    {
        assert_eq!(
            synth_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0),
                Expr::reg(1, 9, Reg::R1))),
            Ok(inst(
                Expr::reg(1, 5, Reg::R0),
                Expr::reg(1, 9, Reg::R1))));
        assert_eq!(
            synth_inst(mnemo, &exprlist!()),
            Err(SynthError::BadArgumentCount(2, 0)));
        assert_eq!(
            synth_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0))),
            Err(SynthError::BadArgumentCount(2, 1)));
        assert_eq!(
            synth_inst(mnemo, &exprlist!(
                Expr::reg(1, 5, Reg::R0),
                Expr::reg(1, 9, Reg::R1),
                Expr::reg(1, 13, Reg::R2))),
            Err(SynthError::BadArgumentCount(2, 3)));
    }
}

