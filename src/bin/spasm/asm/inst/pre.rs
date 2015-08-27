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
use asm::inst::*;

#[derive(Clone, Debug, PartialEq)]
pub enum MnemoAssembleError {
    BadArgumentCount { expected: usize, given: usize },
    UnknownMnemo(String),
}

impl fmt::Display for MnemoAssembleError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &MnemoAssembleError::BadArgumentCount { expected, given } =>
                write!(fmt, "expected {} arguments, {} given", expected, given),
            &MnemoAssembleError::UnknownMnemo(ref mnemo) =>
                write!(fmt, "unknown mnemo {}", mnemo),
        }
    }
}

pub fn pre_assemble_inst(
    mnemo: &str,
    args: ExprList) -> Result<PreAssembledInst, MnemoAssembleError>
{
    match mnemo.to_ascii_lowercase().trim() {
        "add" => pre_assemble_binary(args, Inst::Add),
        "adc" => pre_assemble_binary(args, Inst::Adc),
        "addi" => pre_assemble_binary(args, Inst::Addi),
        "sub" => pre_assemble_binary(args, Inst::Sub),
        "sbc" => pre_assemble_binary(args, Inst::Sbc),
        "subi" => pre_assemble_binary(args, Inst::Subi),
        "and" => pre_assemble_binary(args, Inst::And),
        "or" => pre_assemble_binary(args, Inst::Or),
        "xor" => pre_assemble_binary(args, Inst::Xor),
        "lsl" => pre_assemble_binary(args, Inst::Lsl),
        "lsr" => pre_assemble_binary(args, Inst::Lsr),
        "asr" => pre_assemble_binary(args, Inst::Asr),
        "neg" => pre_assemble_unary(args, Inst::Neg),
        "com" => pre_assemble_unary(args, Inst::Com),
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
        "in" => pre_assemble_binary(args, Inst::In),
        "out" => pre_assemble_binary(args, Inst::Out),
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
        "ei" => pre_assemble_nullary(args, Inst::Ei),
        "di" => pre_assemble_nullary(args, Inst::Di),
        _ => Err(MnemoAssembleError::UnknownMnemo(mnemo.to_string()))
    }
}

fn pre_assemble_nullary(
    args: ExprList,
    inst: PreAssembledInst) -> Result<PreAssembledInst, MnemoAssembleError>
{
    if args.len() != 0 {
        Err(MnemoAssembleError::BadArgumentCount { expected: 0, given: args.len() })
    }
    else { Ok(inst) }
}

fn pre_assemble_unary<F>(
    args: ExprList,
    inst: F) -> Result<PreAssembledInst, MnemoAssembleError> where
    F: FnOnce(Expr) -> PreAssembledInst
{
    if args.len() != 1 {
        Err(MnemoAssembleError::BadArgumentCount { expected: 1, given: args.len() })
    }
    else {
        let mut params = args;
        let p0 = params.pop().unwrap();
        Ok(inst(p0))
    }
}

fn pre_assemble_binary<F>(
    args: ExprList,
    inst: F) -> Result<PreAssembledInst, MnemoAssembleError> where
    F: FnOnce(Expr, Expr) -> PreAssembledInst
{
    if args.len() != 2 {
        Err(MnemoAssembleError::BadArgumentCount { expected: 2, given: args.len() })
    }
    else {
        let mut params = args;
        let p1 = params.pop().unwrap();
        let p0 = params.pop().unwrap();
        Ok(inst(p0, p1))
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::inst::*;
    use asm::expr::*;

    use super::*;

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
    fn should_pre_assemble_neg() { should_pre_assemble_unary_inst("neg", Inst::Neg) }

    #[test]
    fn should_pre_assemble_com() { should_pre_assemble_unary_inst("com", Inst::Com) }

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
    fn should_pre_assemble_in() { should_pre_assemble_binary_inst("in", Inst::In) }

    #[test]
    fn should_pre_assemble_out() { should_pre_assemble_binary_inst("out", Inst::Out) }

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
    fn should_pre_assemble_ei() { should_pre_assemble_nullary_inst("ei", Inst::Ei) }

    #[test]
    fn should_pre_assemble_di() { should_pre_assemble_nullary_inst("di", Inst::Di) }

    #[test]
    fn should_fail_pre_assemble_with_unknown_mnemo() {
        assert_eq!(
            pre_assemble_inst("foobar", vec![]),
            Err(MnemoAssembleError::UnknownMnemo("foobar".to_string())));
    }

    fn should_pre_assemble_nullary_inst(mnemo: &str, inst: PreAssembledInst) {
        assert_eq!(
            pre_assemble_inst(mnemo, vec![]),
            Ok(inst));
        assert_eq!(
            pre_assemble_inst(mnemo, vec!(Expr::Reg(Reg::R0))),
            Err(MnemoAssembleError::BadArgumentCount { expected: 0, given: 1 }));
    }

    fn should_pre_assemble_unary_inst<F>(mnemo: &str, inst: F)
        where F: FnOnce(Expr) -> PreAssembledInst
    {
        assert_eq!(
            pre_assemble_inst(mnemo, vec!(Expr::Reg(Reg::R0))),
            Ok(inst(Expr::Reg(Reg::R0))));
        assert_eq!(
            pre_assemble_inst(mnemo, vec![]),
            Err(MnemoAssembleError::BadArgumentCount { expected: 1, given: 0 }));
        assert_eq!(
            pre_assemble_inst(mnemo, vec!(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))),
            Err(MnemoAssembleError::BadArgumentCount { expected: 1, given: 2 }));
    }

    fn should_pre_assemble_binary_inst<F>(mnemo: &str, inst: F)
        where F: FnOnce(Expr, Expr) -> PreAssembledInst
    {
        assert_eq!(
            pre_assemble_inst(mnemo, vec!(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))),
            Ok(inst(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))));
        assert_eq!(
            pre_assemble_inst(mnemo, vec![]),
            Err(MnemoAssembleError::BadArgumentCount { expected: 2, given: 0 }));
        assert_eq!(
            pre_assemble_inst(mnemo, vec!(Expr::Reg(Reg::R0))),
            Err(MnemoAssembleError::BadArgumentCount { expected: 2, given: 1 }));
        assert_eq!(
            pre_assemble_inst(mnemo, vec!(
                Expr::Reg(Reg::R0),
                Expr::Reg(Reg::R1),
                Expr::Reg(Reg::R2))),
            Err(MnemoAssembleError::BadArgumentCount { expected: 2, given: 3 }));
    }
}
