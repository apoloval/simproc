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
pub struct PreAssembledOperands;

impl Operands for PreAssembledOperands {
    type Immediate = Expr;
    type Addr = Expr;
    type RelAddr = Expr;
    type Reg = Expr;
    type AddrReg = Expr;
}

pub type PreAssembledInst = Inst<PreAssembledOperands>;

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

pub struct InstAssembler<E: ExprAssembler> {
    expr_asm: E,
}

pub type StdInstAssembler<'a> = InstAssembler<StdExprAssembler<'a>>;

impl<E: ExprAssembler> InstAssembler<E> {

    pub fn from_expr_asm(expr_asm: E) -> Self {
        InstAssembler { expr_asm: expr_asm }
    }

    pub fn assemble(
        &mut self,
        inst: PreAssembledInst, base: Addr) -> Result<RuntimeInst, ExprAssembleError>
    {
        match inst {
            Inst::Add(r1, r2) =>
                Ok(Inst::Add(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Adc(r1, r2) =>
                Ok(Inst::Adc(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Addi(r, l) =>
                Ok(Inst::Addi(try!(self.expr_asm.to_reg(r)), try!(self.expr_asm.to_immediate(l)))),
            Inst::Sub(r1, r2) =>
                Ok(Inst::Sub(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Sbc(r1, r2) =>
                Ok(Inst::Sbc(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Subi(r, l) =>
                Ok(Inst::Subi(try!(self.expr_asm.to_reg(r)), try!(self.expr_asm.to_immediate(l)))),
            Inst::Mulw(r1, r2) =>
                Ok(Inst::Mulw(try!(self.expr_asm.to_areg(r1)), try!(self.expr_asm.to_areg(r2)))),
            Inst::And(r1, r2) =>
                Ok(Inst::And(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Or(r1, r2) =>
                Ok(Inst::Or(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Xor(r1, r2) =>
                Ok(Inst::Xor(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Lsl(r1, r2) =>
                Ok(Inst::Lsl(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Lsr(r1, r2) =>
                Ok(Inst::Lsr(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Asr(r1, r2) =>
                Ok(Inst::Asr(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Not(r) =>
                Ok(Inst::Not(try!(self.expr_asm.to_reg(r)))),
            Inst::Comp(r) =>
                Ok(Inst::Comp(try!(self.expr_asm.to_reg(r)))),
            Inst::Inc(r) =>
                Ok(Inst::Inc(try!(self.expr_asm.to_reg(r)))),
            Inst::Incw(r) =>
                Ok(Inst::Incw(try!(self.expr_asm.to_areg(r)))),
            Inst::Dec(r) =>
                Ok(Inst::Dec(try!(self.expr_asm.to_reg(r)))),
            Inst::Decw(r) =>
                Ok(Inst::Decw(try!(self.expr_asm.to_areg(r)))),

            Inst::Mov(r1, r2) =>
                Ok(Inst::Mov(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Ld(r1, r2) =>
                Ok(Inst::Ld(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_areg(r2)))),
            Inst::St(r1, r2) =>
                Ok(Inst::St(try!(self.expr_asm.to_areg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Ldd(r1, r2) =>
                Ok(Inst::Ldd(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_addr(r2)))),
            Inst::Std(r1, r2) =>
                Ok(Inst::Std(try!(self.expr_asm.to_addr(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Ldi(r, l) =>
                Ok(Inst::Ldi(try!(self.expr_asm.to_reg(r)), try!(self.expr_asm.to_immediate(l)))),
            Inst::Ldsp(r) =>
                Ok(Inst::Ldsp(try!(self.expr_asm.to_areg(r)))),
            Inst::Push(r) =>
                Ok(Inst::Push(try!(self.expr_asm.to_reg(r)))),
            Inst::Pop(r) =>
                Ok(Inst::Pop(try!(self.expr_asm.to_reg(r)))),

            Inst::Je(a) =>
                Ok(Inst::Je(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jne(a) =>
                Ok(Inst::Jne(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jl(a) =>
                Ok(Inst::Jl(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jge(a) =>
                Ok(Inst::Jge(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jcc(a) =>
                Ok(Inst::Jcc(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jcs(a) =>
                Ok(Inst::Jcs(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jvc(a) =>
                Ok(Inst::Jvc(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jvs(a) =>
                Ok(Inst::Jvs(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jmp(a) =>
                Ok(Inst::Jmp(try!(self.expr_asm.to_addr(a)))),
            Inst::Rjmp(a) =>
                Ok(Inst::Rjmp(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Ijmp(r) =>
                Ok(Inst::Ijmp(try!(self.expr_asm.to_areg(r)))),
            Inst::Call(a) =>
                Ok(Inst::Call(try!(self.expr_asm.to_addr(a)))),
            Inst::Rcall(a) =>
                Ok(Inst::Rcall(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Icall(r) =>
                Ok(Inst::Icall(try!(self.expr_asm.to_areg(r)))),
            Inst::Ret =>
                Ok(Inst::Ret),
            Inst::Reti =>
                Ok(Inst::Reti),

            Inst::Nop =>
                Ok(Inst::Nop),
            Inst::Halt =>
                Ok(Inst::Halt),
        }
    }
}

#[cfg(test)]
mod test {

    use std::collections::VecDeque;

    use simproc::inst::*;

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
            pre_assemble_inst("foobar", vec![]),
            Err(MnemoAssembleError::UnknownMnemo("foobar".to_string())));
    }

    #[test]
    fn should_asm_add() { should_asm_inst_reg_reg(Inst::Add, Inst::Add); }

    #[test]
    fn should_asm_adc() { should_asm_inst_reg_reg(Inst::Adc, Inst::Adc); }

    #[test]
    fn should_asm_addi() { should_asm_inst_reg_imm(Inst::Addi, Inst::Addi); }

    #[test]
    fn should_asm_sub() { should_asm_inst_reg_reg(Inst::Sub, Inst::Sub); }

    #[test]
    fn should_asm_sbc() { should_asm_inst_reg_reg(Inst::Sbc, Inst::Sbc); }

    #[test]
    fn should_asm_subi() { should_asm_inst_reg_imm(Inst::Subi, Inst::Subi); }

    #[test]
    fn should_asm_mulw() { should_asm_inst_areg_areg(Inst::Mulw, Inst::Mulw); }

    #[test]
    fn should_asm_and() { should_asm_inst_reg_reg(Inst::And, Inst::And); }

    #[test]
    fn should_asm_or() { should_asm_inst_reg_reg(Inst::Or, Inst::Or); }

    #[test]
    fn should_asm_xor() { should_asm_inst_reg_reg(Inst::Xor, Inst::Xor); }

    #[test]
    fn should_asm_lsl() { should_asm_inst_reg_reg(Inst::Lsl, Inst::Lsl); }

    #[test]
    fn should_asm_lsr() { should_asm_inst_reg_reg(Inst::Lsr, Inst::Lsr); }

    #[test]
    fn should_asm_asr() { should_asm_inst_reg_reg(Inst::Asr, Inst::Asr); }

    #[test]
    fn should_asm_not() { should_asm_inst_reg(Inst::Not, Inst::Not); }

    #[test]
    fn should_asm_comp() { should_asm_inst_reg(Inst::Comp, Inst::Comp); }

    #[test]
    fn should_asm_inc() { should_asm_inst_reg(Inst::Inc, Inst::Inc); }

    #[test]
    fn should_asm_incw() { should_asm_inst_areg(Inst::Incw, Inst::Incw); }

    #[test]
    fn should_asm_dec() { should_asm_inst_reg(Inst::Dec, Inst::Dec); }

    #[test]
    fn should_asm_decw() { should_asm_inst_areg(Inst::Decw, Inst::Decw); }

    #[test]
    fn should_asm_mov() { should_asm_inst_reg_reg(Inst::Mov, Inst::Mov); }

    #[test]
    fn should_asm_ld() { should_asm_inst_reg_areg(Inst::Ld, Inst::Ld); }

    #[test]
    fn should_asm_st() { should_asm_inst_areg_reg(Inst::St, Inst::St); }

    #[test]
    fn should_asm_ldd() { should_asm_inst_reg_addr(Inst::Ldd, Inst::Ldd); }

    #[test]
    fn should_asm_std() { should_asm_inst_addr_reg(Inst::Std, Inst::Std); }

    #[test]
    fn should_asm_ldi() { should_asm_inst_reg_imm(Inst::Ldi, Inst::Ldi); }

    #[test]
    fn should_asm_ldsp() { should_asm_inst_areg(Inst::Ldsp, Inst::Ldsp); }

    #[test]
    fn should_asm_push() { should_asm_inst_reg(Inst::Push, Inst::Push); }

    #[test]
    fn should_asm_pop() { should_asm_inst_reg(Inst::Pop, Inst::Pop); }

    #[test]
    fn should_asm_je() { should_asm_inst_raddr(Inst::Je, Inst::Je); }

    #[test]
    fn should_asm_jne() { should_asm_inst_raddr(Inst::Jne, Inst::Jne); }

    #[test]
    fn should_asm_jl() { should_asm_inst_raddr(Inst::Jl, Inst::Jl); }

    #[test]
    fn should_asm_jge() { should_asm_inst_raddr(Inst::Jge, Inst::Jge); }

    #[test]
    fn should_asm_jcc() { should_asm_inst_raddr(Inst::Jcc, Inst::Jcc); }

    #[test]
    fn should_asm_jcs() { should_asm_inst_raddr(Inst::Jcs, Inst::Jcs); }

    #[test]
    fn should_asm_jvc() { should_asm_inst_raddr(Inst::Jvc, Inst::Jvc); }

    #[test]
    fn should_asm_jvs() { should_asm_inst_raddr(Inst::Jvs, Inst::Jvs); }

    #[test]
    fn should_asm_jmp() { should_asm_inst_addr(Inst::Jmp, Inst::Jmp); }

    #[test]
    fn should_asm_rjmp() { should_asm_inst_raddr(Inst::Rjmp, Inst::Rjmp); }

    #[test]
    fn should_asm_ijmp() { should_asm_inst_areg(Inst::Ijmp, Inst::Ijmp); }

    #[test]
    fn should_asm_call() { should_asm_inst_addr(Inst::Call, Inst::Call); }

    #[test]
    fn should_asm_rcall() { should_asm_inst_raddr(Inst::Rcall, Inst::Rcall); }

    #[test]
    fn should_asm_icall() { should_asm_inst_areg(Inst::Icall, Inst::Icall); }

    #[test]
    fn should_asm_ret() { should_asm_nullary_inst(Inst::Ret, Inst::Ret); }

    #[test]
    fn should_asm_reti() { should_asm_nullary_inst(Inst::Reti, Inst::Reti); }

    #[test]
    fn should_asm_nop() { should_asm_nullary_inst(Inst::Nop, Inst::Nop); }

    #[test]
    fn should_asm_halt() { should_asm_nullary_inst(Inst::Halt, Inst::Halt); }

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

    struct MockExprAssembler {
        next_reg: VecDeque<Result<Reg, ExprAssembleError>>,
        next_areg: VecDeque<Result<AddrReg, ExprAssembleError>>,
        next_imm: VecDeque<Result<Immediate, ExprAssembleError>>,
        next_addr: VecDeque<Result<Addr, ExprAssembleError>>,
        next_raddr: VecDeque<Result<RelAddr, ExprAssembleError>>,
    }

    impl MockExprAssembler {
        fn new() -> Self {
            MockExprAssembler {
                next_reg: VecDeque::new(),
                next_areg: VecDeque::new(),
                next_imm: VecDeque::new(),
                next_addr: VecDeque::new(),
                next_raddr: VecDeque::new(),
            }
        }

        fn with_reg(&mut self, reg: Result<Reg, ExprAssembleError>) {
            self.next_reg.push_back(reg);
        }

        fn with_areg(&mut self, areg: Result<AddrReg, ExprAssembleError>) {
            self.next_areg.push_back(areg);
        }

        fn with_imm(&mut self, imm: Result<Immediate, ExprAssembleError>) {
            self.next_imm.push_back(imm);
        }

        fn with_addr(&mut self, addr: Result<Addr, ExprAssembleError>) {
            self.next_addr.push_back(addr);
        }

        fn with_raddr(&mut self, raddr: Result<RelAddr, ExprAssembleError>) {
            self.next_raddr.push_back(raddr);
        }
    }

    impl ExprAssembler for MockExprAssembler {
        fn to_reg(&mut self, _e: Expr) -> Result<Reg, ExprAssembleError> {
            self.next_reg.pop_front().unwrap()
        }
        fn to_areg(&mut self, _e: Expr) -> Result<AddrReg, ExprAssembleError> {
            self.next_areg.pop_front().unwrap()
        }
        fn to_immediate(&mut self, _e: Expr) -> Result<Immediate, ExprAssembleError> {
            self.next_imm.pop_front().unwrap()
        }
        fn to_addr(&mut self, _e: Expr) -> Result<Addr, ExprAssembleError> {
            self.next_addr.pop_front().unwrap()
        }
        fn to_raddr(&mut self, _e: Expr, _base: Addr) -> Result<RelAddr, ExprAssembleError> {
            self.next_raddr.pop_front().unwrap()
        }
    }

    fn should_asm_nullary_inst(pre: PreAssembledInst, full: RuntimeInst) {
        let expr = MockExprAssembler::new();
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(asm.assemble(pre, Addr(1000)), Ok(full));
    }

    fn should_asm_unary_inst<I1, I2, O1, M1>(pre: I1, full: I2, o1: O1, m1: M1) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(O1) -> RuntimeInst,
        O1: Clone,
        M1: Fn(&mut MockExprAssembler, Result<O1, ExprAssembleError>),
    {
        let err = ExprAssembleError::TypeMismatch { expected: "foobar".to_string() };
        let mut expr = MockExprAssembler::new();
        m1(&mut expr, Ok(o1.clone()));
        m1(&mut expr, Err(err.clone()));
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(
            asm.assemble(pre(Expr::Number(1)), Addr(1000)),
            Ok(full(o1)));
        assert_eq!(
            asm.assemble(pre(Expr::Number(1)), Addr(1000)),
            Err(err.clone()));
    }

    fn should_asm_binary_inst<I1, I2, O1, O2, M1, M2>(pre: I1, full: I2, o1: O1, o2: O2, m1: M1, m2: M2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(O1, O2) -> RuntimeInst,
        O1: Clone,
        O2: Clone,
        M1: Fn(&mut MockExprAssembler, Result<O1, ExprAssembleError>),
        M2: Fn(&mut MockExprAssembler, Result<O2, ExprAssembleError>),
    {
        let err = ExprAssembleError::TypeMismatch { expected: "foobar".to_string() };
        let mut expr = MockExprAssembler::new();
        m1(&mut expr, Ok(o1.clone()));
        m2(&mut expr, Ok(o2.clone()));
        m1(&mut expr, Ok(o1.clone()));
        m2(&mut expr, Err(err.clone()));
        m1(&mut expr, Err(err.clone()));
        m2(&mut expr, Ok(o2.clone()));
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(
            asm.assemble(pre(Expr::Number(1), Expr::Number(1)), Addr(1000)),
            Ok(full(o1, o2)));
        assert_eq!(
            asm.assemble(pre(Expr::Number(1), Expr::Number(1)), Addr(1000)),
            Err(err.clone()));
        assert_eq!(
            asm.assemble(pre(Expr::Number(1), Expr::Number(1)), Addr(1000)),
            Err(err.clone()));
    }

    fn should_asm_inst_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(Reg) -> RuntimeInst
    {
        should_asm_unary_inst(
            pre, full,
            Reg::R0,
            MockExprAssembler::with_reg);
    }

    fn should_asm_inst_areg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(AddrReg) -> RuntimeInst
    {
        should_asm_unary_inst(
            pre, full,
            AddrReg::A0,
            MockExprAssembler::with_areg);
    }

    fn should_asm_inst_addr<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(Addr) -> RuntimeInst
    {
        should_asm_unary_inst(
            pre, full,
            Addr(100),
            MockExprAssembler::with_addr);
    }

    fn should_asm_inst_raddr<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(RelAddr) -> RuntimeInst
    {
        should_asm_unary_inst(
            pre, full,
            RelAddr(100),
            MockExprAssembler::with_raddr);
    }

    fn should_asm_inst_reg_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, Reg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, Reg::R1,
            MockExprAssembler::with_reg, MockExprAssembler::with_reg);
    }

    fn should_asm_inst_reg_imm<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, Immediate) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, Immediate(100),
            MockExprAssembler::with_reg, MockExprAssembler::with_imm);
    }

    fn should_asm_inst_areg_areg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(AddrReg, AddrReg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            AddrReg::A0, AddrReg::A1,
            MockExprAssembler::with_areg, MockExprAssembler::with_areg);
    }

    fn should_asm_inst_reg_areg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, AddrReg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, AddrReg::A0,
            MockExprAssembler::with_reg, MockExprAssembler::with_areg);
    }

    fn should_asm_inst_areg_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(AddrReg, Reg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            AddrReg::A0, Reg::R0,
            MockExprAssembler::with_areg, MockExprAssembler::with_reg);
    }

    fn should_asm_inst_reg_addr<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, Addr) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, Addr(100),
            MockExprAssembler::with_reg, MockExprAssembler::with_addr);
    }

    fn should_asm_inst_addr_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Addr, Reg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Addr(100), Reg::R0,
            MockExprAssembler::with_addr, MockExprAssembler::with_reg);
    }
}
