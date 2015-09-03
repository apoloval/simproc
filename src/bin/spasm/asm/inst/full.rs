//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use simproc::inst::*;
use simproc::mem::*;

use asm::expr::*;
use asm::inst::*;

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
            Inst::And(r1, r2) =>
                Ok(Inst::And(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Or(r1, r2) =>
                Ok(Inst::Or(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Xor(r1, r2) =>
                Ok(Inst::Xor(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Lsl(r1) =>
                Ok(Inst::Lsl(try!(self.expr_asm.to_reg(r1)))),
            Inst::Lsr(r1) =>
                Ok(Inst::Lsr(try!(self.expr_asm.to_reg(r1)))),
            Inst::Asr(r1) =>
                Ok(Inst::Asr(try!(self.expr_asm.to_reg(r1)))),
            Inst::Neg(r) =>
                Ok(Inst::Neg(try!(self.expr_asm.to_reg(r)))),
            Inst::Com(r) =>
                Ok(Inst::Com(try!(self.expr_asm.to_reg(r)))),
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
            Inst::Ldw(r1, addr) =>
                Ok(Inst::Ldw(try!(self.expr_asm.to_areg(r1)), try!(self.expr_asm.to_addr(addr)))),
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
            Inst::In(r, p) =>
                Ok(Inst::In(try!(self.expr_asm.to_reg(r)), try!(self.expr_asm.to_ioport(p)))),
            Inst::Out(p, r) =>
                Ok(Inst::Out(try!(self.expr_asm.to_ioport(p)), try!(self.expr_asm.to_reg(r)))),

            Inst::Jnz(a) =>
                Ok(Inst::Jnz(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jz(a) =>
                Ok(Inst::Jz(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jp(a) =>
                Ok(Inst::Jp(try!(self.expr_asm.to_raddr(a, base)))),
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

            Inst::Nop => Ok(Inst::Nop),
            Inst::Halt => Ok(Inst::Halt),
            Inst::Ei => Ok(Inst::Ei),
            Inst::Di => Ok(Inst::Di),
        }
    }
}

#[cfg(test)]
mod test {

    use std::collections::VecDeque;

    use simproc::inst::*;
    use simproc::mem::*;

    use asm::expr::*;
    use asm::inst::*;

    use super::*;

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
    fn should_asm_and() { should_asm_inst_reg_reg(Inst::And, Inst::And); }

    #[test]
    fn should_asm_or() { should_asm_inst_reg_reg(Inst::Or, Inst::Or); }

    #[test]
    fn should_asm_xor() { should_asm_inst_reg_reg(Inst::Xor, Inst::Xor); }

    #[test]
    fn should_asm_lsl() { should_asm_inst_reg(Inst::Lsl, Inst::Lsl); }

    #[test]
    fn should_asm_lsr() { should_asm_inst_reg(Inst::Lsr, Inst::Lsr); }

    #[test]
    fn should_asm_asr() { should_asm_inst_reg(Inst::Asr, Inst::Asr); }

    #[test]
    fn should_asm_neg() { should_asm_inst_reg(Inst::Neg, Inst::Neg); }

    #[test]
    fn should_asm_com() { should_asm_inst_reg(Inst::Com, Inst::Com); }

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
    fn should_asm_ldw() { should_asm_inst_areg_addr(Inst::Ldw, Inst::Ldw); }

    #[test]
    fn should_asm_ldsp() { should_asm_inst_areg(Inst::Ldsp, Inst::Ldsp); }

    #[test]
    fn should_asm_push() { should_asm_inst_reg(Inst::Push, Inst::Push); }

    #[test]
    fn should_asm_pop() { should_asm_inst_reg(Inst::Pop, Inst::Pop); }

    #[test]
    fn should_asm_in() { should_asm_inst_reg_ioport(Inst::In, Inst::In); }

    #[test]
    fn should_asm_out() { should_asm_inst_ioport_reg(Inst::Out, Inst::Out); }

    #[test]
    fn should_asm_jnz() { should_asm_inst_raddr(Inst::Jnz, Inst::Jnz); }

    #[test]
    fn should_asm_jz() { should_asm_inst_raddr(Inst::Jz, Inst::Jz); }

    #[test]
    fn should_asm_jp() { should_asm_inst_raddr(Inst::Jp, Inst::Jp); }

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

    #[test]
    fn should_asm_ei() { should_asm_nullary_inst(Inst::Ei, Inst::Ei); }

    #[test]
    fn should_asm_di() { should_asm_nullary_inst(Inst::Di, Inst::Di); }

    struct MockExprAssembler {
        next_reg: VecDeque<Result<Reg, ExprAssembleError>>,
        next_areg: VecDeque<Result<AddrReg, ExprAssembleError>>,
        next_imm: VecDeque<Result<Immediate, ExprAssembleError>>,
        next_addr: VecDeque<Result<Addr, ExprAssembleError>>,
        next_raddr: VecDeque<Result<RelAddr, ExprAssembleError>>,
        next_ioport: VecDeque<Result<IoPort, ExprAssembleError>>,
    }

    impl MockExprAssembler {
        fn new() -> Self {
            MockExprAssembler {
                next_reg: VecDeque::new(),
                next_areg: VecDeque::new(),
                next_imm: VecDeque::new(),
                next_addr: VecDeque::new(),
                next_raddr: VecDeque::new(),
                next_ioport: VecDeque::new(),
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

        fn with_ioport(&mut self, port: Result<IoPort, ExprAssembleError>) {
            self.next_ioport.push_back(port);
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
        fn to_ioport(&mut self, _e: Expr) -> Result<IoPort, ExprAssembleError> {
            self.next_ioport.pop_front().unwrap()
        }
    }

    fn should_asm_nullary_inst(pre: PreAssembledInst, full: RuntimeInst) {
        let expr = MockExprAssembler::new();
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(asm.assemble(pre, 1000), Ok(full));
    }

    fn should_asm_unary_inst<I1, I2, O1, M1>(pre: I1, full: I2, o1: O1, m1: M1) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(O1) -> RuntimeInst,
        O1: Clone,
        M1: Fn(&mut MockExprAssembler, Result<O1, ExprAssembleError>),
    {
        let err = ExprAssembleError::Undefined { symbol: "foobar".to_string() };
        let mut expr = MockExprAssembler::new();
        m1(&mut expr, Ok(o1.clone()));
        m1(&mut expr, Err(err.clone()));
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(
            asm.assemble(pre(Expr::Number(1)), 1000),
            Ok(full(o1)));
        assert_eq!(
            asm.assemble(pre(Expr::Number(1)), 1000),
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
        let err = ExprAssembleError::Undefined { symbol: "foobar".to_string() };
        let mut expr = MockExprAssembler::new();
        m1(&mut expr, Ok(o1.clone()));
        m2(&mut expr, Ok(o2.clone()));
        m1(&mut expr, Ok(o1.clone()));
        m2(&mut expr, Err(err.clone()));
        m1(&mut expr, Err(err.clone()));
        m2(&mut expr, Ok(o2.clone()));
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(
            asm.assemble(pre(Expr::Number(1), Expr::Number(1)), 1000),
            Ok(full(o1, o2)));
        assert_eq!(
            asm.assemble(pre(Expr::Number(1), Expr::Number(1)), 1000),
            Err(err.clone()));
        assert_eq!(
            asm.assemble(pre(Expr::Number(1), Expr::Number(1)), 1000),
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
        should_asm_unary_inst(pre, full, 100, MockExprAssembler::with_addr);
    }

    fn should_asm_inst_raddr<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(RelAddr) -> RuntimeInst
    {
        should_asm_unary_inst(pre, full, 100, MockExprAssembler::with_raddr);
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

    fn should_asm_inst_reg_ioport<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, IoPort) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, IoPort(100),
            MockExprAssembler::with_reg, MockExprAssembler::with_ioport);
    }

    fn should_asm_inst_ioport_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(IoPort, Reg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            IoPort(100), Reg::R0,
            MockExprAssembler::with_ioport, MockExprAssembler::with_reg);
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
            Reg::R0, 100,
            MockExprAssembler::with_reg, MockExprAssembler::with_addr);
    }

    fn should_asm_inst_areg_addr<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(AddrReg, Addr) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            AddrReg::A0, 100,
            MockExprAssembler::with_areg, MockExprAssembler::with_addr);
    }

    fn should_asm_inst_addr_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Addr, Reg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            100, Reg::R0,
            MockExprAssembler::with_addr, MockExprAssembler::with_reg);
    }
}
