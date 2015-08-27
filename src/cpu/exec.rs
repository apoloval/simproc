//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use cpu::clock::Cycle;
use cpu::reg::Regs;
use inst::{Inst, RuntimeInst, Reg};
use mem::Memory;

pub trait ExecCtx {
    type Mem: Memory;
    fn mem(&mut self) -> &mut Self::Mem;
    fn regs(&mut self) -> &mut Regs;
}

/// Execute the given instruction over the given context
/// It returns the number of cycles that correspond to that instruction execution.
pub fn exec<M: Memory>(inst: &RuntimeInst, ctx: &mut ExecCtx<Mem=M>) -> Cycle {
    match inst {
        &Inst::Nop => exec_nop(ctx),
        &Inst::Add(dst, src) => exec_add(ctx, dst, src, false),
        &Inst::Adc(dst, src) => exec_add(ctx, dst, src, true),
        _ => unimplemented!(),
    }
}

fn exec_nop<M: Memory>(ctx: &mut ExecCtx<Mem=M>) -> Cycle {
    ctx.regs().pc += 1; 4
}

fn exec_add<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg, src: Reg, carry: bool) -> Cycle {
    let mut regs = ctx.regs();
    let lhs = regs.reg(dst) as u16;
    let rhs = regs.reg(src) as u16;
    let c = if carry && regs.st.carry { 1 } else { 0 };
    let res = lhs + rhs + c;
    regs.set_reg(dst, res as u8);

    regs.st.carry = is_carry(res);
    regs.st.zero = is_zero(res);
    regs.st.neg = is_neg(res);
    regs.st.overflow = same_sign(lhs, rhs) && !same_sign(lhs, res);

    if dst == Reg::R0 && src.encode() < 4 { regs.pc += 1; 4 }
    else { regs.pc += 2; 7 }
}

fn is_neg(n: u16) -> bool { n & 0x0080 > 0 }
fn is_zero(n: u16) -> bool { n & 0x00ff == 0 }
fn is_carry(n: u16) -> bool { n > 0xff }
fn same_sign(a: u16, b: u16) -> bool { a & 0x0080 == b & 0x0080 }

#[cfg(test)]
mod test {

    use cpu::reg::Regs;
    use inst::{Inst, Reg};
    use mem::*;

    use super::*;

    #[test]
    fn should_exec_nop() {
        let mut ctx = TestCtx::new();
        exec(&Inst::Nop, &mut ctx);
        assert_eq!(ctx.regs.pc, 1);
    }

    #[test]
    fn should_exec_add() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 29);
        assert_eq!(ctx.regs.r1(), 7);
    }

    #[test]
    fn should_exec_add_ignoring_carry() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(1); ctx.regs.set_r1(1);
        ctx.regs.st.carry = true;
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 2);
    }

    #[test]
    fn should_compute_cycles_after_exec_add() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx), 4);
        assert_eq!(exec(&Inst::Add(Reg::R0, Reg::R5), &mut ctx), 7);
    }

    #[test]
    fn should_update_pc_after_exec_add() {
        let mut ctx = TestCtx::new();
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.pc, 1);
        exec(&Inst::Add(Reg::R0, Reg::R5), &mut ctx);
        assert_eq!(ctx.regs.pc, 3);
    }

    #[test]
    fn should_update_carry_after_exec_add() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.carry, false);
        ctx.regs.set_r0(22); ctx.regs.set_r1(250);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.carry, true);
    }

    #[test]
    fn should_update_zero_after_exec_add() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(5); ctx.regs.set_r1(-5i8 as u8);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_add() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(1); ctx.regs.set_r1(-5i8 as u8);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_update_overflow_after_exec_add() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_r0(120); ctx.regs.set_r1(120);
        exec(&Inst::Add(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
    }

    #[test]
    fn should_exec_adc() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Adc(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 29);
        assert_eq!(ctx.regs.r1(), 7);
    }

    #[test]
    fn should_exec_adc_honoring_carry() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(1); ctx.regs.set_r1(1);
        ctx.regs.st.carry = true;
        exec(&Inst::Adc(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 3);
    }

    struct TestCtx { mem: RamPage, regs: Regs, }

    impl TestCtx {
        fn new() -> Self { TestCtx { mem: RamPage::new(), regs: Regs::new() }}
    }

    impl ExecCtx for TestCtx {
        type Mem = RamPage;
        fn mem(&mut self) -> &mut RamPage { &mut self.mem }
        fn regs(&mut self) -> &mut Regs { &mut self.regs }
    }
}
