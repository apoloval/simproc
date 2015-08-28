//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use cpu::clock::Cycle;
use cpu::reg::Regs;
use inst::*;
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
        &Inst::Sub(dst, src) => exec_sub(ctx, dst, src, false),
        &Inst::Sbc(dst, src) => exec_sub(ctx, dst, src, true),
        &Inst::And(dst, src) => exec_binary_logic(ctx, dst, src, |a, b| a & b),
        &Inst::Or(dst, src) => exec_binary_logic(ctx, dst, src, |a, b| a | b),
        &Inst::Xor(dst, src) => exec_binary_logic(ctx, dst, src, |a, b| a ^ b),
        &Inst::Neg(dst) => exec_unary_logic(ctx, dst, |a| (-(a as i8)) as u8),
        &Inst::Com(dst) => exec_unary_logic(ctx, dst, |a| !a),
        &Inst::Inc(dst) => exec_inc(ctx, dst, true),
        &Inst::Dec(dst) => exec_inc(ctx, dst, false),
        &Inst::Incw(dst) => exec_incw(ctx, dst, true),
        &Inst::Decw(dst) => exec_incw(ctx, dst, false),
        &Inst::Lsl(dst) => exec_shift(ctx, dst, |a| a << 1, |a| a & 0x80 > 0),
        &Inst::Lsr(dst) => exec_shift(ctx, dst, |a| a >> 1, |a| a & 0x01 > 0),
        &Inst::Asr(dst) => exec_shift(ctx, dst, |a| (a >> 1) | (a & 0x80), |a| a & 0x01 > 0),
        &Inst::Mov(dst, src) => exec_mov(ctx, dst, src),
        &Inst::Ld(dst, src) => exec_ld(ctx, dst, src),
        &Inst::St(dst, src) => exec_st(ctx, dst, src),
        &Inst::Ldsp(src) => exec_ldsp(ctx, src),
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

fn exec_sub<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg, src: Reg, carry: bool) -> Cycle {
    let mut regs = ctx.regs();
    let lhs = regs.reg(dst) as i16;
    let rhs = regs.reg(src) as i16;
    let c = if carry && regs.st.carry { 1 } else { 0 };
    let res = (lhs - rhs - c) as u16;
    regs.set_reg(dst, res as u8);

    regs.st.carry = is_carry(res);
    regs.st.zero = is_zero(res);
    regs.st.neg = is_neg(res);
    regs.st.overflow = !same_sign(lhs as u16, rhs as u16) && !same_sign(lhs as u16, res);

    if dst == Reg::R0 && src.encode() < 4 { regs.pc += 1; 4 }
    else { regs.pc += 2; 7 }
}

fn exec_unary_logic<M: Memory, L: Fn(u8) -> u8>(
    ctx: &mut ExecCtx<Mem=M>, dst: Reg, logic: L) -> Cycle
{
    let mut regs = ctx.regs();
    let val = regs.reg(dst);
    let res = logic(val) as u16;
    regs.set_reg(dst, res as u8);

    regs.st.carry = false;
    regs.st.zero = is_zero(res);
    regs.st.neg = is_neg(res);
    regs.st.overflow = false;

    regs.pc += 1; 4
}

fn exec_binary_logic<M: Memory, L: Fn(u8, u8) -> u8>(
    ctx: &mut ExecCtx<Mem=M>, dst: Reg, src: Reg, logic: L) -> Cycle
{
    let mut regs = ctx.regs();
    let lhs = regs.reg(dst);
    let rhs = regs.reg(src);
    let res = logic(lhs, rhs) as u16;
    regs.set_reg(dst, res as u8);

    regs.st.carry = false;
    regs.st.zero = is_zero(res);
    regs.st.neg = is_neg(res);
    regs.st.overflow = false;

    regs.pc += 1; 4
}

fn exec_inc<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg, positive: bool) -> Cycle {
    let mut regs = ctx.regs();
    let val = regs.reg(dst) as i16;
    let res = val + if positive { 1 } else { -1 };
    regs.set_reg(dst, res as u8);

    regs.st.carry = false;
    regs.st.zero = is_zero(res as u16);
    regs.st.neg = is_neg(res as u16);
    regs.st.overflow = val == if positive { 0x7f } else { 0x80 };

    regs.pc += 1; 4
}

fn exec_incw<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: AddrReg, positive: bool) -> Cycle {
    let mut regs = ctx.regs();
    let val = regs.areg(dst) as i32;
    let res = val + if positive { 1 } else { -1 };
    regs.set_areg(dst, res as u16);

    regs.st.carry = false;
    regs.st.zero = is_zero(res as u16);
    regs.st.neg = is_neg(res as u16);
    regs.st.overflow = (val as u16) == if positive { 0x7fff } else { 0x8000 };

    regs.pc += 1; 7
}

fn exec_shift<M: Memory, F: Fn(u8) -> u8, G: Fn(u8) -> bool>(
    ctx: &mut ExecCtx<Mem=M>, dst: Reg, f: F, g: G) -> Cycle
{
    let mut regs = ctx.regs();
    let val = regs.reg(dst);
    let res = f(val);
    regs.set_reg(dst, res);

    regs.st.carry = g(val);
    regs.st.zero = is_zero(res as u16);
    regs.st.neg = is_neg(res as u16);
    regs.st.overflow = regs.st.neg ^ regs.st.carry;

    regs.pc += 2; 4
}

fn exec_mov<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg, src: Reg) -> Cycle {
    let mut regs = ctx.regs();
    let from = regs.reg(src);
    regs.set_reg(dst, from);
    if src == Reg::R0 && dst.encode() < 4 { regs.pc += 1; 4 }
    else { regs.pc += 2; 7 }
}

fn exec_ld<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg, src: AddrReg) -> Cycle {
    let addr = ctx.regs().areg(src);
    let val = ctx.mem().read(addr);
    ctx.regs().set_reg(dst, val);
    if dst == Reg::R0 { ctx.regs().pc += 1; 7 }
    else { ctx.regs().pc += 2; 10 }
}

fn exec_st<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: AddrReg, src: Reg) -> Cycle {
    let addr = ctx.regs().areg(dst);
    let val = ctx.regs().reg(src);
    ctx.mem().write(addr, val);
    if src == Reg::R0 { ctx.regs().pc += 1; 7 }
    else { ctx.regs().pc += 2; 10 }
}

fn exec_ldsp<M: Memory>(ctx: &mut ExecCtx<Mem=M>, src: AddrReg) -> Cycle {
    let regs = ctx.regs();
    let addr = regs.areg(src);
    regs.sp = addr;
    regs.pc += 1; 4
}

fn is_neg(n: u16) -> bool { n & 0x0080 > 0 }
fn is_zero(n: u16) -> bool { n & 0x00ff == 0 }
fn is_carry(n: u16) -> bool { n > 0xff }
fn same_sign(a: u16, b: u16) -> bool { a & 0x0080 == b & 0x0080 }

#[cfg(test)]
mod test {

    use cpu::reg::Regs;
    use inst::{AddrReg, Inst, Reg};
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

    #[test]
    fn should_exec_sub() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 15);
        assert_eq!(ctx.regs.r1(), 7);
    }

    #[test]
    fn should_exec_sub_ignoring_carry() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(5); ctx.regs.set_r1(3);
        ctx.regs.st.carry = true;
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 2);
    }

    #[test]
    fn should_compute_cycles_after_exec_sub() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx), 4);
        assert_eq!(exec(&Inst::Sub(Reg::R0, Reg::R5), &mut ctx), 7);
    }

    #[test]
    fn should_update_pc_after_exec_sub() {
        let mut ctx = TestCtx::new();
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.pc, 1);
        exec(&Inst::Sub(Reg::R0, Reg::R5), &mut ctx);
        assert_eq!(ctx.regs.pc, 3);
    }

    #[test]
    fn should_update_carry_after_exec_sub() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.carry, false);
        ctx.regs.set_r0(-100i8 as u8); ctx.regs.set_r1(250);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.carry, true);
    }

    #[test]
    fn should_update_zero_after_exec_sub() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(5); ctx.regs.set_r1(5);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_sub() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(1); ctx.regs.set_r1(5);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_update_overflow_after_exec_sub() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_r0(120); ctx.regs.set_r1(-120i8 as u8);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
        ctx.regs.set_r0(-120i8 as u8); ctx.regs.set_r1(120);
        exec(&Inst::Sub(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
    }

    #[test]
    fn should_exec_sbc() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22); ctx.regs.set_r1(7);
        exec(&Inst::Sbc(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 15);
        assert_eq!(ctx.regs.r1(), 7);
    }

    #[test]
    fn should_exec_sbc_honoring_carry() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(5); ctx.regs.set_r1(3);
        ctx.regs.st.carry = true;
        exec(&Inst::Sbc(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 1);
    }

    #[test]
    fn should_exec_and() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0x0f);
        exec(&Inst::And(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 0x0f);
        assert_eq!(ctx.regs.r1(), 0x0f);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
        assert_eq!(ctx.regs.st.overflow, false);
    }

    #[test]
    fn should_update_zero_after_exec_and() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0x0f);
        exec(&Inst::And(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0x00);
        exec(&Inst::And(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_and() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0x0f);
        exec(&Inst::And(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0xf0);
        exec(&Inst::And(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_exec_or() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0); ctx.regs.set_r1(0x0f);
        exec(&Inst::Or(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 0xff);
        assert_eq!(ctx.regs.r1(), 0x0f);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
        assert_eq!(ctx.regs.st.overflow, false);
    }

    #[test]
    fn should_update_zero_after_exec_or() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0x0f);
        exec(&Inst::Or(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0x00); ctx.regs.set_r1(0x00);
        exec(&Inst::Or(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_or() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x0f); ctx.regs.set_r1(0x0f);
        exec(&Inst::Or(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0xf0);
        exec(&Inst::Or(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_exec_xor() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0); ctx.regs.set_r1(0x0f);
        exec(&Inst::Xor(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 0xff);
        assert_eq!(ctx.regs.r1(), 0x0f);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
        assert_eq!(ctx.regs.st.overflow, false);
    }

    #[test]
    fn should_update_zero_after_exec_xor() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0x0f);
        exec(&Inst::Xor(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0xff);
        exec(&Inst::Xor(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_xor() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x00); ctx.regs.set_r1(0x0f);
        exec(&Inst::Xor(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(0xff); ctx.regs.set_r1(0x0f);
        exec(&Inst::Xor(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_exec_neg() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0);
        exec(&Inst::Neg(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.r0(), 0x10);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
        assert_eq!(ctx.regs.st.overflow, false);
    }

    #[test]
    fn should_update_zero_after_exec_neg() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0);
        exec(&Inst::Neg(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0x00);
        exec(&Inst::Neg(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_neg() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0);
        exec(&Inst::Neg(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(0x0f);
        exec(&Inst::Neg(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_exec_com() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0);
        exec(&Inst::Com(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.r0(), 0x0f);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
        assert_eq!(ctx.regs.st.overflow, false);
    }

    #[test]
    fn should_update_zero_after_exec_com() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0);
        exec(&Inst::Com(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0xff);
        exec(&Inst::Com(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_com() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0);
        exec(&Inst::Com(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(0x0f);
        exec(&Inst::Com(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_exec_inc() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22);
        assert_eq!(exec(&Inst::Inc(Reg::R0), &mut ctx), 4);
        assert_eq!(ctx.regs.r0(), 23);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
    }

    #[test]
    fn should_update_zero_after_exec_inc() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22);
        exec(&Inst::Inc(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(-1i8 as u8);
        exec(&Inst::Inc(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_inc() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22);
        exec(&Inst::Inc(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(-22i8 as u8);
        exec(&Inst::Inc(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_update_overflow_after_exec_inc() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22);
        exec(&Inst::Inc(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_r0(0x7f);
        exec(&Inst::Inc(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
    }

    #[test]
    fn should_exec_dec() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22);
        assert_eq!(exec(&Inst::Dec(Reg::R0), &mut ctx), 4);
        assert_eq!(ctx.regs.r0(), 21);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
    }

    #[test]
    fn should_update_zero_after_exec_dec() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22);
        exec(&Inst::Dec(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(1);
        exec(&Inst::Dec(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_dec() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22);
        exec(&Inst::Dec(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(-22i8 as u8);
        exec(&Inst::Dec(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_update_overflow_after_exec_dec() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(22);
        exec(&Inst::Dec(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_r0(0x80);
        exec(&Inst::Dec(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
    }

    #[test]
    fn should_exec_incw() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(22);
        assert_eq!(exec(&Inst::Incw(AddrReg::A0), &mut ctx), 7);
        assert_eq!(ctx.regs.a0(), 23);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
    }

    #[test]
    fn should_update_zero_after_exec_incw() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(22);
        exec(&Inst::Incw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_a0(-1i8 as u16);
        exec(&Inst::Incw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_incw() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(22);
        exec(&Inst::Incw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_a0(-22i8 as u16);
        exec(&Inst::Incw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_update_overflow_after_exec_incw() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(22);
        exec(&Inst::Incw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_a0(0x7fff);
        exec(&Inst::Incw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
    }

    #[test]
    fn should_exec_decw() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(22);
        assert_eq!(exec(&Inst::Decw(AddrReg::A0), &mut ctx), 7);
        assert_eq!(ctx.regs.a0(), 21);
        assert_eq!(ctx.regs.pc, 1);
        assert_eq!(ctx.regs.st.carry, false);
    }

    #[test]
    fn should_update_zero_after_exec_decw() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(22);
        exec(&Inst::Decw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_a0(1);
        exec(&Inst::Decw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_decw() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(22);
        exec(&Inst::Decw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_a0(-22i8 as u16);
        exec(&Inst::Decw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_update_overflow_after_exec_decw() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(22);
        exec(&Inst::Decw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_a0(0x8000);
        exec(&Inst::Decw(AddrReg::A0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
    }

    #[test]
    fn should_exec_lsl() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x0f);
        assert_eq!(exec(&Inst::Lsl(Reg::R0), &mut ctx), 4);
        assert_eq!(ctx.regs.r0(), 0x1e);
        assert_eq!(ctx.regs.pc, 2);
    }

    #[test]
    fn should_update_carry_after_exec_lsl() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x40);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0x80);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_zero_after_exec_lsl() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x0f);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0x80);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_lsl() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x0f);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(0x40);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_update_overflow_after_exec_lsl() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x01);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_r0(0xc0);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_r0(0x40);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
        ctx.regs.set_r0(0x80);
        exec(&Inst::Lsl(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
    }

    #[test]
    fn should_exec_lsr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0);
        assert_eq!(exec(&Inst::Lsr(Reg::R0), &mut ctx), 4);
        assert_eq!(ctx.regs.r0(), 0x78);
        assert_eq!(ctx.regs.st.neg, false);
        assert_eq!(ctx.regs.pc, 2);
    }

    #[test]
    fn should_update_carry_after_exec_lsr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x40);
        exec(&Inst::Lsr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.carry, false);
        ctx.regs.set_r0(0x41);
        exec(&Inst::Lsr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.carry, true);
    }

    #[test]
    fn should_update_zero_after_exec_lsr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x0f);
        exec(&Inst::Lsr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0x01);
        exec(&Inst::Lsr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_overflow_after_exec_lsr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x02);
        exec(&Inst::Lsr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_r0(0x01);
        exec(&Inst::Lsr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
    }

    #[test]
    fn should_exec_asr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0xf0);
        assert_eq!(exec(&Inst::Asr(Reg::R0), &mut ctx), 4);
        assert_eq!(ctx.regs.r0(), 0xf8);
        assert_eq!(ctx.regs.pc, 2);
    }

    #[test]
    fn should_update_carry_after_exec_asr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x40);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.carry, false);
        ctx.regs.set_r0(0x41);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.carry, true);
    }

    #[test]
    fn should_update_zero_after_exec_asr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x81);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, false);
        ctx.regs.set_r0(0x01);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.zero, true);
    }

    #[test]
    fn should_update_neg_after_exec_asr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x01);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, false);
        ctx.regs.set_r0(0x81);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.neg, true);
    }

    #[test]
    fn should_update_overflow_after_exec_asr() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(0x02);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
        ctx.regs.set_r0(0x01);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
        ctx.regs.set_r0(0x82);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, true);
        ctx.regs.set_r0(0x81);
        exec(&Inst::Asr(Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.st.overflow, false);
    }

    #[test]
    fn should_exec_mov() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r1(42);
        exec(&Inst::Mov(Reg::R0, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 42);
    }

    #[test]
    fn should_update_pc_after_exec_mov() {
        let mut ctx = TestCtx::new();
        exec(&Inst::Mov(Reg::R1, Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.pc, 1);
        exec(&Inst::Mov(Reg::R1, Reg::R2), &mut ctx);
        assert_eq!(ctx.regs.pc, 3);
    }

    #[test]
    fn should_compute_cycles_after_exec_mov() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Mov(Reg::R1, Reg::R0), &mut ctx), 4);
        assert_eq!(exec(&Inst::Mov(Reg::R4, Reg::R0), &mut ctx), 7);
        assert_eq!(exec(&Inst::Mov(Reg::R0, Reg::R1), &mut ctx), 7);
    }

    #[test]
    fn should_exec_ld() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a1(0x1000);
        ctx.mem().write(0x1000, 42);
        exec(&Inst::Ld(Reg::R0, AddrReg::A1), &mut ctx);
        assert_eq!(ctx.regs.r0(), 42);
    }

    #[test]
    fn should_update_pc_after_exec_ld() {
        let mut ctx = TestCtx::new();
        exec(&Inst::Ld(Reg::R0, AddrReg::A1), &mut ctx);
        assert_eq!(ctx.regs.pc, 1);
        exec(&Inst::Ld(Reg::R1, AddrReg::A1), &mut ctx);
        assert_eq!(ctx.regs.pc, 3);
    }

    #[test]
    fn should_compute_cycles_after_exec_ld() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Ld(Reg::R0, AddrReg::A1), &mut ctx), 7);
        assert_eq!(exec(&Inst::Ld(Reg::R1, AddrReg::A1), &mut ctx), 10);
    }

    #[test]
    fn should_exec_st() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a1(0x1000);
        ctx.regs.set_r0(42);
        exec(&Inst::St(AddrReg::A1, Reg::R0), &mut ctx);
        assert_eq!(ctx.mem.read(0x1000), 42);
    }

    #[test]
    fn should_update_pc_after_exec_st() {
        let mut ctx = TestCtx::new();
        exec(&Inst::St(AddrReg::A1, Reg::R0), &mut ctx);
        assert_eq!(ctx.regs.pc, 1);
        exec(&Inst::St(AddrReg::A1, Reg::R1), &mut ctx);
        assert_eq!(ctx.regs.pc, 3);
    }

    #[test]
    fn should_compute_cycles_after_exec_st() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::St(AddrReg::A1, Reg::R0), &mut ctx), 7);
        assert_eq!(exec(&Inst::St(AddrReg::A1, Reg::R1), &mut ctx), 10);
    }

    #[test]
    fn should_exec_ldsp() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a1(0x1000);
        assert_eq!(exec(&Inst::Ldsp(AddrReg::A1), &mut ctx), 4);
        assert_eq!(ctx.regs.sp, 0x1000);
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
