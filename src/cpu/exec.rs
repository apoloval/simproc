//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use cpu::clock::Cycle;
use cpu::io::*;
use cpu::reg::{Regs, StatusReg};
use inst::*;
use mem::{Addr, Memory, RelAddr};

pub trait ExecCtx<'a> {
    type Mem: Memory;
    fn mem(&mut self) -> &mut Self::Mem;
    fn regs(&mut self) -> &mut Regs;
    fn io(&mut self) -> &mut Io<'a>;
}

/// Execute the given instruction over the given context
/// It returns the number of cycles that correspond to that instruction execution.
pub fn exec<M: Memory>(inst: &RuntimeInst, ctx: &mut ExecCtx<Mem=M>) -> Cycle {
    match inst {
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
        &Inst::Ldd(dst, addr) => exec_ldd(ctx, dst, addr),
        &Inst::Ldi(dst, Immediate(val)) => exec_ldi(ctx, dst, val),
        &Inst::Ldw(dst, addr) => exec_ldw(ctx, dst, addr),
        &Inst::St(dst, src) => exec_st(ctx, dst, src),
        &Inst::Std(dst, src) => exec_std(ctx, dst, src),
        &Inst::Ldsp(src) => exec_ldsp(ctx, src),
        &Inst::Push(src) => exec_push(ctx, src),
        &Inst::Pop(dst) => exec_pop(ctx, dst),
        &Inst::In(dst, port) => exec_in(ctx, dst, port),
        &Inst::Out(port, src) => exec_out(ctx, port, src),
        &Inst::Je(offset) => exec_rjmp(ctx, offset, |st| !st.zero),
        &Inst::Jne(offset) => exec_rjmp(ctx, offset, |st| st.zero),
        &Inst::Jl(offset) => exec_rjmp(ctx, offset, |st| !st.neg),
        &Inst::Jge(offset) => exec_rjmp(ctx, offset, |st| st.neg),
        &Inst::Jcc(offset) => exec_rjmp(ctx, offset, |st| !st.carry),
        &Inst::Jcs(offset) => exec_rjmp(ctx, offset, |st| st.carry),
        &Inst::Jvc(offset) => exec_rjmp(ctx, offset, |st| !st.overflow),
        &Inst::Jvs(offset) => exec_rjmp(ctx, offset, |st| st.overflow),
        &Inst::Rjmp(offset) => exec_rjmp(ctx, offset, |_| true),
        &Inst::Jmp(addr) => exec_jmp(ctx, addr, false),
        &Inst::Call(addr) => exec_jmp(ctx, addr, true),
        &Inst::Ijmp(src) => exec_ijmp(ctx, src, false),
        &Inst::Icall(src) => exec_ijmp(ctx, src, true),
        &Inst::Ret => exec_ret(ctx, false),
        &Inst::Reti => exec_ret(ctx, true),
        &Inst::Nop => exec_nop(ctx),
        &Inst::Halt => exec_halt(),
        &Inst::Ei => exec_set_int(ctx, true),
        &Inst::Di => exec_set_int(ctx, false),
        _ => unimplemented!(),
    }
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

    regs.pc += 2; 4
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

fn exec_ldd<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg, src: Addr) -> Cycle {
    let val = ctx.mem().read(src);
    ctx.regs().set_reg(dst, val);
    ctx.regs().pc += 2; 10
}

fn exec_ldi<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg, val: u8) -> Cycle {
    ctx.regs().set_reg(dst, val);
    ctx.regs().pc += 2; 7
}

fn exec_ldw<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: AddrReg, addr: Addr) -> Cycle {
    ctx.regs().set_areg(dst, addr);
    ctx.regs().pc += 3; 10
}

fn exec_st<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: AddrReg, src: Reg) -> Cycle {
    let addr = ctx.regs().areg(dst);
    let val = ctx.regs().reg(src);
    ctx.mem().write(addr, val);
    if src == Reg::R0 { ctx.regs().pc += 1; 7 }
    else { ctx.regs().pc += 2; 10 }
}

fn exec_std<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Addr, src: Reg) -> Cycle {
    let val = ctx.regs().reg(src);
    ctx.mem().write(dst, val);
    ctx.regs().pc += 2; 10
}

fn exec_ldsp<M: Memory>(ctx: &mut ExecCtx<Mem=M>, src: AddrReg) -> Cycle {
    let regs = ctx.regs();
    let addr = regs.areg(src);
    regs.sp = addr;
    regs.pc += 1; 4
}

fn exec_push<M: Memory>(ctx: &mut ExecCtx<Mem=M>, src: Reg) -> Cycle {
    let val = ctx.regs().reg(src);
    let addr = (ctx.regs().sp as i32 - 1) as u16;
    ctx.mem().write(addr, val);
    ctx.regs().sp = addr;
    ctx.regs().pc += 1; 7
}

fn exec_pop<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg) -> Cycle {
    let addr = ctx.regs().sp;
    let val = ctx.mem().read(addr);
    ctx.regs().set_reg(dst, val);
    ctx.regs().sp = (addr as i32 + 1) as u16;
    ctx.regs().pc += 1; 7
}

fn exec_in<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: Reg, port: IoPort) -> Cycle {
    let IoPort(p) = port;
    let val = ctx.io().read(p);
    ctx.regs().set_reg(dst, val);
    ctx.regs().pc += 2; 7
}

fn exec_out<M: Memory>(ctx: &mut ExecCtx<Mem=M>, port: IoPort, src: Reg) -> Cycle {
    let IoPort(p) = port;
    let val = ctx.regs().reg(src);
    ctx.io().write(p, val);
    ctx.regs().pc += 2; 7
}

fn exec_rjmp<M: Memory, F: Fn(&StatusReg) -> bool>(
    ctx: &mut ExecCtx<Mem=M>, offset: RelAddr, f: F) -> Cycle
{
    let inc = if f(&ctx.regs().st) { offset as i32 } else { 2 };
    let pc = ctx.regs().pc as i32;
    ctx.regs().pc = (pc + inc) as u16; 7
}

fn exec_jmp<M: Memory>(ctx: &mut ExecCtx<Mem=M>, addr: Addr, is_call: bool) -> Cycle {
    let cont = add(ctx.regs().pc, 3);
    ctx.regs().pc = addr;
    if is_call {
        let sp = add(ctx.regs().sp, -2);
        ctx.mem().write(sp, cont as u8);
        ctx.mem().write(sp + 1, (cont >> 8) as u8);
        ctx.regs().sp = sp;
        16
    } else { 10 }
}

fn exec_ijmp<M: Memory>(ctx: &mut ExecCtx<Mem=M>, src: AddrReg, is_call: bool) -> Cycle {
    let addr = ctx.regs().areg(src);
    exec_jmp(ctx, addr, is_call) - 6
}

fn exec_ret<M: Memory>(ctx: &mut ExecCtx<Mem=M>, enable_int: bool) -> Cycle {
    let sp = ctx.regs().sp;
    let al = ctx.mem().read(sp) as u16;
    let ah = (ctx.mem().read(add(sp, 1)) as u16) << 8;
    let addr = al | ah;
    ctx.regs().pc = addr;
    ctx.regs().sp = add(sp, 2);
    if enable_int { ctx.regs().st.int = true; }
    10
}

fn exec_nop<M: Memory>(ctx: &mut ExecCtx<Mem=M>) -> Cycle {
    ctx.regs().pc += 1; 4
}

fn exec_halt() -> Cycle { 4 }

fn exec_set_int<M: Memory>(ctx: &mut ExecCtx<Mem=M>, enabled: bool) -> Cycle {
    ctx.regs().st.int = enabled;
    ctx.regs().pc += 1; 4
}

fn is_neg(n: u16) -> bool { n & 0x0080 > 0 }
fn is_zero(n: u16) -> bool { n & 0x00ff == 0 }
fn is_carry(n: u16) -> bool { n > 0xff }
fn same_sign(a: u16, b: u16) -> bool { a & 0x0080 == b & 0x0080 }
fn add(n: u16, inc: i16) -> u16 { (n as i32 + inc as i32) as u16 }

#[cfg(test)]
mod test {

    use cpu::io::*;
    use cpu::reg::Regs;
    use inst::*;
    use mem::*;

    use super::*;

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
        assert_eq!(ctx.regs.pc, 2);
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
        assert_eq!(ctx.regs.pc, 2);
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
        assert_eq!(ctx.regs.pc, 2);
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
    fn should_exec_ldd() {
        let mut ctx = TestCtx::new();
        ctx.mem().write(0x1000, 42);
        assert_eq!(exec(&Inst::Ldd(Reg::R0, 0x1000), &mut ctx), 10);
        assert_eq!(ctx.regs.r0(), 42);
        assert_eq!(ctx.regs.pc, 2);
    }

    #[test]
    fn should_exec_ldi() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Ldi(Reg::R0, Immediate(42)), &mut ctx), 7);
        assert_eq!(ctx.regs.r0(), 42);
        assert_eq!(ctx.regs.pc, 2);
    }

    #[test]
    fn should_exec_ldw() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Ldw(AddrReg::A0, 0x1000), &mut ctx), 10);
        assert_eq!(ctx.regs.a0(), 0x1000);
        assert_eq!(ctx.regs.pc, 3);
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
    fn should_exec_std() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_r0(42);
        assert_eq!(exec(&Inst::Std(0x1000, Reg::R0), &mut ctx), 10);
        assert_eq!(ctx.mem().read(0x1000), 42);
        assert_eq!(ctx.regs.pc, 2);
    }

    #[test]
    fn should_exec_ldsp() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a1(0x1000);
        assert_eq!(exec(&Inst::Ldsp(AddrReg::A1), &mut ctx), 4);
        assert_eq!(ctx.regs.sp, 0x1000);
    }

    #[test]
    fn should_exec_push() {
        let mut ctx = TestCtx::new();
        ctx.regs.sp = 0x1001;
        ctx.regs.set_r0(42);
        assert_eq!(exec(&Inst::Push(Reg::R0), &mut ctx), 7);
        assert_eq!(ctx.regs.sp, 0x1000);
        assert_eq!(ctx.mem().read(0x1000), 42);
        assert_eq!(ctx.regs.pc, 1);
    }

    #[test]
    fn should_exec_pop() {
        let mut ctx = TestCtx::new();
        ctx.regs.sp = 0x1000;
        ctx.mem().write(0x1000, 42);
        assert_eq!(exec(&Inst::Pop(Reg::R0), &mut ctx), 7);
        assert_eq!(ctx.regs.sp, 0x1001);
        assert_eq!(ctx.regs.r0(), 42);
        assert_eq!(ctx.regs.pc, 1);
    }

    #[test]
    fn should_exec_in() {
        let mut val = 42;
        let mut ctx = TestCtx::new();
        ctx.bind_io(0x10, &mut val);
        assert_eq!(exec(&Inst::In(Reg::R0, IoPort(0x10)), &mut ctx), 7);
        assert_eq!(ctx.regs.r0(), 42);
        assert_eq!(ctx.regs.pc, 2);
    }

    #[test]
    fn should_exec_out() {
        let mut val = 0;
        {
            let mut ctx = TestCtx::new();
            ctx.bind_io(0x10, &mut val);
            ctx.regs.set_r0(42);
            assert_eq!(exec(&Inst::Out(IoPort(0x10), Reg::R0), &mut ctx), 7);
            assert_eq!(ctx.regs.pc, 2);
        }
        assert_eq!(val, 42);
    }

    #[test]
    fn should_exec_je() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.zero = true;
        assert_eq!(exec(&Inst::Je(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 2);
        ctx.regs.st.zero = false;
        assert_eq!(exec(&Inst::Je(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 102);
    }

    #[test]
    fn should_exec_jne() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.zero = false;
        assert_eq!(exec(&Inst::Jne(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 2);
        ctx.regs.st.zero = true;
        assert_eq!(exec(&Inst::Jne(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 102);
    }

    #[test]
    fn should_exec_jl() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.neg = true;
        assert_eq!(exec(&Inst::Jl(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 2);
        ctx.regs.st.neg = false;
        assert_eq!(exec(&Inst::Jl(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 102);
    }

    #[test]
    fn should_exec_jge() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.neg = false;
        assert_eq!(exec(&Inst::Jge(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 2);
        ctx.regs.st.neg = true;
        assert_eq!(exec(&Inst::Jge(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 102);
    }

    #[test]
    fn should_exec_jcc() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.carry = true;
        assert_eq!(exec(&Inst::Jcc(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 2);
        ctx.regs.st.carry = false;
        assert_eq!(exec(&Inst::Jcc(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 102);
    }

    #[test]
    fn should_exec_jcs() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.carry = false;
        assert_eq!(exec(&Inst::Jcs(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 2);
        ctx.regs.st.carry = true;
        assert_eq!(exec(&Inst::Jcs(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 102);
    }

    #[test]
    fn should_exec_jvc() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.overflow = true;
        assert_eq!(exec(&Inst::Jvc(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 2);
        ctx.regs.st.overflow = false;
        assert_eq!(exec(&Inst::Jvc(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 102);
    }

    #[test]
    fn should_exec_jvs() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.overflow = false;
        assert_eq!(exec(&Inst::Jvs(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 2);
        ctx.regs.st.overflow = true;
        assert_eq!(exec(&Inst::Jvs(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 102);
    }

    #[test]
    fn should_exec_rjmp() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Rjmp(100), &mut ctx), 7);
        assert_eq!(ctx.regs.pc, 100);
    }

    #[test]
    fn should_exec_jmp() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Jmp(0x1000), &mut ctx), 10);
        assert_eq!(ctx.regs.pc, 0x1000);
    }

    #[test]
    fn should_exec_call() {
        let mut ctx = TestCtx::new();
        ctx.regs.sp = 0x1002;
        ctx.regs.pc = 0x5060;
        assert_eq!(exec(&Inst::Call(0x1000), &mut ctx), 16);
        assert_eq!(ctx.regs.pc, 0x1000);
        assert_eq!(ctx.mem().read(0x1000), 0x63);
        assert_eq!(ctx.mem().read(0x1001), 0x50);
    }

    #[test]
    fn should_exec_ijmp() {
        let mut ctx = TestCtx::new();
        ctx.regs.set_a0(0x1000);
        assert_eq!(exec(&Inst::Ijmp(AddrReg::A0), &mut ctx), 4);
        assert_eq!(ctx.regs.pc, 0x1000);
    }

    #[test]
    fn should_exec_icall() {
        let mut ctx = TestCtx::new();
        ctx.regs.sp = 0x1002;
        ctx.regs.pc = 0x5060;
        ctx.regs.set_a0(0x1000);
        assert_eq!(exec(&Inst::Icall(AddrReg::A0), &mut ctx), 10);
        assert_eq!(ctx.regs.pc, 0x1000);
        assert_eq!(ctx.mem().read(0x1000), 0x63);
        assert_eq!(ctx.mem().read(0x1001), 0x50);
    }

    #[test]
    fn should_exec_ret() {
        let mut ctx = TestCtx::new();
        ctx.mem().write(0x1000, 0x60);
        ctx.mem().write(0x1001, 0x50);
        ctx.regs.sp = 0x1000;
        assert_eq!(exec(&Inst::Ret, &mut ctx), 10);
        assert_eq!(ctx.regs.pc, 0x5060);
    }

    #[test]
    fn should_exec_reti() {
        let mut ctx = TestCtx::new();
        ctx.mem().write(0x1000, 0x60);
        ctx.mem().write(0x1001, 0x50);
        ctx.regs.sp = 0x1000;
        ctx.regs.st.int = false;
        assert_eq!(exec(&Inst::Reti, &mut ctx), 10);
        assert_eq!(ctx.regs.pc, 0x5060);
        assert_eq!(ctx.regs.st.int, true);
    }

    #[test]
    fn should_exec_nop() {
        let mut ctx = TestCtx::new();
        exec(&Inst::Nop, &mut ctx);
        assert_eq!(ctx.regs.pc, 1);
    }

    #[test]
    fn should_exec_halt() {
        let mut ctx = TestCtx::new();
        assert_eq!(exec(&Inst::Halt, &mut ctx), 4);
        assert_eq!(ctx.regs.pc, 0);
    }

    #[test]
    fn should_exec_ei() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.int = false;
        assert_eq!(exec(&Inst::Ei, &mut ctx), 4);
        assert_eq!(ctx.regs.st.int, true);
    }

    #[test]
    fn should_exec_di() {
        let mut ctx = TestCtx::new();
        ctx.regs.st.int = true;
        assert_eq!(exec(&Inst::Di, &mut ctx), 4);
        assert_eq!(ctx.regs.st.int, false);
    }

    struct TestCtx<'a> { mem: RamPage, regs: Regs, io: Io<'a> }

    impl<'a> TestCtx<'a> {
        fn new() -> Self { TestCtx { mem: RamPage::new(), regs: Regs::new(), io: Io::new() }}
        fn bind_io(&mut self, port: u8, val: &'a mut u8) {
            let dev = TestDev { val: val };
            self.io.attach(port, dev);
        }
    }

    impl<'a> ExecCtx<'a> for TestCtx<'a> {
        type Mem = RamPage;
        fn mem(&mut self) -> &mut RamPage { &mut self.mem }
        fn regs(&mut self) -> &mut Regs { &mut self.regs }
        fn io(&mut self) -> &mut Io<'a> { &mut self.io }
    }

    struct TestDev<'a> { val: &'a mut u8 }

    impl<'a> IoDevice for TestDev<'a> {
        fn read(&mut self) -> u8 { *self.val }
        fn write(&mut self, val: u8) { *self.val = val }
    }
}
