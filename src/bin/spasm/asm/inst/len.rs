//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use simproc::inst::*;

use asm::inst::*;

pub fn inst_len(inst: &PreAssembledInst) -> usize {
    match inst {
        &Inst::Add(Expr::Reg(Reg::R0), Expr::Reg(src)) if src.encode() < 4 => 1,
        &Inst::Add(_, _) => 2,
        &Inst::Adc(Expr::Reg(Reg::R0), Expr::Reg(src)) if src.encode() < 4 => 1,
        &Inst::Adc(_, _) => 2,
        &Inst::Addi(_, _) => 2,
        &Inst::Sub(Expr::Reg(Reg::R0), Expr::Reg(src)) if src.encode() < 4 => 1,
        &Inst::Sub(_, _) => 2,
        &Inst::Sbc(Expr::Reg(Reg::R0), Expr::Reg(src)) if src.encode() < 4 => 1,
        &Inst::Sbc(_, _) => 2,
        &Inst::Subi(_, _) => 2,
        &Inst::And(Expr::Reg(Reg::R0), Expr::Reg(src)) if src.encode() < 4 => 1,
        &Inst::And(_, _) => 2,
        &Inst::Or(Expr::Reg(Reg::R0), Expr::Reg(src)) if src.encode() < 4 => 1,
        &Inst::Or(_, _) => 2,
        &Inst::Xor(Expr::Reg(Reg::R0), Expr::Reg(src)) if src.encode() < 4 => 1,
        &Inst::Xor(_, _) => 2,
        &Inst::Lsl(_) => 2,
        &Inst::Lsr(_) => 2,
        &Inst::Asr(_) => 2,
        &Inst::Neg(_) => 1,
        &Inst::Com(_) => 1,
        &Inst::Inc(_) => 1,
        &Inst::Incw(_) => 1,
        &Inst::Dec(_) => 1,
        &Inst::Decw(_) => 1,
        &Inst::Mov(Expr::Reg(dst), Expr::Reg(Reg::R0)) if dst.encode() < 4 => 1,
        &Inst::Mov(_, _) => 2,
        &Inst::Ld(Expr::Reg(Reg::R0), _) => 1,
        &Inst::Ld(_, _) => 2,
        &Inst::St(_, Expr::Reg(Reg::R0)) => 1,
        &Inst::St(_, _) => 2,
        &Inst::Ldd(_, _) => 3,
        &Inst::Std(_, _) => 3,
        &Inst::Ldi(_, _) => 2,
        &Inst::Ldw(_, _) => 3,
        &Inst::Ldsp(_) => 1,
        &Inst::Push(_) => 1,
        &Inst::In(_, _) => 2,
        &Inst::Out(_, _) => 2,
        &Inst::Pop(_) => 1,
        &Inst::Jnz(_) => 2,
        &Inst::Jz(_) => 2,
        &Inst::Jp(_) => 2,
        &Inst::Jn(_) => 2,
        &Inst::Jcc(_) => 2,
        &Inst::Jcs(_) => 2,
        &Inst::Jvc(_) => 2,
        &Inst::Jvs(_) => 2,
        &Inst::Jmp(_) => 3,
        &Inst::Rjmp(_) => 2,
        &Inst::Ijmp(_) => 1,
        &Inst::Call(_) => 3,
        &Inst::Rcall(_) => 2,
        &Inst::Icall(_) => 1,
        &Inst::Ret => 1,
        &Inst::Reti => 1,
        &Inst::Nop => 1,
        &Inst::Halt => 1,
        &Inst::Ei => 1,
        &Inst::Di => 1,
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use super::*;

    #[test]
    fn should_calculate_add_len() {
        assert_eq!(inst_len(&Inst::Add(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))), 1);
        assert_eq!(inst_len(&Inst::Add(Expr::Reg(Reg::R0), Expr::Reg(Reg::R4))), 2);
        assert_eq!(inst_len(&Inst::Add(Expr::Reg(Reg::R1), Expr::Reg(Reg::R1))), 2);
    }

    #[test]
    fn should_calculate_adc_len() {
        assert_eq!(inst_len(&Inst::Adc(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))), 1);
        assert_eq!(inst_len(&Inst::Adc(Expr::Reg(Reg::R0), Expr::Reg(Reg::R4))), 2);
        assert_eq!(inst_len(&Inst::Adc(Expr::Reg(Reg::R1), Expr::Reg(Reg::R1))), 2);
    }

    #[test]
    fn should_calculate_sub_len() {
        assert_eq!(inst_len(&Inst::Sub(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))), 1);
        assert_eq!(inst_len(&Inst::Sub(Expr::Reg(Reg::R0), Expr::Reg(Reg::R4))), 2);
        assert_eq!(inst_len(&Inst::Sub(Expr::Reg(Reg::R1), Expr::Reg(Reg::R1))), 2);
    }

    #[test]
    fn should_calculate_sbc_len() {
        assert_eq!(inst_len(&Inst::Sbc(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))), 1);
        assert_eq!(inst_len(&Inst::Sbc(Expr::Reg(Reg::R0), Expr::Reg(Reg::R4))), 2);
        assert_eq!(inst_len(&Inst::Sbc(Expr::Reg(Reg::R1), Expr::Reg(Reg::R1))), 2);
    }

    #[test]
    fn should_calculate_and_len() {
        assert_eq!(inst_len(&Inst::And(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))), 1);
        assert_eq!(inst_len(&Inst::And(Expr::Reg(Reg::R0), Expr::Reg(Reg::R4))), 2);
        assert_eq!(inst_len(&Inst::And(Expr::Reg(Reg::R1), Expr::Reg(Reg::R1))), 2);
    }

    #[test]
    fn should_calculate_or_len() {
        assert_eq!(inst_len(&Inst::Or(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))), 1);
        assert_eq!(inst_len(&Inst::Or(Expr::Reg(Reg::R0), Expr::Reg(Reg::R4))), 2);
        assert_eq!(inst_len(&Inst::Or(Expr::Reg(Reg::R1), Expr::Reg(Reg::R1))), 2);
    }

    #[test]
    fn should_calculate_xor_len() {
        assert_eq!(inst_len(&Inst::Xor(Expr::Reg(Reg::R0), Expr::Reg(Reg::R1))), 1);
        assert_eq!(inst_len(&Inst::Xor(Expr::Reg(Reg::R0), Expr::Reg(Reg::R4))), 2);
        assert_eq!(inst_len(&Inst::Xor(Expr::Reg(Reg::R1), Expr::Reg(Reg::R1))), 2);
    }

    #[test]
    fn should_calculate_mov_len() {
        assert_eq!(inst_len(&Inst::Mov(Expr::Reg(Reg::R1), Expr::Reg(Reg::R0))), 1);
        assert_eq!(inst_len(&Inst::Mov(Expr::Reg(Reg::R4), Expr::Reg(Reg::R0))), 2);
        assert_eq!(inst_len(&Inst::Mov(Expr::Reg(Reg::R1), Expr::Reg(Reg::R1))), 2);
    }

    #[test]
    fn should_calculate_ld_len() {
        assert_eq!(inst_len(&Inst::Ld(Expr::Reg(Reg::R0), Expr::AddrReg(AddrReg::A2))), 1);
        assert_eq!(inst_len(&Inst::Ld(Expr::Reg(Reg::R1), Expr::AddrReg(AddrReg::A2))), 2);
    }

    #[test]
    fn should_calculate_st_len() {
        assert_eq!(inst_len(&Inst::St(Expr::AddrReg(AddrReg::A2), Expr::Reg(Reg::R0))), 1);
        assert_eq!(inst_len(&Inst::St(Expr::AddrReg(AddrReg::A2), Expr::Reg(Reg::R1))), 2);
    }
}
