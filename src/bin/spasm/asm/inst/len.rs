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
        &Inst::Add(_, _) => 2,
        &Inst::Adc(_, _) => 2,
        &Inst::Addi(_, _) => 2,
        &Inst::Sub(_, _) => 2,
        &Inst::Sbc(_, _) => 2,
        &Inst::Subi(_, _) => 2,
        &Inst::And(_, _) => 2,
        &Inst::Or(_, _) => 2,
        &Inst::Xor(_, _) => 2,
        &Inst::Lsl(_, _) => 2,
        &Inst::Lsr(_, _) => 2,
        &Inst::Asr(_, _) => 2,
        &Inst::Not(_) => 1,
        &Inst::Comp(_) => 1,
        &Inst::Inc(_) => 1,
        &Inst::Incw(_) => 1,
        &Inst::Dec(_) => 1,
        &Inst::Decw(_) => 1,
        &Inst::Mov(_, _) => 2,
        &Inst::Ld(_, _) => 2,
        &Inst::St(_, _) => 2,
        &Inst::Ldd(_, _) => 3,
        &Inst::Std(_, _) => 3,
        &Inst::Ldi(_, _) => 2,
        &Inst::Ldsp(_) => 1,
        &Inst::Push(_) => 1,
        &Inst::In(_, _) => 2,
        &Inst::Out(_, _) => 2,
        &Inst::Pop(_) => 1,
        &Inst::Je(_) => 2,
        &Inst::Jne(_) => 2,
        &Inst::Jl(_) => 2,
        &Inst::Jge(_) => 2,
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
    }
}
