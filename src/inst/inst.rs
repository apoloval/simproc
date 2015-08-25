//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::io;
use std::iter::Iterator;

use byteorder::*;

use inst::ops::*;
use mem::*;

/// A SP-80 instruction
#[derive(Debug, PartialEq)]
pub enum Inst<O: Operands> {

    // Arithmetic-logic instructions
    Add(O::Reg, O::Reg),
    Adc(O::Reg, O::Reg),
    Addi(O::Reg, O::Immediate),
    Sub(O::Reg, O::Reg),
    Sbc(O::Reg, O::Reg),
    Subi(O::Reg, O::Immediate),
    And(O::Reg, O::Reg),
    Or(O::Reg, O::Reg),
    Xor(O::Reg, O::Reg),
    Lsl(O::Reg, O::Reg),
    Lsr(O::Reg, O::Reg),
    Asr(O::Reg, O::Reg),
    Not(O::Reg),
    Comp(O::Reg),
    Inc(O::Reg),
    Incw(O::AddrReg),
    Dec(O::Reg),
    Decw(O::AddrReg),

    // Load/store instructions
    Mov(O::Reg, O::Reg),
    Ld(O::Reg, O::AddrReg),
    St(O::AddrReg, O::Reg),
    Ldd(O::Reg, O::Addr),
    Std(O::Addr, O::Reg),
    Ldi(O::Reg, O::Immediate),
    Ldsp(O::AddrReg),
    Push(O::Reg),
    Pop(O::Reg),

    // Branching instructions
    Je(O::RelAddr),
    Jne(O::RelAddr),
    Jl(O::RelAddr),
    Jge(O::RelAddr),
    Jcc(O::RelAddr),
    Jcs(O::RelAddr),
    Jvc(O::RelAddr),
    Jvs(O::RelAddr),
    Jmp(O::Addr),
    Rjmp(O::RelAddr),
    Ijmp(O::AddrReg),
    Call(O::Addr),
    Rcall(O::RelAddr),
    Icall(O::AddrReg),
    Ret,
    Reti,

    // Others
    Nop,
    Halt,
}

pub type RuntimeInst = Inst<RuntimeOperands>;

/// A macro to pack bits using the opcode coding of SP-80.
macro_rules! pack {
    ($w:ident, $($b:expr),+) => (
        $w.write(&[$($b)+])
    );

    ($w:ident, reg $($r:ident),+ in $b:expr) => (
        $w.write(&[$($b | $r.encode())+])
    );

    ($w:ident, offset $o:ident in $b:expr) => ({
        let bin = ($o & 0x03ff).to_le();
        let l = (bin >> 8) as u8;
        let r = bin as u8;
        $w.write(&[$b | l, r])
    });
    ($w:ident, word $wrd:ident in $b:expr) => ({
        let l = ($wrd.to_be() >> 8) as u8;
        let r = $wrd.to_be() as u8;
        $w.write(&[$b, l, r])
    });
    ($w:ident, $b1:expr, regs $r1:ident, $r2:ident in $b2:expr) => ({
        let regs = $b2 | $r2.encode() | ($r1.encode() << 3);
        $w.write(&[$b1, regs])
    });
    ($w:ident, reg $r:ident in $b:expr, word $wrd:expr) => ({
        let mut bytes = [0, 0];
        LittleEndian::write_u16(&mut bytes, $wrd);
        $w.write(&[$b | $r.encode(), bytes[0], bytes[1]])
    });
    ($w:ident, reg $r:ident in $b1:expr, byte $b2:expr) => ({
        $w.write(&[$b1 | $r.encode(), $b2])
    });

}

macro_rules! get {
    ($e:expr) => ({
        match $e {
            Some(val) => val,
            None => return None,
        }
    })
}

impl<O: Operands> Inst<O> {

    #[allow(dead_code)]
    pub fn len(&self) -> usize {
        match self {
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

}

impl RuntimeInst {

    /// Encode a instruction using the given writer
    pub fn encode<W: io::Write>(&self, w: &mut W) -> io::Result<usize> {
        match self {
            &Inst::Add(ref r1, ref r2) if Self::is_onebyte(r1, r2) => pack!(w, reg r2 in 0x20),
            &Inst::Add(ref r1, ref r2) => pack!(w, 0x80, regs r1, r2 in 0x00),
            &Inst::Adc(ref r1, ref r2) if Self::is_onebyte(r1, r2) => pack!(w, reg r2 in 0x24),
            &Inst::Adc(ref r1, ref r2) => pack!(w, 0x81, regs r1, r2 in 0x00),
            &Inst::Addi(ref r, Immediate(k)) => pack!(w, reg r in 0x90, byte k),
            &Inst::Sub(ref r1, ref r2) if Self::is_onebyte(r1, r2) => pack!(w, reg r2 in 0x28),
            &Inst::Sub(ref r1, ref r2) => pack!(w, 0x82, regs r1, r2 in 0x00),
            &Inst::Sbc(ref r1, ref r2) if Self::is_onebyte(r1, r2) => pack!(w, reg r2 in 0x2c),
            &Inst::Sbc(ref r1, ref r2) => pack!(w, 0x83, regs r1, r2 in 0x00),
            &Inst::Subi(ref r, Immediate(k)) => pack!(w, reg r in 0x98, byte k),
            &Inst::And(ref r1, ref r2) => pack!(w, 0x84, regs r1, r2 in 0x00),
            &Inst::Or(ref r1, ref r2) => pack!(w, 0x85, regs r1, r2 in 0x00),
            &Inst::Xor(ref r1, ref r2) => pack!(w, 0x86, regs r1, r2 in 0x00),
            &Inst::Lsl(ref r1, ref r2) => pack!(w, 0x87, regs r1, r2 in 0x00),
            &Inst::Lsr(ref r1, ref r2) => pack!(w, 0x88, regs r1, r2 in 0x00),
            &Inst::Asr(ref r1, ref r2) => pack!(w, 0x89, regs r1, r2 in 0x00),
            &Inst::Not(ref r) => pack!(w, reg r in 0x40),
            &Inst::Comp(ref r) => pack!(w, reg r in 0x48),
            &Inst::Inc(ref r) => pack!(w, reg r in 0x50),
            &Inst::Incw(ref a) => pack!(w, reg a in 0x08),
            &Inst::Dec(ref r) => pack!(w, reg r in 0x58),
            &Inst::Decw(ref a) => pack!(w, reg a in 0x0c),
            &Inst::Mov(ref r1, ref r2) if Self::is_onebyte(r2, r1) => pack!(w, reg r1 in 0x60),
            &Inst::Mov(ref r1, ref r2) => pack!(w, 0xc0, regs r1, r2 in 0x00),
            &Inst::Ld(ref r1, ref r2) if *r1 == Reg::R0 => pack!(w, reg r2 in 0x64),
            &Inst::Ld(ref r1, ref r2) => pack!(w, 0xc1, regs r1, r2 in 0x00),
            &Inst::St(ref a, ref r) if *r == Reg::R0 => pack!(w, reg a in 0x68),
            &Inst::St(ref a, ref r) => pack!(w, 0xc2, regs a, r in 0x00),
            &Inst::Ldd(ref r, a) => pack!(w, reg r in 0xc8, word a),
            &Inst::Std(a, ref r) => pack!(w, reg r in 0xd0, word a),
            &Inst::Ldi(ref r, Immediate(k)) => pack!(w, reg r in 0xd8, byte k),
            &Inst::Ldsp(ref r) => pack!(w, reg r in 0x6c),
            &Inst::Push(ref r) => pack!(w, reg r in 0x70),
            &Inst::Pop(ref r) => pack!(w, reg r in 0x78),
            &Inst::Je(o) => pack!(w, offset o in 0x80),
            &Inst::Jne(o) => pack!(w, offset o in 0x84),
            &Inst::Jl(o) => pack!(w, offset o in 0x88),
            &Inst::Jge(o) => pack!(w, offset o in 0x8c),
            &Inst::Jcc(o) => pack!(w, offset o in 0x90),
            &Inst::Jcs(o) => pack!(w, offset o in 0x94),
            &Inst::Jvc(o) => pack!(w, offset o in 0x98),
            &Inst::Jvs(o) => pack!(w, offset o in 0x9c),
            &Inst::Jmp(a) => pack!(w, word a in 0xa0),
            &Inst::Rjmp(o) => pack!(w, offset o in 0xa4),
            &Inst::Ijmp(ref a) => pack!(w, reg a in 0xa8),
            &Inst::Call(a) => pack!(w, word a in 0xac),
            &Inst::Rcall(o) => pack!(w, offset o in 0xb0),
            &Inst::Icall(ref a) => pack!(w, reg a in 0xb4),
            &Inst::Ret => pack!(w, 0xb8),
            &Inst::Reti => pack!(w, 0xbc),
            &Inst::Nop => pack!(w, 0x00),
            &Inst::Halt => pack!(w, 0x15),
        }
    }

    pub fn decode<I: IntoIterator<Item=u8>>(input: I) -> Option<RuntimeInst> {
        let mut bytes = input.into_iter();
        let first = get!(bytes.next());
        match first >> 6 {
            0 => Self::decode_ctrl(first),
            1 => Self::decode_dt(first, bytes),
            2 => Self::decode_br(first, bytes),
            3 => Self::decode_al(first, bytes),
            _ => None
        }
    }

    fn is_onebyte(accum: &Reg, other: &Reg) -> bool {
        *accum == Reg::R0 && other.encode() < 4
    }

    fn decode_al<I: Iterator<Item=u8>>(first: u8, input: I) -> Option<RuntimeInst> {
        let mut bytes = input;
        match (first & 0x38) >> 3 {
            0 => Some(Inst::Addi(get!(Self::decode_reg(first)), Immediate(get!(bytes.next())))),
            1 => Some(Inst::Subi(get!(Self::decode_reg(first)), Immediate(get!(bytes.next())))),
            2 => Some(Inst::Not(get!(Self::decode_reg(first)))),
            3 => Some(Inst::Comp(get!(Self::decode_reg(first)))),
            4 => Some(Inst::Inc(get!(Self::decode_reg(first)))),
            5 => Some(Inst::Dec(get!(Self::decode_reg(first)))),
            6 => {
                let reg = get!(Self::decode_areg(first));
                Some(if first & 0x04 == 0x04 { Inst::Decw(reg) } else { Inst::Incw(reg) })
            },
            7 => Self::decode_arithmetic(first, get!(bytes.next())),
            _ => None
        }
    }

    fn decode_arithmetic(first: u8, next: u8) -> Option<RuntimeInst> {
        let (dst, src) = get!(Self::decode_regs(next));
        match (first & 0x07, next >> 6) {
            (0, 0) => Some(Inst::Add(dst, src)),
            (0, 1) => Some(Inst::Adc(dst, src)),
            (0, 2) => Some(Inst::Sub(dst, src)),
            (0, 3) => Some(Inst::Sbc(dst, src)),
            (2, 0) => Some(Inst::And(dst, src)),
            (2, 2) => Some(Inst::Or(dst, src)),
            (3, 0) => Some(Inst::Xor(dst, src)),
            (4, 0) => Some(Inst::Lsl(dst, src)),
            (4, 2) => Some(Inst::Lsr(dst, src)),
            (5, 2) => Some(Inst::Asr(dst, src)),
            _ => None,
        }
    }

    fn decode_dt<I: Iterator<Item=u8>>(first: u8, input: I) -> Option<RuntimeInst> {
        let mut bytes = input;
        match ((first >> 3) & 0x07, first & 0x03) {
            (0, _) =>
                Some(Inst::Ldd(get!(Self::decode_reg(first)), get!(Self::decode_word(bytes)))),
            (1, _) =>
                Some(Inst::Std(get!(Self::decode_word(bytes)), get!(Self::decode_reg(first)))),
            (2, _) =>
                Some(Inst::Ldi(get!(Self::decode_reg(first)), get!(Self::decode_immediate(bytes)))),
            (3, _) =>
                Some(Inst::Ldsp(get!(Self::decode_areg(first)))),
            (4, _) =>
                Some(Inst::Push(get!(Self::decode_reg(first)))),
            (6, _) =>
                Some(Inst::Pop(get!(Self::decode_reg(first)))),
            (7, 0) => {
                let (dst, src) = get!(Self::decode_regs(get!(bytes.next())));
                Some(Inst::Mov(dst, src))
            },
            (7, 1) => {
                let next = get!(bytes.next());
                let dst = get!(Reg::decode(next >> 3));
                let src = get!(AddrReg::decode(next & 0x03));
                Some(Inst::Ld(dst, src))
            },
            (7, 2) => {
                let next = get!(bytes.next());
                let dst = get!(AddrReg::decode(next >> 3));
                let src = get!(Reg::decode(next & 0x03));
                Some(Inst::St(dst, src))
            },
            _ => None,
        }
    }

    fn decode_br<I: Iterator<Item=u8>>(first: u8, input: I) -> Option<RuntimeInst> {
        let mut bytes = input;
        match (first >> 2) & 0x0f {
            0 => Some(Inst::Je(Self::decode_offset(first, get!(bytes.next())))),
            1 => Some(Inst::Jne(Self::decode_offset(first, get!(bytes.next())))),
            2 => Some(Inst::Jl(Self::decode_offset(first, get!(bytes.next())))),
            3 => Some(Inst::Jge(Self::decode_offset(first, get!(bytes.next())))),
            4 => Some(Inst::Jcc(Self::decode_offset(first, get!(bytes.next())))),
            5 => Some(Inst::Jcs(Self::decode_offset(first, get!(bytes.next())))),
            6 => Some(Inst::Jvc(Self::decode_offset(first, get!(bytes.next())))),
            7 => Some(Inst::Jvs(Self::decode_offset(first, get!(bytes.next())))),
            8 => Some(Inst::Jmp(get!(Self::decode_word(bytes)))),
            9 => Some(Inst::Rjmp(Self::decode_offset(first, get!(bytes.next())))),
            10 => Some(Inst::Ijmp(get!(Self::decode_areg(first & 0x3)))),
            11 => Some(Inst::Call(get!(Self::decode_word(bytes)))),
            12 => Some(Inst::Rcall(Self::decode_offset(first, get!(bytes.next())))),
            13 => Some(Inst::Icall(get!(Self::decode_areg(first & 0x3)))),
            14 => Some(Inst::Ret),
            15 => Some(Inst::Reti),
            _ => None,
        }
    }

    fn decode_ctrl(first: u8) -> Option<RuntimeInst> {
        match first {
            0x00 => Some(Inst::Nop),
            0x14 => Some(Inst::Halt),
            _ => None,
        }
    }

    fn decode_reg(byte: u8)  -> Option<Reg> { Reg::decode(byte & 0x07) }

    fn decode_areg(byte: u8)  -> Option<AddrReg> { AddrReg::decode(byte & 0x03) }

    fn decode_regs(byte: u8) -> Option<(Reg, Reg)> {
        Some((
            get!(Reg::decode((byte & 0x3f) >> 3)),
            get!(Reg::decode((byte & 0x3f) >> 0))))
    }

    fn decode_immediate<I: Iterator<Item=u8>>(input: I) -> Option<Immediate> {
        let mut bytes = input;
        bytes.next().map(|b| Immediate(b))
    }

    fn decode_word<I: Iterator<Item=u8>>(input: I) -> Option<u16> {
        let mut bytes = input;
        let lsb = get!(bytes.next());
        let msb = get!(bytes.next());
        Some(LittleEndian::read_u16(&[lsb, msb]))
    }

    fn decode_offset(first: u8, next: u8) -> RelAddr {
        let buf = [ first & 0x3, next];
        BigEndian::read_i16(&buf)
    }
}

#[cfg(test)]
mod test {

    use inst::ops::*;

    use super::*;

    #[test]
    fn encode_add() {
        assert_encode(Inst::Add(Reg::R0, Reg::R1), &[0x21]);
        assert_encode(Inst::Add(Reg::R0, Reg::R4), &[0x80, 0x04]);
        assert_encode(Inst::Add(Reg::R3, Reg::R5), &[0x80, 0x1d]);
    }

    #[test]
    fn encode_adc() {
        assert_encode(Inst::Adc(Reg::R0, Reg::R1), &[0x25]);
        assert_encode(Inst::Adc(Reg::R0, Reg::R4), &[0x81, 0x04]);
        assert_encode(Inst::Adc(Reg::R3, Reg::R5), &[0x81, 0x1d]);
    }

    #[test]
    fn encode_addi() { assert_encode(Inst::Addi(Reg::R3, Immediate(100)), &[0x93, 0x64]); }

    #[test]
    fn encode_sub() {
        assert_encode(Inst::Sub(Reg::R0, Reg::R1), &[0x29]);
        assert_encode(Inst::Sub(Reg::R0, Reg::R4), &[0x82, 0x04]);
        assert_encode(Inst::Sub(Reg::R3, Reg::R5), &[0x82, 0x1d]);
    }

    #[test]
    fn encode_sbc() {
        assert_encode(Inst::Sbc(Reg::R0, Reg::R1), &[0x2d]);
        assert_encode(Inst::Sbc(Reg::R0, Reg::R4), &[0x83, 0x04]);
        assert_encode(Inst::Sbc(Reg::R3, Reg::R5), &[0x83, 0x1d]);
    }

    #[test]
    fn encode_subi() { assert_encode(Inst::Subi(Reg::R3, Immediate(100)), &[0x9b, 0x64]); }

    #[test]
    fn encode_and() { assert_encode(Inst::And(Reg::R3, Reg::R5), &[0x84, 0x1d]); }

    #[test]
    fn encode_or() { assert_encode(Inst::Or(Reg::R3, Reg::R5), &[0x85, 0x1d]); }

    #[test]
    fn encode_xor() { assert_encode(Inst::Xor(Reg::R1, Reg::R7), &[0x86, 0x0f]); }

    #[test]
    fn encode_lsl() { assert_encode(Inst::Lsl(Reg::R3, Reg::R4), &[0x87, 0x1c]); }

    #[test]
    fn encode_lsr() { assert_encode(Inst::Lsr(Reg::R6, Reg::R1), &[0x88, 0x31]); }

    #[test]
    fn encode_asr() { assert_encode(Inst::Asr(Reg::R1, Reg::R2), &[0x89, 0x0a]); }

    #[test]
    fn encode_not() { assert_encode(Inst::Not(Reg::R5), &[0x45]); }

    #[test]
    fn encode_comp() { assert_encode(Inst::Comp(Reg::R1), &[0x49]); }

    #[test]
    fn encode_inc() { assert_encode(Inst::Inc(Reg::R6), &[0x56]); }

    #[test]
    fn encode_incw() { assert_encode(Inst::Incw(AddrReg::A2), &[0x0a]); }

    #[test]
    fn encode_dec() { assert_encode(Inst::Dec(Reg::R7), &[0x5f]); }

    #[test]
    fn encode_decw() { assert_encode(Inst::Decw(AddrReg::A1), &[0x0d]); }

    #[test]
    fn encode_mov() {
        assert_encode(Inst::Mov(Reg::R1, Reg::R0), &[0x61]);
        assert_encode(Inst::Mov(Reg::R4, Reg::R0), &[0xc0, 0x20]);
        assert_encode(Inst::Mov(Reg::R3, Reg::R5), &[0xc0, 0x1d]);
    }

    #[test]
    fn encode_ld() {
        assert_encode(Inst::Ld(Reg::R0, AddrReg::A1), &[0x65]);
        assert_encode(Inst::Ld(Reg::R0, AddrReg::A3), &[0x67]);
        assert_encode(Inst::Ld(Reg::R3, AddrReg::A3), &[0xc1, 0x1b]);
    }

    #[test]
    fn encode_st() {
        assert_encode(Inst::St(AddrReg::A1, Reg::R0), &[0x69]);
        assert_encode(Inst::St(AddrReg::A3, Reg::R0), &[0x6b]);
        assert_encode(Inst::St(AddrReg::A3, Reg::R3), &[0xc2, 0x1b]);
    }

    #[test]
    fn encode_ldd() { assert_encode(Inst::Ldd(Reg::R1, 0x2010), &[0xc9, 0x10, 0x20]); }

    #[test]
    fn encode_std() { assert_encode(Inst::Std(0x1020, Reg::R6), &[0xd6, 0x20, 0x10]); }

    #[test]
    fn encode_ldi() { assert_encode(Inst::Ldi(Reg::R2, Immediate(-13i8 as u8)), &[0xda, 0xf3]); }

    #[test]
    fn encode_ldsp() { assert_encode(Inst::Ldsp(AddrReg::A1), &[0x6d]); }

    #[test]
    fn encode_push() { assert_encode(Inst::Push(Reg::R3), &[0x73]); }

    #[test]
    fn encode_pop() { assert_encode(Inst::Pop(Reg::R5), &[0x7d]); }

    #[test]
    fn encode_je() { assert_encode(Inst::Je(1), &[0x80, 0x01]); }

    #[test]
    fn encode_jne() { assert_encode(Inst::Jne(-1), &[0x87, 0xff]); }

    #[test]
    fn encode_jl() { assert_encode(Inst::Jl(2), &[0x88, 0x02]); }

    #[test]
    fn encode_jge() { assert_encode(Inst::Jge(-2), &[0x8f, 0xfe]); }

    #[test]
    fn encode_jcc() { assert_encode(Inst::Jcc(3), &[0x90, 0x03]); }

    #[test]
    fn encode_jcs() { assert_encode(Inst::Jcs(-3), &[0x97, 0xfd]); }

    #[test]
    fn encode_jvc() { assert_encode(Inst::Jvc(4), &[0x98, 0x04]); }

    #[test]
    fn encode_jvs() { assert_encode(Inst::Jvs(-4), &[0x9f, 0xfc]); }

    #[test]
    fn encode_jmp() { assert_encode(Inst::Jmp(0x4321), &[0xa0, 0x21, 0x43]); }

    #[test]
    fn encode_rjmp() { assert_encode(Inst::Rjmp(100), &[0xa4, 0x64]); }

    #[test]
    fn encode_ijmp() { assert_encode(Inst::Ijmp(AddrReg::A3), &[0xab]); }

    #[test]
    fn encode_call() { assert_encode(Inst::Call(0x1234), &[0xac, 0x34, 0x12]); }

    #[test]
    fn encode_rcall() { assert_encode(Inst::Rcall(-100), &[0xb3, 0x9c]); }

    #[test]
    fn encode_icall() { assert_encode(Inst::Icall(AddrReg::A2), &[0xb6]); }

    #[test]
    fn encode_ret() { assert_encode(Inst::Ret, &[0xb8]); }

    #[test]
    fn encode_reti() { assert_encode(Inst::Reti, &[0xbc]); }

    #[test]
    fn encode_nop() { assert_encode(Inst::Nop, &[0x00]); }

    #[test]
    fn encode_halt() { assert_encode(Inst::Halt, &[0x15]); }

    #[test]
    fn decode_add() { assert_decode(Inst::Add(Reg::R0, Reg::R1), &[0xf8, 0x01]) }

    #[test]
    fn decode_adc() { assert_decode(Inst::Adc(Reg::R0, Reg::R1), &[0xf8, 0x41]) }

    #[test]
    fn decode_addi() { assert_decode(Inst::Addi(Reg::R0, Immediate(7)), &[0xc0, 0x07]) }

    #[test]
    fn decode_sub() { assert_decode(Inst::Sub(Reg::R0, Reg::R1), &[0xf8, 0x81]) }

    #[test]
    fn decode_sbc() { assert_decode(Inst::Sbc(Reg::R0, Reg::R1), &[0xf8, 0xc1]) }

    #[test]
    fn decode_subi() { assert_decode(Inst::Subi(Reg::R0, Immediate(7)), &[0xc8, 0x07]) }

    #[test]
    fn decode_and() { assert_decode(Inst::And(Reg::R0, Reg::R1), &[0xfa, 0x01]) }

    #[test]
    fn decode_or() { assert_decode(Inst::Or(Reg::R0, Reg::R1), &[0xfa, 0x81]) }

    #[test]
    fn decode_xor() { assert_decode(Inst::Xor(Reg::R0, Reg::R1), &[0xfb, 0x01]) }

    #[test]
    fn decode_lsl() { assert_decode(Inst::Lsl(Reg::R0, Reg::R1), &[0xfc, 0x01]) }

    #[test]
    fn decode_lsr() { assert_decode(Inst::Lsr(Reg::R0, Reg::R1), &[0xfc, 0x81]) }

    #[test]
    fn decode_asr() { assert_decode(Inst::Asr(Reg::R0, Reg::R1), &[0xfd, 0x81]) }

    #[test]
    fn decode_not() { assert_decode(Inst::Not(Reg::R0), &[0xd0]) }

    #[test]
    fn decode_comp() { assert_decode(Inst::Comp(Reg::R0), &[0xd8]) }

    #[test]
    fn decode_inc() { assert_decode(Inst::Inc(Reg::R0), &[0xe0]) }

    #[test]
    fn decode_incw() { assert_decode(Inst::Incw(AddrReg::A0), &[0xf0]) }

    #[test]
    fn decode_dec() { assert_decode(Inst::Dec(Reg::R0), &[0xe8]) }

    #[test]
    fn decode_decw() { assert_decode(Inst::Decw(AddrReg::A0), &[0xf4]) }

    #[test]
    fn decode_mov() { assert_decode(Inst::Mov(Reg::R0, Reg::R1), &[0x78, 0x01]) }

    #[test]
    fn decode_ld() { assert_decode(Inst::Ld(Reg::R0, AddrReg::A1), &[0x79, 0x01]) }

    #[test]
    fn decode_st() { assert_decode(Inst::St(AddrReg::A0, Reg::R1), &[0x7a, 0x01]) }

    #[test]
    fn decode_ldd() { assert_decode(Inst::Ldd(Reg::R0, 0x8000), &[0x40, 0x00, 0x80]) }

    #[test]
    fn decode_std() { assert_decode(Inst::Std(0x8000, Reg::R0), &[0x48, 0x00, 0x80]) }

    #[test]
    fn decode_ldi() { assert_decode(Inst::Ldi(Reg::R0, Immediate(7)), &[0x50, 0x07]) }

    #[test]
    fn decode_ldsp() { assert_decode(Inst::Ldsp(AddrReg::A0), &[0x58]) }

    #[test]
    fn decode_push() { assert_decode(Inst::Push(Reg::R0), &[0x60]) }

    #[test]
    fn decode_pop() { assert_decode(Inst::Pop(Reg::R0), &[0x70]) }

    #[test]
    fn decode_je() { assert_decode(Inst::Je(0x100), &[0x81, 0x00]) }

    #[test]
    fn decode_jne() { assert_decode(Inst::Jne(0x100), &[0x85, 0x00]) }

    #[test]
    fn decode_jl() { assert_decode(Inst::Jl(0x100), &[0x89, 0x00]) }

    #[test]
    fn decode_jge() { assert_decode(Inst::Jge(0x100), &[0x8d, 0x00]) }

    #[test]
    fn decode_jcc() { assert_decode(Inst::Jcc(0x100), &[0x91, 0x00]) }

    #[test]
    fn decode_jcs() { assert_decode(Inst::Jcs(0x100), &[0x95, 0x00]) }

    #[test]
    fn decode_jvc() { assert_decode(Inst::Jvc(0x100), &[0x99, 0x00]) }

    #[test]
    fn decode_jvs() { assert_decode(Inst::Jvs(0x100), &[0x9d, 0x00]) }

    #[test]
    fn decode_jmp() { assert_decode(Inst::Jmp(0x8000), &[0xa0, 0x00, 0x80]) }

    #[test]
    fn decode_rjmp() { assert_decode(Inst::Rjmp(0x100), &[0xa5, 0x00]) }

    #[test]
    fn decode_ijmp() { assert_decode(Inst::Ijmp(AddrReg::A0), &[0xa8]) }

    #[test]
    fn decode_call() { assert_decode(Inst::Call(0x8000), &[0xac, 0x00, 0x80]) }

    #[test]
    fn decode_rcall() { assert_decode(Inst::Rcall(0x100), &[0xb1, 0x00]) }

    #[test]
    fn decode_icall() { assert_decode(Inst::Icall(AddrReg::A0), &[0xb4]) }

    #[test]
    fn decode_ret() { assert_decode(Inst::Ret, &[0xb8]) }

    #[test]
    fn decode_reti() { assert_decode(Inst::Reti, &[0xbc]) }

    #[test]
    fn decode_nop() { assert_decode(Inst::Nop, &[0x00]) }

    #[test]
    fn decode_halt() { assert_decode(Inst::Halt, &[0x14]) }

    fn assert_encode(inst: RuntimeInst, bytes: &[u8]) {
        let mut w: Vec<u8> = Vec::with_capacity(16);
        let result = inst.encode(&mut w);
        assert!(result.is_ok());
        assert_eq!(bytes.len(), result.ok().unwrap());
        assert_eq!(&w[..], bytes);
    }

    fn assert_decode(expected: RuntimeInst, bytes: &[u8]) {
        let actual = RuntimeInst::decode(bytes.iter().map(|b| *b));
        assert!(actual.is_some());
        assert_eq!(actual.unwrap(), expected);
    }

}
