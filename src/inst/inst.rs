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
    Lsl(O::Reg),
    Lsr(O::Reg),
    Asr(O::Reg),
    Neg(O::Reg),
    Com(O::Reg),
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
    Ldw(O::AddrReg, O::Addr),
    Ldsp(O::AddrReg),
    Push(O::Reg),
    Pop(O::Reg),
    In(O::Reg, O::IoPort),
    Out(O::IoPort, O::Reg),

    // Branching instructions
    Jnz(O::RelAddr),
    Jz(O::RelAddr),
    Jp(O::RelAddr),
    Jn(O::RelAddr),
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
    Ei,
    Di,
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
    ($w:ident, $b1:expr, reg $r1:ident in $b2:expr) => ({
        let regs = $b2 | $r1.encode();
        $w.write(&[$b1, regs])
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
            &Inst::And(ref r1, ref r2) if Self::is_onebyte(r1, r2) => pack!(w, reg r2 in 0x30),
            &Inst::And(ref r1, ref r2) => pack!(w, 0x84, regs r1, r2 in 0x00),
            &Inst::Or(ref r1, ref r2) if Self::is_onebyte(r1, r2) => pack!(w, reg r2 in 0x34),
            &Inst::Or(ref r1, ref r2) => pack!(w, 0x85, regs r1, r2 in 0x00),
            &Inst::Xor(ref r1, ref r2) if Self::is_onebyte(r1, r2) => pack!(w, reg r2 in 0x38),
            &Inst::Xor(ref r1, ref r2) => pack!(w, 0x86, regs r1, r2 in 0x00),
            &Inst::Lsl(ref r1) => pack!(w, 0x87, reg r1 in 0x00),
            &Inst::Lsr(ref r1) => pack!(w, 0x88, reg r1 in 0x00),
            &Inst::Asr(ref r1) => pack!(w, 0x89, reg r1 in 0x00),
            &Inst::Neg(ref r) => pack!(w, reg r in 0x40),
            &Inst::Com(ref r) => pack!(w, reg r in 0x48),
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
            &Inst::Ldw(ref a, addr) => pack!(w, reg a in 0xc4, word addr),
            &Inst::Ldsp(ref r) => pack!(w, reg r in 0x6c),
            &Inst::Push(ref r) => pack!(w, reg r in 0x70),
            &Inst::Pop(ref r) => pack!(w, reg r in 0x78),
            &Inst::In(ref r, IoPort(p)) => pack!(w, reg r in 0xf0, byte p),
            &Inst::Out(IoPort(p), ref r) => pack!(w, reg r in 0xf8, byte p),
            &Inst::Jnz(o) => pack!(w, offset o in 0xa0),
            &Inst::Jz(o) => pack!(w, offset o in 0xa4),
            &Inst::Jp(o) => pack!(w, offset o in 0xa8),
            &Inst::Jn(o) => pack!(w, offset o in 0xac),
            &Inst::Jcc(o) => pack!(w, offset o in 0xb0),
            &Inst::Jcs(o) => pack!(w, offset o in 0xb4),
            &Inst::Jvc(o) => pack!(w, offset o in 0xb8),
            &Inst::Jvs(o) => pack!(w, offset o in 0xbc),
            &Inst::Jmp(a) => pack!(w, word a in 0x8a),
            &Inst::Rjmp(o) => pack!(w, offset o in 0xe0),
            &Inst::Ijmp(ref a) => pack!(w, reg a in 0x10),
            &Inst::Call(a) => pack!(w, word a in 0x8b),
            &Inst::Rcall(o) => pack!(w, offset o in 0xe4),
            &Inst::Icall(ref a) => pack!(w, reg a in 0x14),
            &Inst::Ret => pack!(w, 0x02),
            &Inst::Reti => pack!(w, 0x03),
            &Inst::Nop => pack!(w, 0x00),
            &Inst::Halt => pack!(w, 0x3c),
            &Inst::Ei => pack!(w, 0x04),
            &Inst::Di => pack!(w, 0x05),
        }
    }

    pub fn decode<I: IntoIterator<Item=u8>>(input: I) -> Option<RuntimeInst> {
        let mut bytes = input.into_iter();
        let first = get!(bytes.next());
        let next = if first & 0x80 == 0x80 { get!(bytes.next()) } else { 0 };
        let f5 = first & 0xf8;
        let f6 = first & 0xfc;
        let f8 = first;
        match (f5, f6, f8) {
            (0x40, _, _) => Some(Inst::Neg(Self::decode_reg(first))),
            (0x48, _, _) => Some(Inst::Com(Self::decode_reg(first))),
            (0x50, _, _) => Some(Inst::Inc(Self::decode_reg(first))),
            (0x58, _, _) => Some(Inst::Dec(Self::decode_reg(first))),
            (0x70, _, _) => Some(Inst::Push(Self::decode_reg(first))),
            (0x78, _, _) => Some(Inst::Pop(Self::decode_reg(first))),
            (0xc8, _, _) => Some(Inst::Ldd(Self::decode_reg(first), Self::decode_word(next, get!(bytes.next())))),
            (0xd0, _, _) => Some(Inst::Std(Self::decode_word(next, get!(bytes.next())), Self::decode_reg(first))),
            (0xd8, _, _) => Some(Inst::Ldi(Self::decode_reg(first), Immediate(next))),
            (0x90, _, _) => Some(Inst::Addi(Self::decode_reg(first), Immediate(next))),
            (0x98, _, _) => Some(Inst::Subi(Self::decode_reg(first), Immediate(next))),
            (0xf0, _, _) => Some(Inst::In(Self::decode_reg(first), IoPort(next))),
            (0xf8, _, _) => Some(Inst::Out(IoPort(next), Self::decode_reg(first))),
            (_, 0x08, _) => Some(Inst::Incw(Self::decode_areg(first))),
            (_, 0x0c, _) => Some(Inst::Decw(Self::decode_areg(first))),
            (_, 0x10, _) => Some(Inst::Ijmp(Self::decode_areg(first))),
            (_, 0x14, _) => Some(Inst::Icall(Self::decode_areg(first))),
            (_, 0x20, _) => Some(Inst::Add(Reg::R0, Self::decode_short_reg(first))),
            (_, 0x24, _) => Some(Inst::Adc(Reg::R0, Self::decode_short_reg(first))),
            (_, 0x28, _) => Some(Inst::Sub(Reg::R0, Self::decode_short_reg(first))),
            (_, 0x2c, _) => Some(Inst::Sbc(Reg::R0, Self::decode_short_reg(first))),
            (_, 0x30, _) => Some(Inst::And(Reg::R0, Self::decode_short_reg(first))),
            (_, 0x34, _) => Some(Inst::Or(Reg::R0, Self::decode_short_reg(first))),
            (_, 0x38, _) => Some(Inst::Xor(Reg::R0, Self::decode_short_reg(first))),
            (_, 0x60, _) => Some(Inst::Mov(Self::decode_short_reg(first), Reg::R0)),
            (_, 0x64, _) => Some(Inst::Ld(Reg::R0, Self::decode_areg(first))),
            (_, 0x68, _) => Some(Inst::St(Self::decode_areg(first), Reg::R0)),
            (_, 0x6c, _) => Some(Inst::Ldsp(Self::decode_areg(first))),
            (_, 0xa0, _) => Some(Inst::Jnz(Self::decode_offset(first, next))),
            (_, 0xa4, _) => Some(Inst::Jz(Self::decode_offset(first, next))),
            (_, 0xa8, _) => Some(Inst::Jp(Self::decode_offset(first, next))),
            (_, 0xac, _) => Some(Inst::Jn(Self::decode_offset(first, next))),
            (_, 0xb0, _) => Some(Inst::Jcc(Self::decode_offset(first, next))),
            (_, 0xb4, _) => Some(Inst::Jcs(Self::decode_offset(first, next))),
            (_, 0xb8, _) => Some(Inst::Jvc(Self::decode_offset(first, next))),
            (_, 0xbc, _) => Some(Inst::Jvs(Self::decode_offset(first, next))),
            (_, 0xc4, _) => Some(Inst::Ldw(Self::decode_areg(first), Self::decode_word(next, get!(bytes.next())))),
            (_, 0xe0, _) => Some(Inst::Rjmp(Self::decode_offset(first, next))),
            (_, 0xe4, _) => Some(Inst::Rcall(Self::decode_offset(first, next))),
            (_, _, 0x00) => Some(Inst::Nop),
            (_, _, 0x02) => Some(Inst::Ret),
            (_, _, 0x03) => Some(Inst::Reti),
            (_, _, 0x04) => Some(Inst::Ei),
            (_, _, 0x05) => Some(Inst::Di),
            (_, _, 0x3c) => Some(Inst::Halt),
            (_, _, 0x80) => Self::decode_reg_reg(Inst::Add, next),
            (_, _, 0x81) => Self::decode_reg_reg(Inst::Adc, next),
            (_, _, 0x82) => Self::decode_reg_reg(Inst::Sub, next),
            (_, _, 0x83) => Self::decode_reg_reg(Inst::Sbc, next),
            (_, _, 0x84) => Self::decode_reg_reg(Inst::And, next),
            (_, _, 0x85) => Self::decode_reg_reg(Inst::Or, next),
            (_, _, 0x86) => Self::decode_reg_reg(Inst::Xor, next),
            (_, _, 0x87) => Self::decode_long_reg(Inst::Lsl, next),
            (_, _, 0x88) => Self::decode_long_reg(Inst::Lsr, next),
            (_, _, 0x89) => Self::decode_long_reg(Inst::Asr, next),
            (_, _, 0x8a) => Some(Inst::Jmp(Self::decode_word(next, get!(bytes.next())))),
            (_, _, 0x8b) => Some(Inst::Call(Self::decode_word(next, get!(bytes.next())))),
            (_, _, 0xc0) => Self::decode_reg_reg(Inst::Mov, next),
            (_, _, 0xc1) => Self::decode_reg_areg(Inst::Ld, next),
            (_, _, 0xc2) => Self::decode_areg_reg(Inst::St, next),
            _ => None
        }
    }

    fn is_onebyte(accum: &Reg, other: &Reg) -> bool {
        *accum == Reg::R0 && other.encode() < 4
    }

    fn decode_short_reg(byte: u8)  -> Reg { Reg::decode(byte & 0x03).unwrap() }

    fn decode_reg(byte: u8)  -> Reg { Reg::decode(byte & 0x07).unwrap() }

    fn decode_areg(byte: u8)  -> AddrReg { AddrReg::decode(byte & 0x03).unwrap() }

    fn decode_regs(byte: u8) -> Option<(Reg, Reg)> {
        Some((
            get!(Reg::decode((byte & 0x38) >> 3)),
            get!(Reg::decode((byte & 0x07) >> 0))))
    }

    fn decode_word(lsb: u8, msb: u8) -> u16 {
        LittleEndian::read_u16(&[lsb, msb])
    }

    fn decode_offset(first: u8, next: u8) -> RelAddr {
        let msb = if first & 0x02 > 0 { first | 0xfc } else { first & 0x03 };
        let buf = [ msb , next ];
        BigEndian::read_i16(&buf)
    }

    fn decode_reg_reg<F, I>(inst: F, next: u8) -> Option<I> where F: Fn(Reg, Reg) -> I {
        let (dst, src) = get!(Self::decode_regs(next));
        Some(inst(dst, src))
    }

    fn decode_long_reg<F, I>(inst: F, next: u8) -> Option<I> where F: Fn(Reg) -> I {
        let dst = Self::decode_reg(next);
        Some(inst(dst))
    }

    fn decode_reg_areg<F, I>(inst: F, next: u8) -> Option<I> where F: Fn(Reg, AddrReg) -> I {
        let dst = Self::decode_reg(next >> 3);
        let src = Self::decode_areg(next);
        Some(inst(dst, src))
    }

    fn decode_areg_reg<F, I>(inst: F, next: u8) -> Option<I> where F: Fn(AddrReg, Reg) -> I {
        let dst = Self::decode_areg(next >> 3);
        let src = Self::decode_reg(next);
        Some(inst(dst, src))
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
    fn encode_and() {
        assert_encode(Inst::And(Reg::R0, Reg::R1), &[0x31]);
        assert_encode(Inst::And(Reg::R0, Reg::R4), &[0x84, 0x04]);
        assert_encode(Inst::And(Reg::R3, Reg::R5), &[0x84, 0x1d]);
    }

    #[test]
    fn encode_or() {
        assert_encode(Inst::Or(Reg::R0, Reg::R1), &[0x35]);
        assert_encode(Inst::Or(Reg::R0, Reg::R4), &[0x85, 0x04]);
        assert_encode(Inst::Or(Reg::R3, Reg::R5), &[0x85, 0x1d]);
    }

    #[test]
    fn encode_xor() {
        assert_encode(Inst::Xor(Reg::R0, Reg::R1), &[0x39]);
        assert_encode(Inst::Xor(Reg::R0, Reg::R4), &[0x86, 0x04]);
        assert_encode(Inst::Xor(Reg::R1, Reg::R7), &[0x86, 0x0f]);
    }

    #[test]
    fn encode_lsl() { assert_encode(Inst::Lsl(Reg::R3), &[0x87, 0x03]); }

    #[test]
    fn encode_lsr() { assert_encode(Inst::Lsr(Reg::R6), &[0x88, 0x06]); }

    #[test]
    fn encode_asr() { assert_encode(Inst::Asr(Reg::R1), &[0x89, 0x01]); }

    #[test]
    fn encode_neg() { assert_encode(Inst::Neg(Reg::R5), &[0x45]); }

    #[test]
    fn encode_com() { assert_encode(Inst::Com(Reg::R1), &[0x49]); }

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
    fn encode_ldw() { assert_encode(Inst::Ldw(AddrReg::A1, 0x1020), &[0xc5, 0x20, 0x10]); }

    #[test]
    fn encode_ldsp() { assert_encode(Inst::Ldsp(AddrReg::A1), &[0x6d]); }

    #[test]
    fn encode_push() { assert_encode(Inst::Push(Reg::R3), &[0x73]); }

    #[test]
    fn encode_pop() { assert_encode(Inst::Pop(Reg::R5), &[0x7d]); }

    #[test]
    fn encode_in() { assert_encode(Inst::In(Reg::R0, IoPort(100)), &[0xf0, 0x64]); }

    #[test]
    fn encode_out() { assert_encode(Inst::Out(IoPort(100), Reg::R0), &[0xf8, 0x64]); }

    #[test]
    fn encode_jnz() { assert_encode(Inst::Jnz(1), &[0xa0, 0x01]); }

    #[test]
    fn encode_jz() { assert_encode(Inst::Jz(-1), &[0xa7, 0xff]); }

    #[test]
    fn encode_jp() { assert_encode(Inst::Jp(2), &[0xa8, 0x02]); }

    #[test]
    fn encode_jn() { assert_encode(Inst::Jn(-2), &[0xaf, 0xfe]); }

    #[test]
    fn encode_jcc() { assert_encode(Inst::Jcc(3), &[0xb0, 0x03]); }

    #[test]
    fn encode_jcs() { assert_encode(Inst::Jcs(-3), &[0xb7, 0xfd]); }

    #[test]
    fn encode_jvc() { assert_encode(Inst::Jvc(4), &[0xb8, 0x04]); }

    #[test]
    fn encode_jvs() { assert_encode(Inst::Jvs(-4), &[0xbf, 0xfc]); }

    #[test]
    fn encode_jmp() { assert_encode(Inst::Jmp(0x4321), &[0x8a, 0x21, 0x43]); }

    #[test]
    fn encode_rjmp() { assert_encode(Inst::Rjmp(100), &[0xe0, 0x64]); }

    #[test]
    fn encode_ijmp() { assert_encode(Inst::Ijmp(AddrReg::A3), &[0x13]); }

    #[test]
    fn encode_call() { assert_encode(Inst::Call(0x1234), &[0x8b, 0x34, 0x12]); }

    #[test]
    fn encode_rcall() { assert_encode(Inst::Rcall(-100), &[0xe7, 0x9c]); }

    #[test]
    fn encode_icall() { assert_encode(Inst::Icall(AddrReg::A2), &[0x16]); }

    #[test]
    fn encode_ret() { assert_encode(Inst::Ret, &[0x02]); }

    #[test]
    fn encode_reti() { assert_encode(Inst::Reti, &[0x03]); }

    #[test]
    fn encode_nop() { assert_encode(Inst::Nop, &[0x00]); }

    #[test]
    fn encode_halt() { assert_encode(Inst::Halt, &[0x3c]); }

    #[test]
    fn encode_ei() { assert_encode(Inst::Ei, &[0x04]); }

    #[test]
    fn encode_di() { assert_encode(Inst::Di, &[0x05]); }

    #[test]
    fn decode_add() {
        assert_decode(Inst::Add(Reg::R0, Reg::R1), &[0x21]);
        assert_decode(Inst::Add(Reg::R0, Reg::R4), &[0x80, 0x04]);
        assert_decode(Inst::Add(Reg::R1, Reg::R2), &[0x80, 0x0a]);
    }

    #[test]
    fn decode_adc() {
        assert_decode(Inst::Adc(Reg::R0, Reg::R1), &[0x25]);
        assert_decode(Inst::Adc(Reg::R0, Reg::R4), &[0x81, 0x04]);
        assert_decode(Inst::Adc(Reg::R1, Reg::R2), &[0x81, 0x0a]);
    }

    #[test]
    fn decode_addi() { assert_decode(Inst::Addi(Reg::R0, Immediate(7)), &[0x90, 0x07]) }

    #[test]
    fn decode_sub() {
        assert_decode(Inst::Sub(Reg::R0, Reg::R1), &[0x29]);
        assert_decode(Inst::Sub(Reg::R0, Reg::R4), &[0x82, 0x04]);
        assert_decode(Inst::Sub(Reg::R1, Reg::R2), &[0x82, 0x0a]);
    }

    #[test]
    fn decode_sbc() {
        assert_decode(Inst::Sbc(Reg::R0, Reg::R1), &[0x2d]);
        assert_decode(Inst::Sbc(Reg::R0, Reg::R4), &[0x83, 0x04]);
        assert_decode(Inst::Sbc(Reg::R1, Reg::R2), &[0x83, 0x0a]);
    }

    #[test]
    fn decode_subi() { assert_decode(Inst::Subi(Reg::R0, Immediate(7)), &[0x98, 0x07]) }

    #[test]
    fn decode_and() {
        assert_decode(Inst::And(Reg::R0, Reg::R1), &[0x31]);
        assert_decode(Inst::And(Reg::R0, Reg::R4), &[0x84, 0x04]);
        assert_decode(Inst::And(Reg::R1, Reg::R2), &[0x84, 0x0a]);
    }

    #[test]
    fn decode_or() {
        assert_decode(Inst::Or(Reg::R0, Reg::R1), &[0x35]);
        assert_decode(Inst::Or(Reg::R0, Reg::R4), &[0x85, 0x04]);
        assert_decode(Inst::Or(Reg::R1, Reg::R2), &[0x85, 0x0a]);
    }

    #[test]
    fn decode_xor() {
        assert_decode(Inst::Xor(Reg::R0, Reg::R1), &[0x39]);
        assert_decode(Inst::Xor(Reg::R0, Reg::R4), &[0x86, 0x04]);
        assert_decode(Inst::Xor(Reg::R1, Reg::R2), &[0x86, 0x0a]);
    }

    #[test]
    fn decode_lsl() { assert_decode(Inst::Lsl(Reg::R1), &[0x87, 0x01]) }

    #[test]
    fn decode_lsr() { assert_decode(Inst::Lsr(Reg::R1), &[0x88, 0x01]) }

    #[test]
    fn decode_asr() { assert_decode(Inst::Asr(Reg::R1), &[0x89, 0x01]) }

    #[test]
    fn decode_neg() { assert_decode(Inst::Neg(Reg::R0), &[0x40]) }

    #[test]
    fn decode_com() { assert_decode(Inst::Com(Reg::R0), &[0x48]) }

    #[test]
    fn decode_inc() { assert_decode(Inst::Inc(Reg::R0), &[0x50]) }

    #[test]
    fn decode_incw() { assert_decode(Inst::Incw(AddrReg::A0), &[0x08]) }

    #[test]
    fn decode_dec() { assert_decode(Inst::Dec(Reg::R0), &[0x58]) }

    #[test]
    fn decode_decw() { assert_decode(Inst::Decw(AddrReg::A0), &[0x0c]) }

    #[test]
    fn decode_mov() {
        assert_decode(Inst::Mov(Reg::R1, Reg::R0), &[0x61]);
        assert_decode(Inst::Mov(Reg::R4, Reg::R0), &[0xc0, 0x20]);
        assert_decode(Inst::Mov(Reg::R0, Reg::R1), &[0xc0, 0x01]);
    }

    #[test]
    fn decode_ld() {
        assert_decode(Inst::Ld(Reg::R0, AddrReg::A1), &[0x65]);
        assert_decode(Inst::Ld(Reg::R1, AddrReg::A1), &[0xc1, 0x09]);
    }

    #[test]
    fn decode_st() {
        assert_decode(Inst::St(AddrReg::A1, Reg::R0), &[0x69]);
        assert_decode(Inst::St(AddrReg::A0, Reg::R1), &[0xc2, 0x01]);
    }

    #[test]
    fn decode_ldd() { assert_decode(Inst::Ldd(Reg::R0, 0x8000), &[0xc8, 0x00, 0x80]) }

    #[test]
    fn decode_std() { assert_decode(Inst::Std(0x8000, Reg::R0), &[0xd0, 0x00, 0x80]) }

    #[test]
    fn decode_ldi() { assert_decode(Inst::Ldi(Reg::R0, Immediate(7)), &[0xd8, 0x07]) }

    #[test]
    fn decode_ldw() { assert_decode(Inst::Ldw(AddrReg::A0, 0x1020), &[0xc4, 0x20, 0x10]) }

    #[test]
    fn decode_ldsp() { assert_decode(Inst::Ldsp(AddrReg::A0), &[0x6c]) }

    #[test]
    fn decode_push() { assert_decode(Inst::Push(Reg::R0), &[0x70]) }

    #[test]
    fn decode_pop() { assert_decode(Inst::Pop(Reg::R0), &[0x78]) }

    #[test]
    fn decode_in() { assert_decode(Inst::In(Reg::R0, IoPort(100)), &[0xf0, 0x64]) }

    #[test]
    fn decode_out() { assert_decode(Inst::Out(IoPort(100), Reg::R0), &[0xf8, 0x64]) }

    #[test]
    fn decode_jnz() { assert_decode(Inst::Jnz(0x100), &[0xa1, 0x00]) }

    #[test]
    fn decode_jz() { assert_decode(Inst::Jz(0x100), &[0xa5, 0x00]) }

    #[test]
    fn decode_jp() { assert_decode(Inst::Jp(0x100), &[0xa9, 0x00]) }

    #[test]
    fn decode_jn() { assert_decode(Inst::Jn(0x100), &[0xad, 0x00]) }

    #[test]
    fn decode_jcc() { assert_decode(Inst::Jcc(0x100), &[0xb1, 0x00]) }

    #[test]
    fn decode_jcs() { assert_decode(Inst::Jcs(0x100), &[0xb5, 0x00]) }

    #[test]
    fn decode_jvc() { assert_decode(Inst::Jvc(0x100), &[0xb9, 0x00]) }

    #[test]
    fn decode_jvs() { assert_decode(Inst::Jvs(0x100), &[0xbd, 0x00]) }

    #[test]
    fn decode_jmp() { assert_decode(Inst::Jmp(0x8000), &[0x8a, 0x00, 0x80]) }

    #[test]
    fn decode_rjmp() { assert_decode(Inst::Rjmp(-2), &[0xe3, 0xfe]) }

    #[test]
    fn decode_ijmp() { assert_decode(Inst::Ijmp(AddrReg::A0), &[0x10]) }

    #[test]
    fn decode_call() { assert_decode(Inst::Call(0x8000), &[0x8b, 0x00, 0x80]) }

    #[test]
    fn decode_rcall() { assert_decode(Inst::Rcall(0x100), &[0xe5, 0x00]) }

    #[test]
    fn decode_icall() { assert_decode(Inst::Icall(AddrReg::A0), &[0x14]) }

    #[test]
    fn decode_ret() { assert_decode(Inst::Ret, &[0x02]) }

    #[test]
    fn decode_reti() { assert_decode(Inst::Reti, &[0x03]) }

    #[test]
    fn decode_nop() { assert_decode(Inst::Nop, &[0x00]) }

    #[test]
    fn decode_halt() { assert_decode(Inst::Halt, &[0x3c]) }

    #[test]
    fn decode_ei() { assert_decode(Inst::Ei, &[0x04]) }

    #[test]
    fn decode_di() { assert_decode(Inst::Di, &[0x05]) }

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
