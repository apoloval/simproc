//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate serialize;

use std::io;
use std::mem;
use std::num::{Int};
use std::str::FromStr;

use self::serialize::hex::FromHex;

use Encode;

mod args;
pub use self::args::*;

/// A SP-80 instruction
// pub enum Sp80Inst<I, A, RA, R, DR, AR> {
pub enum Sp80Inst {

	// Arithmetic-logic instructions
	Add(Reg, Reg),
	Addw(AddrReg, AddrReg),
	Addi(Reg, Immediate),
	Sub(Reg, Reg),
	Subw(AddrReg, AddrReg),
	Subi(Reg, Immediate),
	Mulw(AddrReg, AddrReg),
	And(Reg, Reg),
	Or(Reg, Reg),
	Xor(Reg, Reg),
	Lsl(Reg, Reg),
	Lsr(Reg, Reg),
	Asr(Reg, Reg),
	Not(Reg),
	Comp(Reg),
	Inc(Reg),
	Incw(AddrReg),
	Dec(Reg),
	Decw(AddrReg),

	// Load/store instructions
	Mov(Reg, Reg),
	Ld(Reg, AddrReg),
	St(AddrReg, Reg),
	Ldd(Reg, Addr),
	Std(Addr, Reg),
	Ldi(Reg, Immediate),
	Ldsp(AddrReg),
	Push(Reg),
	Pop(Reg),

	// Branching instructions
	Je(RelAddr),
	Jne(RelAddr),
	Jl(RelAddr),
	Jge(RelAddr),
	Jcc(RelAddr),
	Jcs(RelAddr),
	Jvc(RelAddr),
	Jvs(RelAddr),
	Jmp(Addr),
	Rjmp(RelAddr),
	Ijmp(AddrReg),
	Call(Addr),
	Rcall(RelAddr),
	Icall(AddrReg),
	Ret,
	Reti,

	// Others
	Nop,
	Halt,
}

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
		let l = ($wrd.to_be() >> 8) as u8;
		let r = $wrd.to_be() as u8;
		$w.write(&[$b | $r.encode(), l, r])
	});
	($w:ident, reg $r:ident in $b1:expr, byte $b2:expr) => ({
		$w.write(&[$b1 | $r.encode(), $b2])
	});

}

impl Encode for Sp80Inst {

	fn len(&self) -> usize {
		match self {
			&Sp80Inst::Add(_, _) => 2,
			&Sp80Inst::Addw(_, _) => 2,
			&Sp80Inst::Addi(_, _) => 2,
			&Sp80Inst::Sub(_, _) => 2,
			&Sp80Inst::Subw(_, _) => 2,
			&Sp80Inst::Subi(_, _) => 2,
			&Sp80Inst::Mulw(_, _) => 2,
			&Sp80Inst::And(_, _) => 2,
			&Sp80Inst::Or(_, _) => 2,
			&Sp80Inst::Xor(_, _) => 2,
			&Sp80Inst::Lsl(_, _) => 2,
			&Sp80Inst::Lsr(_, _) => 2,
			&Sp80Inst::Asr(_, _) => 2,
			&Sp80Inst::Not(_) => 1,
			&Sp80Inst::Comp(_) => 1,
			&Sp80Inst::Inc(_) => 1,
			&Sp80Inst::Incw(_) => 1,
			&Sp80Inst::Dec(_) => 1,
			&Sp80Inst::Decw(_) => 1,
			&Sp80Inst::Mov(_, _) => 2,
			&Sp80Inst::Ld(_, _) => 2,
			&Sp80Inst::St(_, _) => 2,
			&Sp80Inst::Ldd(_, _) => 3,
			&Sp80Inst::Std(_, _) => 3,
			&Sp80Inst::Ldi(_, _) => 2,
			&Sp80Inst::Ldsp(_) => 1,
			&Sp80Inst::Push(_) => 1,
			&Sp80Inst::Pop(_) => 1,
			&Sp80Inst::Je(_) => 2,
			&Sp80Inst::Jne(_) => 2,
			&Sp80Inst::Jl(_) => 2,
			&Sp80Inst::Jge(_) => 2,
			&Sp80Inst::Jcc(_) => 2,
			&Sp80Inst::Jcs(_) => 2,
			&Sp80Inst::Jvc(_) => 2,
			&Sp80Inst::Jvs(_) => 2,
			&Sp80Inst::Jmp(_) => 3,
			&Sp80Inst::Rjmp(_) => 2,
			&Sp80Inst::Ijmp(_) => 1,
			&Sp80Inst::Call(_) => 3,
			&Sp80Inst::Rcall(_) => 2,
			&Sp80Inst::Icall(_) => 1,
			&Sp80Inst::Ret => 1,
			&Sp80Inst::Reti => 1,
			&Sp80Inst::Nop => 1,
			&Sp80Inst::Halt => 1,
		}
	}

	/// Encode a instruction using the given writer
	fn encode<W: io::Write>(&self, w: &mut W) -> io::Result<usize> {
		match self {
			&Sp80Inst::Add(ref r1, ref r2) => pack!(w, 0xf8, regs r1, r2 in 0x00),
			&Sp80Inst::Addw(ref a1, ref a2) => pack!(w, 0xf8, regs a1, a2 in 0x40),
			&Sp80Inst::Addi(ref r, Immediate(k)) => pack!(w, reg r in 0xc0, byte k),
			&Sp80Inst::Sub(ref r1, ref r2) => pack!(w, 0xf8, regs r1, r2 in 0x80),
			&Sp80Inst::Subw(ref a1, ref a2) => pack!(w, 0xf8, regs a1, a2 in 0xc0),
			&Sp80Inst::Subi(ref r, Immediate(k)) => pack!(w, reg r in 0xc8, byte k),
			&Sp80Inst::Mulw(ref a1, ref a2) => pack!(w, 0xf8, regs a1, a2 in 0x40),
			&Sp80Inst::And(ref r1, ref r2) => pack!(w, 0xf9, regs r1, r2 in 0x00),
			&Sp80Inst::Or(ref r1, ref r2) => pack!(w, 0xf9, regs r1, r2 in 0x80),
			&Sp80Inst::Xor(ref r1, ref r2) => pack!(w, 0xfa, regs r1, r2 in 0x00),
			&Sp80Inst::Lsl(ref r1, ref r2) => pack!(w, 0xfc, regs r1, r2 in 0x00),
			&Sp80Inst::Lsr(ref r1, ref r2) => pack!(w, 0xfc, regs r1, r2 in 0x80),
			&Sp80Inst::Asr(ref r1, ref r2) => pack!(w, 0xfd, regs r1, r2 in 0x80),
			&Sp80Inst::Not(ref r) => pack!(w, reg r in 0xd0),
			&Sp80Inst::Comp(ref r) => pack!(w, reg r in 0xd8),
			&Sp80Inst::Inc(ref r) => pack!(w, reg r in 0xe0),
			&Sp80Inst::Incw(ref a) => pack!(w, reg a in 0xf0),
			&Sp80Inst::Dec(ref r) => pack!(w, reg r in 0xe8),
			&Sp80Inst::Decw(ref a) => pack!(w, reg a in 0xf4),
			&Sp80Inst::Mov(ref r1, ref r2) => pack!(w, 0x78, regs r1, r2 in 0x00),
			&Sp80Inst::Ld(ref r, ref a) => pack!(w, 0x79, regs r, a in 0x00),
			&Sp80Inst::St(ref a, ref r) => pack!(w, 0x7a, regs a, r in 0x00),
			&Sp80Inst::Ldd(ref r, Addr(a)) => pack!(w, reg r in 0x40, word a),
			&Sp80Inst::Std(Addr(a), ref r) => pack!(w, reg r in 0x48, word a),
			&Sp80Inst::Ldi(ref r, Immediate(k)) => pack!(w, reg r in 0x50, byte k),
			&Sp80Inst::Ldsp(ref r) => pack!(w, reg r in 0x58),
			&Sp80Inst::Push(ref r) => pack!(w, reg r in 0x60),
			&Sp80Inst::Pop(ref r) => pack!(w, reg r in 0x70),
			&Sp80Inst::Je(RelAddr(o)) => pack!(w, offset o in 0x80),
			&Sp80Inst::Jne(RelAddr(o)) => pack!(w, offset o in 0x84),
			&Sp80Inst::Jl(RelAddr(o)) => pack!(w, offset o in 0x88),
			&Sp80Inst::Jge(RelAddr(o)) => pack!(w, offset o in 0x8c),
			&Sp80Inst::Jcc(RelAddr(o)) => pack!(w, offset o in 0x90),
			&Sp80Inst::Jcs(RelAddr(o)) => pack!(w, offset o in 0x94),
			&Sp80Inst::Jvc(RelAddr(o)) => pack!(w, offset o in 0x98),
			&Sp80Inst::Jvs(RelAddr(o)) => pack!(w, offset o in 0x9c),
			&Sp80Inst::Jmp(Addr(a)) => pack!(w, word a in 0xa0),
			&Sp80Inst::Rjmp(RelAddr(o)) => pack!(w, offset o in 0xa4),
			&Sp80Inst::Ijmp(ref a) => pack!(w, reg a in 0xa8),
			&Sp80Inst::Call(Addr(a)) => pack!(w, word a in 0xac),
			&Sp80Inst::Rcall(RelAddr(o)) => pack!(w, offset o in 0xb0),
			&Sp80Inst::Icall(ref a) => pack!(w, reg a in 0xb4),
			&Sp80Inst::Ret => pack!(w, 0xb8),
			&Sp80Inst::Reti => pack!(w, 0xbc),
			&Sp80Inst::Nop => pack!(w, 0x00),
			&Sp80Inst::Halt => pack!(w, 0x15),
		}
	}
}
