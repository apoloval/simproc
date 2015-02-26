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
use sp80::args::*;

/// A SP-80 instruction
pub enum Inst<A: Args> {

	// Arithmetic-logic instructions
	Add(A::Reg, A::Reg),
	Addw(A::AddrReg, A::AddrReg),
	Addi(A::Reg, A::Immediate),
	Sub(A::Reg, A::Reg),
	Subw(A::AddrReg, A::AddrReg),
	Subi(A::Reg, A::Immediate),
	Mulw(A::AddrReg, A::AddrReg),
	And(A::Reg, A::Reg),
	Or(A::Reg, A::Reg),
	Xor(A::Reg, A::Reg),
	Lsl(A::Reg, A::Reg),
	Lsr(A::Reg, A::Reg),
	Asr(A::Reg, A::Reg),
	Not(A::Reg),
	Comp(A::Reg),
	Inc(A::Reg),
	Incw(A::AddrReg),
	Dec(A::Reg),
	Decw(A::AddrReg),

	// Load/store instructions
	Mov(A::Reg, A::Reg),
	Ld(A::Reg, A::AddrReg),
	St(A::AddrReg, A::Reg),
	Ldd(A::Reg, A::Addr),
	Std(A::Addr, A::Reg),
	Ldi(A::Reg, A::Immediate),
	Ldsp(A::AddrReg),
	Push(A::Reg),
	Pop(A::Reg),

	// Branching instructions
	Je(A::RelAddr),
	Jne(A::RelAddr),
	Jl(A::RelAddr),
	Jge(A::RelAddr),
	Jcc(A::RelAddr),
	Jcs(A::RelAddr),
	Jvc(A::RelAddr),
	Jvs(A::RelAddr),
	Jmp(A::Addr),
	Rjmp(A::RelAddr),
	Ijmp(A::AddrReg),
	Call(A::Addr),
	Rcall(A::RelAddr),
	Icall(A::AddrReg),
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

impl Encode for Inst<RuntimeArgs> {

	fn len(&self) -> usize {
		match self {
			&Inst::Add(_, _) => 2,
			&Inst::Addw(_, _) => 2,
			&Inst::Addi(_, _) => 2,
			&Inst::Sub(_, _) => 2,
			&Inst::Subw(_, _) => 2,
			&Inst::Subi(_, _) => 2,
			&Inst::Mulw(_, _) => 2,
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

	/// Encode a instruction using the given writer
	fn encode<W: io::Write>(&self, w: &mut W) -> io::Result<usize> {
		match self {
			&Inst::Add(ref r1, ref r2) => pack!(w, 0xf8, regs r1, r2 in 0x00),
			&Inst::Addw(ref a1, ref a2) => pack!(w, 0xf8, regs a1, a2 in 0x40),
			&Inst::Addi(ref r, Immediate(k)) => pack!(w, reg r in 0xc0, byte k),
			&Inst::Sub(ref r1, ref r2) => pack!(w, 0xf8, regs r1, r2 in 0x80),
			&Inst::Subw(ref a1, ref a2) => pack!(w, 0xf8, regs a1, a2 in 0xc0),
			&Inst::Subi(ref r, Immediate(k)) => pack!(w, reg r in 0xc8, byte k),
			&Inst::Mulw(ref a1, ref a2) => pack!(w, 0xf8, regs a1, a2 in 0x40),
			&Inst::And(ref r1, ref r2) => pack!(w, 0xf9, regs r1, r2 in 0x00),
			&Inst::Or(ref r1, ref r2) => pack!(w, 0xf9, regs r1, r2 in 0x80),
			&Inst::Xor(ref r1, ref r2) => pack!(w, 0xfa, regs r1, r2 in 0x00),
			&Inst::Lsl(ref r1, ref r2) => pack!(w, 0xfc, regs r1, r2 in 0x00),
			&Inst::Lsr(ref r1, ref r2) => pack!(w, 0xfc, regs r1, r2 in 0x80),
			&Inst::Asr(ref r1, ref r2) => pack!(w, 0xfd, regs r1, r2 in 0x80),
			&Inst::Not(ref r) => pack!(w, reg r in 0xd0),
			&Inst::Comp(ref r) => pack!(w, reg r in 0xd8),
			&Inst::Inc(ref r) => pack!(w, reg r in 0xe0),
			&Inst::Incw(ref a) => pack!(w, reg a in 0xf0),
			&Inst::Dec(ref r) => pack!(w, reg r in 0xe8),
			&Inst::Decw(ref a) => pack!(w, reg a in 0xf4),
			&Inst::Mov(ref r1, ref r2) => pack!(w, 0x78, regs r1, r2 in 0x00),
			&Inst::Ld(ref r, ref a) => pack!(w, 0x79, regs r, a in 0x00),
			&Inst::St(ref a, ref r) => pack!(w, 0x7a, regs a, r in 0x00),
			&Inst::Ldd(ref r, Addr(a)) => pack!(w, reg r in 0x40, word a),
			&Inst::Std(Addr(a), ref r) => pack!(w, reg r in 0x48, word a),
			&Inst::Ldi(ref r, Immediate(k)) => pack!(w, reg r in 0x50, byte k),
			&Inst::Ldsp(ref r) => pack!(w, reg r in 0x58),
			&Inst::Push(ref r) => pack!(w, reg r in 0x60),
			&Inst::Pop(ref r) => pack!(w, reg r in 0x70),
			&Inst::Je(RelAddr(o)) => pack!(w, offset o in 0x80),
			&Inst::Jne(RelAddr(o)) => pack!(w, offset o in 0x84),
			&Inst::Jl(RelAddr(o)) => pack!(w, offset o in 0x88),
			&Inst::Jge(RelAddr(o)) => pack!(w, offset o in 0x8c),
			&Inst::Jcc(RelAddr(o)) => pack!(w, offset o in 0x90),
			&Inst::Jcs(RelAddr(o)) => pack!(w, offset o in 0x94),
			&Inst::Jvc(RelAddr(o)) => pack!(w, offset o in 0x98),
			&Inst::Jvs(RelAddr(o)) => pack!(w, offset o in 0x9c),
			&Inst::Jmp(Addr(a)) => pack!(w, word a in 0xa0),
			&Inst::Rjmp(RelAddr(o)) => pack!(w, offset o in 0xa4),
			&Inst::Ijmp(ref a) => pack!(w, reg a in 0xa8),
			&Inst::Call(Addr(a)) => pack!(w, word a in 0xac),
			&Inst::Rcall(RelAddr(o)) => pack!(w, offset o in 0xb0),
			&Inst::Icall(ref a) => pack!(w, reg a in 0xb4),
			&Inst::Ret => pack!(w, 0xb8),
			&Inst::Reti => pack!(w, 0xbc),
			&Inst::Nop => pack!(w, 0x00),
			&Inst::Halt => pack!(w, 0x15),
		}
	}
}
