//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::old_io as io;
use std::num::{Int};

/// An immediate value that comes after an opcode. 
pub type Immediate = i8;

/// An address in SP-80 of 16-bits
pub type Addr = u16;

/// A relative address, i.e. a delta respect the current PC. 
pub type RelAddr = i16;

/// General purpose 8-bit Regs. 
pub enum Reg { R0, R1, R2, R3, R4, R5, R6, R7 }

impl Reg {

	/// Encode a general purpose register into its binary representation
	pub fn encode(&self) -> u8 {
		match self {
			&Reg::R0 => 0x00,
			&Reg::R1 => 0x01,
			&Reg::R2 => 0x02,
			&Reg::R3 => 0x03,
			&Reg::R4 => 0x04,
			&Reg::R5 => 0x05,
			&Reg::R6 => 0x06,
			&Reg::R7 => 0x07,
		}
	}
}

/// 16-bits address Regs. 
pub enum AddrReg { A0, A1, A2, A3 }

impl AddrReg {
	
	/// Encode a general purpose register into its binary representation
	pub fn encode(&self) -> u8 {
		match self {
			&AddrReg::A0 => 0x00,
			&AddrReg::A1 => 0x01,
			&AddrReg::A2 => 0x02,
			&AddrReg::A3 => 0x03,
		}
	}
}

/// A SP-80 instruction
pub enum Instruction {

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

/// A encoding error
enum EncError {
	IoError(io::IoError),
}

/// The result of an encoding operation
pub type EncResult = Option<EncError>;

impl Instruction {

	fn enc<W: Writer>(w: &mut W, byte: u8) -> EncResult {
		match w.write_u8(byte) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}	

	fn enc_with_reg<W: Writer>(w: &mut W, byte: u8, reg: &Reg) -> EncResult {
		match w.write_u8(byte | reg.encode()) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_areg<W: Writer>(w: &mut W, byte: u8, areg: &AddrReg) -> EncResult {
		match w.write_u8(byte | areg.encode()) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_reg_k<W: Writer>(w: &mut W, byte: u8, reg: &Reg, k: i8) -> EncResult {
		match w.write_all(&[byte | reg.encode(), k as u8]) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_reg_word<W: Writer>(w: &mut W, byte: u8, reg: &Reg, word: u16) -> EncResult {
		let l = (word.to_be() >> 8) as u8;
		let r = word.to_be() as u8;
		match w.write_all(&[byte | reg.encode(), l, r]) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_areg_reg<W: Writer>(w: &mut W, byte: u8, areg: &AddrReg, reg: &Reg) -> EncResult {
		let regs = reg.encode() | (areg.encode() << 3);
		match w.write_all(&[byte, regs]) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_reg_areg<W: Writer>(w: &mut W, byte: u8, reg: &Reg, areg: &AddrReg) -> EncResult {
		let regs = areg.encode() | (reg.encode() << 3);
		match w.write_all(&[byte, regs]) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_reg_reg<W: Writer>(w: &mut W, byte: u8, 
								   mask: u8, reg1: &Reg, reg2: &Reg) -> EncResult {
		let regs = mask | reg2.encode() | (reg1.encode() << 3);
		match w.write_all(&[byte, regs]) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_areg_areg<W: Writer>(w: &mut W, byte: u8, 
								   mask: u8, areg1: &AddrReg, areg2: &AddrReg) -> EncResult {
		let regs = mask | areg2.encode() | (areg1.encode() << 3);
		match w.write_all(&[byte, regs]) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_word<W: Writer>(w: &mut W, byte: u8, word: u16) -> EncResult {
		let l = (word.to_be() >> 8) as u8;
		let r = word.to_be() as u8;
		match w.write_all(&[byte, l, r]) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	fn enc_with_offset<W: Writer>(w: &mut W, byte: u8, word: i16) -> EncResult {
		let bin = (word & 0x03ff).to_le();
		let l = (bin >> 8) as u8;
		let r = bin as u8;
		match w.write_all(&[byte | l, r]) {
			Ok(_) =>  None,
			Err(e) => Some(EncError::IoError(e)),
		}
	}

	/// Encode a instruction using the given writer
	pub fn encode<W: Writer>(&self, w: &mut W) -> EncResult {	
		match self {
			&Instruction::Add(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0xf8, 0x00, &r1, &r2),
			&Instruction::Addw(ref a1, ref a2) =>
				Instruction::enc_with_areg_areg(w, 0xf8, 0x40, &a1, &a2),
			&Instruction::Addi(ref r, k) =>
				Instruction::enc_with_reg_k(w, 0xc0, &r, k),
			&Instruction::Sub(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0xf8, 0x80, &r1, &r2),
			&Instruction::Subw(ref a1, ref a2) =>
				Instruction::enc_with_areg_areg(w, 0xf8, 0xc0, &a1, &a2), 
			&Instruction::Subi(ref r, k) =>
				Instruction::enc_with_reg_k(w, 0xc8, &r, k),
			&Instruction::Mulw(ref a1, ref a2) =>
				Instruction::enc_with_areg_areg(w, 0xf8, 0x40, &a1, &a2),
			&Instruction::And(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0xf9, 0x00, &r1, &r2),
			&Instruction::Or(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0xf9, 0x80, &r1, &r2),
			&Instruction::Xor(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0xfa, 0x00, &r1, &r2),
			&Instruction::Lsl(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0xfc, 0x00, &r1, &r2),
			&Instruction::Lsr(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0xfc, 0x80, &r1, &r2),
			&Instruction::Asr(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0xfd, 0x80, &r1, &r2),
			&Instruction::Not(ref r) =>
				Instruction::enc_with_reg(w, 0xd0, &r),
			&Instruction::Comp(ref r) =>
				Instruction::enc_with_reg(w, 0xd8, &r),
			&Instruction::Inc(ref r) =>
				Instruction::enc_with_reg(w, 0xe0, &r),
			&Instruction::Incw(ref a) =>
				Instruction::enc_with_areg(w, 0xf0, &a), 
			&Instruction::Dec(ref r) =>
				Instruction::enc_with_reg(w, 0xe8, &r),
			&Instruction::Decw(ref a) =>
				Instruction::enc_with_areg(w, 0xf4, &a), 
			&Instruction::Mov(ref r1, ref r2) =>
				Instruction::enc_with_reg_reg(w, 0x78, 0x00, &r1, &r2),
			&Instruction::Ld(ref r, ref a) =>
				Instruction::enc_with_reg_areg(w, 0x79, &r, &a),
			&Instruction::St(ref a, ref r) =>
				Instruction::enc_with_areg_reg(w, 0x7a, &a, &r),
			&Instruction::Ldd(ref r, a) =>
				Instruction::enc_with_reg_word(w, 0x40, &r, a),
			&Instruction::Std(a, ref r) =>
				Instruction::enc_with_reg_word(w, 0x48, &r, a),
			&Instruction::Ldi(ref r, k) =>
				Instruction::enc_with_reg_k(w, 0x50, &r, k),
			&Instruction::Ldsp(ref r) =>
				Instruction::enc_with_areg(w, 0x58, &r),
			&Instruction::Push(ref r) =>
				Instruction::enc_with_reg(w, 0x60, &r),
			&Instruction::Pop(ref r) =>
				Instruction::enc_with_reg(w, 0x70, &r),
			&Instruction::Je(o) =>
				Instruction::enc_with_offset(w, 0x80, o),
			&Instruction::Jne(o) =>
				Instruction::enc_with_offset(w, 0x84, o),
			&Instruction::Jl(o) =>
				Instruction::enc_with_offset(w, 0x88, o),
			&Instruction::Jge(o) =>
				Instruction::enc_with_offset(w, 0x8c, o),
			&Instruction::Jcc(o) =>
				Instruction::enc_with_offset(w, 0x90, o),
			&Instruction::Jcs(o) =>
				Instruction::enc_with_offset(w, 0x94, o),
			&Instruction::Jvc(o) =>
				Instruction::enc_with_offset(w, 0x98, o),
			&Instruction::Jvs(o) =>
				Instruction::enc_with_offset(w, 0x9c, o),
			&Instruction::Jmp(a) =>
				Instruction::enc_with_word(w, 0xa0, a),
			&Instruction::Rjmp(o) =>
				Instruction::enc_with_offset(w, 0xa4, o),
			&Instruction::Ijmp(ref a) =>
				Instruction::enc_with_areg(w, 0xa8, &a),
			&Instruction::Call(a) =>
				Instruction::enc_with_word(w, 0xac, a),
			&Instruction::Rcall(o) =>
				Instruction::enc_with_offset(w, 0xb0, o),
			&Instruction::Icall(ref a) =>
				Instruction::enc_with_areg(w, 0xb4, &a),
			&Instruction::Ret =>
				Instruction::enc(w, 0xb8),
			&Instruction::Reti =>
				Instruction::enc(w, 0xbc),
			&Instruction::Nop =>
				Instruction::enc(w, 0x00),
			&Instruction::Halt =>
				Instruction::enc(w, 0x15),
		}
	}
}

#[cfg(test)]
mod tests {

	use std::old_io::MemWriter;

	use super::*;

	fn assert_encode(inst: Instruction, bytes: &[u8]) {
		let mut w = MemWriter::new();
		inst.encode(&mut w);
		assert_eq!(w.into_inner(), bytes);
	}	

	#[test]
	fn encode_add() { assert_encode(Instruction::Add(Reg::R3, Reg::R5), &[0xf8, 0x1d]); }

	#[test]
	fn encode_addw() { assert_encode(Instruction::Addw(AddrReg::A3, AddrReg::A2), &[0xf8, 0x5a]); }

	#[test]
	fn encode_addi() { assert_encode(Instruction::Addi(Reg::R3, 100), &[0xc3, 0x64]); }

	#[test]
	fn encode_sub() { assert_encode(Instruction::Sub(Reg::R3, Reg::R5), &[0xf8, 0x9d]); }

	#[test]
	fn encode_subw() { assert_encode(Instruction::Subw(AddrReg::A3, AddrReg::A2), &[0xf8, 0xda]); }

	#[test]
	fn encode_subi() { assert_encode(Instruction::Subi(Reg::R3, 100), &[0xcb, 0x64]); }

	#[test]
	fn encode_mulw() { assert_encode(Instruction::Mulw(AddrReg::A3, AddrReg::A2), &[0xf8, 0x5a]); }

	#[test]
	fn encode_and() { assert_encode(Instruction::And(Reg::R3, Reg::R5), &[0xf9, 0x1d]); }

	#[test]
	fn encode_or() { assert_encode(Instruction::Or(Reg::R3, Reg::R5), &[0xf9, 0x9d]); }

	#[test]
	fn encode_xor() { assert_encode(Instruction::Xor(Reg::R1, Reg::R7), &[0xfa, 0x0f]); }

	#[test]
	fn encode_lsl() { assert_encode(Instruction::Lsl(Reg::R3, Reg::R4), &[0xfc, 0x1c]); }

	#[test]
	fn encode_lsr() { assert_encode(Instruction::Lsr(Reg::R6, Reg::R1), &[0xfc, 0xb1]); }

	#[test]
	fn encode_asr() { assert_encode(Instruction::Asr(Reg::R1, Reg::R2), &[0xfd, 0x8a]); }

	#[test]
	fn encode_not() { assert_encode(Instruction::Not(Reg::R5), &[0xd5]); }

	#[test]
	fn encode_comp() { assert_encode(Instruction::Comp(Reg::R1), &[0xd9]); }

	#[test]
	fn encode_inc() { assert_encode(Instruction::Inc(Reg::R6), &[0xe6]); }

	#[test]
	fn encode_incw() { assert_encode(Instruction::Incw(AddrReg::A2), &[0xf2]); }

	#[test]
	fn encode_dec() { assert_encode(Instruction::Dec(Reg::R7), &[0xef]); }

	#[test]
	fn encode_decw() { assert_encode(Instruction::Decw(AddrReg::A1), &[0xf5]); }

	#[test]
	fn encode_mov() { assert_encode(Instruction::Mov(Reg::R6, Reg::R2), &[0x78, 0x32]); }

	#[test]
	fn encode_ld() { assert_encode(Instruction::Ld(Reg::R4, AddrReg::A1), &[0x79, 0x21]); }

	#[test]
	fn encode_st() { assert_encode(Instruction::St(AddrReg::A3, Reg::R2), &[0x7a, 0x1a]); }

	#[test]
	fn encode_ldd() { assert_encode(Instruction::Ldd(Reg::R1, 0x2010), &[0x41, 0x10, 0x20]); }

	#[test]
	fn encode_std() { assert_encode(Instruction::Std(0x1020, Reg::R6), &[0x4e, 0x20, 0x10]); }

	#[test]
	fn encode_ldi() { assert_encode(Instruction::Ldi(Reg::R2, -13), &[0x52, 0xf3]); }

	#[test]
	fn encode_ldsp() { assert_encode(Instruction::Ldsp(AddrReg::A1), &[0x59]); }

	#[test]
	fn encode_push() { assert_encode(Instruction::Push(Reg::R3), &[0x63]); }

	#[test]
	fn encode_pop() { assert_encode(Instruction::Pop(Reg::R5), &[0x75]); }

	#[test]
	fn encode_je() { assert_encode(Instruction::Je(1), &[0x80, 0x01]); }

	#[test]
	fn encode_jne() { assert_encode(Instruction::Jne(-1), &[0x87, 0xff]); }

	#[test]
	fn encode_jl() { assert_encode(Instruction::Jl(2), &[0x88, 0x02]); }

	#[test]
	fn encode_jge() { assert_encode(Instruction::Jge(-2), &[0x8f, 0xfe]); }

	#[test]
	fn encode_jcc() { assert_encode(Instruction::Jcc(3), &[0x90, 0x03]); }

	#[test]
	fn encode_jcs() { assert_encode(Instruction::Jcs(-3), &[0x97, 0xfd]); }

	#[test]
	fn encode_jvc() { assert_encode(Instruction::Jvc(4), &[0x98, 0x04]); }

	#[test]
	fn encode_jvs() { assert_encode(Instruction::Jvs(-4), &[0x9f, 0xfc]); }

	#[test]
	fn encode_jmp() { assert_encode(Instruction::Jmp(0x4321), &[0xa0, 0x21, 0x43]); }

	#[test]
	fn encode_rjmp() { assert_encode(Instruction::Rjmp(100), &[0xa4, 0x64]); }

	#[test]
	fn encode_ijmp() { assert_encode(Instruction::Ijmp(AddrReg::A3), &[0xab]); }

	#[test]
	fn encode_call() { assert_encode(Instruction::Call(0x1234), &[0xac, 0x34, 0x12]); }

	#[test]
	fn encode_rcall() { assert_encode(Instruction::Rcall(-100), &[0xb3, 0x9c]); }

	#[test]
	fn encode_icall() { assert_encode(Instruction::Icall(AddrReg::A2), &[0xb6]); }

	#[test]
	fn encode_ret() { assert_encode(Instruction::Ret, &[0xb8]); }

	#[test]
	fn encode_reti() { assert_encode(Instruction::Reti, &[0xbc]); }

	#[test]
	fn encode_nop() { assert_encode(Instruction::Nop, &[0x00]); }

	#[test]
	fn encode_halt() { assert_encode(Instruction::Halt, &[0x15]); }
}
