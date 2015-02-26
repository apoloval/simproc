//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::str::FromStr;

use ::Encode;
use sp80::*;

#[test]
fn should_immediate_from_str() {
	assert_eq!(Immediate(80), FromStr::from_str("80").ok().unwrap());
}

#[test]
fn should_immediate_from_hex_str() {
	assert_eq!(Immediate(0x10), FromStr::from_str("0x10").ok().unwrap());
}

#[test]
fn should_immediate_from_neg_str() {
	assert_eq!(Immediate(-80), FromStr::from_str("-80").ok().unwrap());
}

#[test]
fn should_immediate_from_neg_hex_str() {
	assert_eq!(Immediate(-0x10), FromStr::from_str("-0x10").ok().unwrap());
}

#[test]
fn should_fail_immediate_from_too_large_str() {
	let r: Result<Immediate, String> = FromStr::from_str("8000");
	assert!(r.err().unwrap().starts_with("invalid decimal value in `8000`:"));
}

#[test]
fn should_fail_immediate_from_too_large_hex_str() {
	let r: Result<Immediate, String> = FromStr::from_str("0x100");
	assert!(r.err().unwrap().starts_with("invalid hexadecimal value in `0x100`:"));
}

#[test]
fn should_addr_from_str() {
	assert_eq!(Addr(0x1234), FromStr::from_str("4660").ok().unwrap());
}

#[test]
fn should_addr_from_byte_str() {
	assert_eq!(Addr(0x12), FromStr::from_str("18").ok().unwrap());
}

#[test]
fn should_fail_addr_from_negative_str() {
	let r: Result<Addr, String> = FromStr::from_str("-18");
	assert!(r.err().unwrap().starts_with("invalid negative value in `-18`"));
}

#[test]
fn should_addr_from_hex_str() {
	assert_eq!(Addr(0x1234), FromStr::from_str("0x1234").ok().unwrap());
}

#[test]
fn should_addr_from_hex_byte_str() {
	assert_eq!(Addr(0x12), FromStr::from_str("0x12").ok().unwrap());
}

#[test]
fn should_fail_addr_from_negative_hex_str() {
	let r: Result<Addr, String> = FromStr::from_str("-0x12");
	assert!(r.err().unwrap().starts_with("invalid negative value in `-0x12`"));
}

#[test]
fn should_reladdr_from_str() {
	assert_eq!(RelAddr(0x1234), FromStr::from_str("4660").ok().unwrap());
}

#[test]
fn should_reladdr_from_byte_str() {
	assert_eq!(RelAddr(0x12), FromStr::from_str("18").ok().unwrap());
}

#[test]
fn should_reladdr_from_negative_str() {
	assert_eq!(RelAddr(-0x12), FromStr::from_str("-18").ok().unwrap());
}

#[test]
fn should_reladdr_from_hex_str() {
	assert_eq!(RelAddr(0x1234), FromStr::from_str("0x1234").ok().unwrap());
}

#[test]
fn should_reladdr_from_hex_byte_str() {
	assert_eq!(RelAddr(0x12), FromStr::from_str("0x12").ok().unwrap());
}

#[test]
fn should_reladdr_from_negative_hex_str() {
	assert_eq!(RelAddr(-0x1234), FromStr::from_str("-0x1234").ok().unwrap());
}

fn assert_encode(inst: Inst, bytes: &[u8]) {
	let mut w: Vec<u8> = Vec::with_capacity(16);
	let result = inst.encode(&mut w);
	assert!(result.is_ok());
	assert_eq!(bytes.len(), result.ok().unwrap());
	assert_eq!(&w[..], bytes);
}	

#[test]
fn encode_add() { assert_encode(Inst::Add(Reg::R3, Reg::R5), &[0xf8, 0x1d]); }

#[test]
fn encode_addw() { assert_encode(Inst::Addw(AddrReg::A3, AddrReg::A2), &[0xf8, 0x5a]); }

#[test]
fn encode_addi() { assert_encode(Inst::Addi(Reg::R3, Immediate(100)), &[0xc3, 0x64]); }

#[test]
fn encode_sub() { assert_encode(Inst::Sub(Reg::R3, Reg::R5), &[0xf8, 0x9d]); }

#[test]
fn encode_subw() { assert_encode(Inst::Subw(AddrReg::A3, AddrReg::A2), &[0xf8, 0xda]); }

#[test]
fn encode_subi() { assert_encode(Inst::Subi(Reg::R3, Immediate(100)), &[0xcb, 0x64]); }

#[test]
fn encode_mulw() { assert_encode(Inst::Mulw(AddrReg::A3, AddrReg::A2), &[0xf8, 0x5a]); }

#[test]
fn encode_and() { assert_encode(Inst::And(Reg::R3, Reg::R5), &[0xf9, 0x1d]); }

#[test]
fn encode_or() { assert_encode(Inst::Or(Reg::R3, Reg::R5), &[0xf9, 0x9d]); }

#[test]
fn encode_xor() { assert_encode(Inst::Xor(Reg::R1, Reg::R7), &[0xfa, 0x0f]); }

#[test]
fn encode_lsl() { assert_encode(Inst::Lsl(Reg::R3, Reg::R4), &[0xfc, 0x1c]); }

#[test]
fn encode_lsr() { assert_encode(Inst::Lsr(Reg::R6, Reg::R1), &[0xfc, 0xb1]); }

#[test]
fn encode_asr() { assert_encode(Inst::Asr(Reg::R1, Reg::R2), &[0xfd, 0x8a]); }

#[test]
fn encode_not() { assert_encode(Inst::Not(Reg::R5), &[0xd5]); }

#[test]
fn encode_comp() { assert_encode(Inst::Comp(Reg::R1), &[0xd9]); }

#[test]
fn encode_inc() { assert_encode(Inst::Inc(Reg::R6), &[0xe6]); }

#[test]
fn encode_incw() { assert_encode(Inst::Incw(AddrReg::A2), &[0xf2]); }

#[test]
fn encode_dec() { assert_encode(Inst::Dec(Reg::R7), &[0xef]); }

#[test]
fn encode_decw() { assert_encode(Inst::Decw(AddrReg::A1), &[0xf5]); }

#[test]
fn encode_mov() { assert_encode(Inst::Mov(Reg::R6, Reg::R2), &[0x78, 0x32]); }

#[test]
fn encode_ld() { assert_encode(Inst::Ld(Reg::R4, AddrReg::A1), &[0x79, 0x21]); }

#[test]
fn encode_st() { assert_encode(Inst::St(AddrReg::A3, Reg::R2), &[0x7a, 0x1a]); }

#[test]
fn encode_ldd() { assert_encode(Inst::Ldd(Reg::R1, Addr(0x2010)), &[0x41, 0x10, 0x20]); }

#[test]
fn encode_std() { assert_encode(Inst::Std(Addr(0x1020), Reg::R6), &[0x4e, 0x20, 0x10]); }

#[test]
fn encode_ldi() { assert_encode(Inst::Ldi(Reg::R2, Immediate(-13)), &[0x52, 0xf3]); }

#[test]
fn encode_ldsp() { assert_encode(Inst::Ldsp(AddrReg::A1), &[0x59]); }

#[test]
fn encode_push() { assert_encode(Inst::Push(Reg::R3), &[0x63]); }

#[test]
fn encode_pop() { assert_encode(Inst::Pop(Reg::R5), &[0x75]); }

#[test]
fn encode_je() { assert_encode(Inst::Je(RelAddr(1)), &[0x80, 0x01]); }

#[test]
fn encode_jne() { assert_encode(Inst::Jne(RelAddr(-1)), &[0x87, 0xff]); }

#[test]
fn encode_jl() { assert_encode(Inst::Jl(RelAddr(2)), &[0x88, 0x02]); }

#[test]
fn encode_jge() { assert_encode(Inst::Jge(RelAddr(-2)), &[0x8f, 0xfe]); }

#[test]
fn encode_jcc() { assert_encode(Inst::Jcc(RelAddr(3)), &[0x90, 0x03]); }

#[test]
fn encode_jcs() { assert_encode(Inst::Jcs(RelAddr(-3)), &[0x97, 0xfd]); }

#[test]
fn encode_jvc() { assert_encode(Inst::Jvc(RelAddr(4)), &[0x98, 0x04]); }

#[test]
fn encode_jvs() { assert_encode(Inst::Jvs(RelAddr(-4)), &[0x9f, 0xfc]); }

#[test]
fn encode_jmp() { assert_encode(Inst::Jmp(Addr(0x4321)), &[0xa0, 0x21, 0x43]); }

#[test]
fn encode_rjmp() { assert_encode(Inst::Rjmp(RelAddr(100)), &[0xa4, 0x64]); }

#[test]
fn encode_ijmp() { assert_encode(Inst::Ijmp(AddrReg::A3), &[0xab]); }

#[test]
fn encode_call() { assert_encode(Inst::Call(Addr(0x1234)), &[0xac, 0x34, 0x12]); }

#[test]
fn encode_rcall() { assert_encode(Inst::Rcall(RelAddr(-100)), &[0xb3, 0x9c]); }

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
