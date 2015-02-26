//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate serialize;

use std::io;
use std::marker::MarkerTrait;
use std::mem;
use std::num::{Int};
use std::str::FromStr;

use self::serialize::hex::FromHex;

/// An immediate value that comes after an opcode. 
#[derive(Debug, PartialEq)]
pub struct Immediate(pub u8);

macro_rules! int_from_str(
	($s:expr => $t:ty) => (
		if $s.starts_with("0x") || $s.starts_with("-0x") { int_from_str!($s => $t as hex) } 
		else { int_from_str!($s => $t as dec) }
	);
	($s:expr => $t:ty as pos) => (
		if $s.starts_with("0x") || $s.starts_with("-0x") { int_from_str!($s => $t as pos hex) } 
		else { int_from_str!($s => $t as pos dec) }
	);
	($s:expr => $t:ty as pos dec) => ({
		if $s.starts_with("-") { Err(format!("invalid negative value in `{}`", $s)) }
		else { int_from_str!($s => $t as dec) }
	});
	($s:expr => $t:ty as dec) => ({
		let (range, sign) = if $s.starts_with("-") { (1.., -1) } else { (0.., 1) };
		let k: $t = match FromStr::from_str(&$s[range]) {
			Ok(k) => k,
			Err(e) => return Err(format!("invalid decimal value in `{}`: {}", $s, e)),
		};
		Ok((k * sign) as $t)
	});
	($s:expr => $t:ty as pos hex) => ({
		if $s.starts_with("-") { Err(format!("invalid negative value in `{}`", $s)) }
		else { int_from_str!($s => $t as hex) }
	});
	($s:expr => $t:ty as hex) => ({
		let (range, sign) = if $s.starts_with("0x") { (2.., 1) }
			else if $s.starts_with("-0x") { (3.., -1) }
			else if $s.starts_with("-") { (0.., -1) }
			else { (0.., 1) };
		match $s[range].from_hex() {
			Ok(v) => {
				let type_len = mem::size_of::<$t>();
				let bytes_len = v.len();
				if bytes_len <= type_len {
					let mut k = 0 as $t;
					let mut i = 8*(type_len - bytes_len);
					for b in v.iter() {
						k |= ((*b as $t) << i) as $t;
						i += 8;
					}
					Ok((Int::from_be(k) * sign) as $t) 

				}
				else { Err(format!("invalid hexadecimal value in `{}`: number out of range", $s)) }
			},
			Err(e) => Err(format!("invalid hexadecimal value in `{}`: {}", $s, e)),
		}
	});
);

impl FromStr for Immediate {
	type Err = String;
	fn from_str(s: &str) -> Result<Immediate, String> {
		int_from_str!(s => u8).map(|k| Immediate(k)) 
	}
}

/// An address in SP-80 of 16-bits
#[derive(Debug, PartialEq)]
pub struct Addr(pub u16);

impl FromStr for Addr {
	type Err = String;
	fn from_str(s: &str) -> Result<Addr, String> {
		int_from_str!(s => u16 as pos).map(|k| Addr(k)) 
	}
}

/// A relative address, i.e. a delta respect the current PC. 
#[derive(Debug, PartialEq)]
pub struct RelAddr(pub i16);

impl FromStr for RelAddr {
	type Err = String;
	fn from_str(s: &str) -> Result<RelAddr, String> {
		int_from_str!(s => i16).map(|k| RelAddr(k)) 
	}
}

/// General purpose 8-bit Regs. 
pub enum Reg { R0, R1, R2, R3, R4, R5, R6, R7 }

impl FromStr for Reg {
	type Err = String;
	fn from_str(s: &str) -> Result<Reg, String> {
		match s {
			"r0" | "R0" => Ok(Reg::R0),
			"r1" | "R1" => Ok(Reg::R1),
			"r2" | "R2" => Ok(Reg::R2),
			"r3" | "R3" => Ok(Reg::R3),
			"r4" | "R4" => Ok(Reg::R4),
			"r5" | "R5" => Ok(Reg::R5),
			"r6" | "R6" => Ok(Reg::R6),
			"r7" | "R7" => Ok(Reg::R7),
			_ => Err(format!("invalid register name `{}`", s))
		}
	}
}

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

impl FromStr for AddrReg {
	type Err = String;
	fn from_str(s: &str) -> Result<AddrReg, String> {
		match s {
			"a0" | "A0" => Ok(AddrReg::A0),
			"a1" | "A1" => Ok(AddrReg::A1),
			"a2" | "A2" => Ok(AddrReg::A2),
			"a3" | "A3" => Ok(AddrReg::A3),
			_ => Err(format!("invalid address register name `{}`", s))
		}
	}
}

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

pub trait Args : MarkerTrait {
	type Immediate;
	type Addr;
	type RelAddr;
	type Reg;
	type AddrReg;
}

pub struct RuntimeArgs;

impl Args for RuntimeArgs {
	type Immediate = Immediate;
	type Addr = Addr;
	type RelAddr = RelAddr;
	type Reg = Reg;
	type AddrReg = AddrReg;
}

pub struct AssemblyArgs;

impl Args for AssemblyArgs {
	type Immediate = String;
	type Addr = String;
	type RelAddr = String;
	type Reg = String;
	type AddrReg = String;
}

