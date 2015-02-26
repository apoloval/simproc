//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::num::ParseIntError;
use std::error::FromError;
use std::str::FromStr;

use simproc::sp80::*;

use asm::assembly::SymbolTable;

pub enum ArgAssemblyError {
	ParseInt(ParseIntError),
	BadReg(String),
	BadAddrReg(String),
	Unknown
}

pub struct ArgAssemble<'a> {
	symbols: &'a SymbolTable,
	location: usize,
}

impl<'a> ArgAssemble<'a> {

	pub fn with_symbols(symbols: &'a SymbolTable) -> ArgAssemble<'a> {
		ArgAssemble { symbols: symbols, location: 0 }
	}

	pub fn with_location(&mut self, loc: usize) -> &ArgAssemble {
		self.location = loc;
		self
	}
}

impl<'a> ArgMap<AssemblyArgs, RuntimeArgs, ArgAssemblyError> for ArgAssemble<'a> {

	fn map_immediate(&self, src: &String) -> Result<Immediate, ArgAssemblyError> {
		match FromStr::from_str(&src[..]) {
			Ok(k) => Ok(Immediate(k)),
			Err(e) => Err(ArgAssemblyError::ParseInt(e))
		}
	}

	fn map_addr(&self, src: &String) -> Result<Addr, ArgAssemblyError> {
		match FromStr::from_str(&src[..]) {
			Ok(k) => Ok(Addr(k)),
			Err(e) => Err(ArgAssemblyError::ParseInt(e))
		}
	}

	fn map_rel_addr(&self, src: &String) -> Result<RelAddr, ArgAssemblyError> {
		match FromStr::from_str(&src[..]) {
			Ok(k) => Ok(RelAddr(k)),
			Err(e) => Err(ArgAssemblyError::ParseInt(e))
		}
	}

	fn map_reg(&self, src: &String) -> Result<Reg, ArgAssemblyError> {
		match &src[..] {
			"r0" | "R0" => Ok(Reg::R0),
			"r1" | "R1" => Ok(Reg::R1),
			"r2" | "R2" => Ok(Reg::R2),
			"r3" | "R3" => Ok(Reg::R3),
			"r4" | "R4" => Ok(Reg::R4),
			"r5" | "R5" => Ok(Reg::R5),
			"r6" | "R6" => Ok(Reg::R6),
			"r7" | "R7" => Ok(Reg::R7),
			_ => Err(ArgAssemblyError::BadReg(src.clone()))
		}
	}

	fn map_addr_reg(&self, src: &String) -> Result<AddrReg, ArgAssemblyError> {
		match &src[..] {
			"a0" | "A0" => Ok(AddrReg::A0),
			"a1" | "A1" => Ok(AddrReg::A1),
			"a2" | "A2" => Ok(AddrReg::A2),
			"a3" | "A3" => Ok(AddrReg::A3),
			_ => Err(ArgAssemblyError::BadAddrReg(src.clone()))
		}
	}
}
