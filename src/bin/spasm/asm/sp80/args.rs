//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::error::Error;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use simproc::sp80::*;

use asm::assembly::SymbolTable;

pub enum ArgAssemblyError {
	ParseInt(String, ParseIntError),
	BadReg(String),
	BadAddrReg(String),
}

impl fmt::Display for ArgAssemblyError {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
		match self {
			&ArgAssemblyError::ParseInt(ref expr, ref cause) => 
				write!(fmt, "invalid numeric expression in `{}`: {}", expr, cause),
			&ArgAssemblyError::BadReg(ref expr) => 
				write!(fmt, "invalid register `{}`", expr),
			&ArgAssemblyError::BadAddrReg(ref expr) => 
				write!(fmt, "invalid address register `{}`", expr),			
		}
	}
}

impl Error for ArgAssemblyError {
	fn description(&self) -> &str {
		match self {
			&ArgAssemblyError::ParseInt(ref expr, ref cause) => "invalid numeric expression",
			&ArgAssemblyError::BadReg(ref expr) => "invalid register",
			&ArgAssemblyError::BadAddrReg(ref expr) => "invalid address register",
		}
	}
}

pub struct ArgAssembler<'a> {
	symbols: &'a SymbolTable,
	location: usize,
}

impl<'a> ArgAssembler<'a> {

	pub fn with_symbols(symbols: &'a SymbolTable) -> ArgAssembler<'a> {
		ArgAssembler { symbols: symbols, location: 0 }
	}

	pub fn with_location(&mut self, loc: usize) -> &ArgAssembler {
		self.location = loc;
		self
	}
}

impl<'a> ArgMap<AssemblyArgs, RuntimeArgs, ArgAssemblyError> for ArgAssembler<'a> {

	fn map_immediate(&self, src: &String) -> Result<Immediate, ArgAssemblyError> {
		match FromStr::from_str(&src[..]) {
			Ok(k) => Ok(Immediate(k)),
			Err(e) => Err(ArgAssemblyError::ParseInt(src.clone(), e))
		}
	}

	fn map_addr(&self, src: &String) -> Result<Addr, ArgAssemblyError> {
		match FromStr::from_str(&src[..]) {
			Ok(k) => Ok(Addr(k)),
			Err(e) => Err(ArgAssemblyError::ParseInt(src.clone(), e))
		}
	}

	fn map_rel_addr(&self, src: &String) -> Result<RelAddr, ArgAssemblyError> {
		match FromStr::from_str(&src[..]) {
			Ok(k) => Ok(RelAddr(k)),
			Err(e) => Err(ArgAssemblyError::ParseInt(src.clone(), e))
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
