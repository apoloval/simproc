//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::error::Error;
use std::fmt;
use std::num::ToPrimitive;

use simproc::sp80::*;

use asm::assembly::SymbolTable;
use asm::parser;

pub enum ArgAssemblyError {
	BadNumber(String),
	OutOfRange(String),
	BadReg(String),
	BadAddrReg(String),
}

impl fmt::Display for ArgAssemblyError {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
		match self {
			&ArgAssemblyError::BadNumber(ref expr) => 
				write!(fmt, "invalid numeric expression in `{}`", expr),
			&ArgAssemblyError::OutOfRange(ref expr) => 
				write!(fmt, "number `{}` is out of range", expr),
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
			&ArgAssemblyError::BadNumber(_) => "invalid numeric expression",
			&ArgAssemblyError::OutOfRange(_) => "number out of range",
			&ArgAssemblyError::BadReg(_) => "invalid register",
			&ArgAssemblyError::BadAddrReg(_) => "invalid address register",
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

	pub fn set_location(&mut self, loc: usize) { self.location = loc; }
}

macro_rules! parse_num(
	($s:expr, $c:ident) => ({
		let num = match parser::parse_num(&$s[..]) {
			Some(n) => n,
			None => return Err(ArgAssemblyError::BadNumber($s.clone())),
		};
		match num.$c() {
			Some(k) => Ok(k),
			None => Err(ArgAssemblyError::OutOfRange($s.clone())),
		}
	});
);

impl<'a> ArgMap<AssemblyArgs, RuntimeArgs, ArgAssemblyError> for ArgAssembler<'a> {

	fn map_immediate(&self, src: &String) -> Result<Immediate, ArgAssemblyError> {
		parse_num!(src, to_u8).map(|n| Immediate(n))
	}

	fn map_addr(&self, src: &String) -> Result<Addr, ArgAssemblyError> {
		parse_num!(src, to_u16).map(|n| Addr(n))
	}

	fn map_rel_addr(&self, src: &String) -> Result<RelAddr, ArgAssemblyError> {
		parse_num!(src, to_i16).map(|n| RelAddr(n))
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
