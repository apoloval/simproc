//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::str::FromStr;

use simproc::sp80::*;

use asm::mnemo::*;

impl FromMnemoArg for Reg {
	type Err = String;
	fn from_mnemo_arg(arg: &MnemoArg) -> Result<Reg, String> { FromStr::from_str(arg.expr) }
}

impl FromMnemoArg for AddrReg {
	type Err = String;
	fn from_mnemo_arg(arg: &MnemoArg) -> Result<AddrReg, String> { FromStr::from_str(arg.expr) }
}

macro_rules! resolve_numeric(
	($e:expr => $nt:ty as $rt:ident : $s:expr) => ({
		let head = $e.char_at(0);
		if head.is_numeric() || head == '-' { FromStr::from_str($e).map(|r| (r, false)) }
		else { 
			match $s.get($e) {
				Some(k) => Ok(($rt(*k as $nt), true)),
				None => Err(format!("no symbol found for expression `{}`", $e)),
			}
		}
	});
);

impl FromMnemoArg for Immediate {
	type Err = String;
	fn from_mnemo_arg(arg: &MnemoArg) -> Result<Immediate, String> {
		resolve_numeric!(arg.expr => u8 as Immediate : arg.symbols).map(|r| r.0)
	}
}

impl FromMnemoArg for Addr {
	type Err = String;
	fn from_mnemo_arg(arg: &MnemoArg) -> Result<Addr, String> {
		resolve_numeric!(arg.expr => u16 as Addr : arg.symbols).map(|r| r.0)
	}
}

impl FromMnemoArg for RelAddr {
	type Err = String;
	fn from_mnemo_arg(arg: &MnemoArg) -> Result<RelAddr, String> {
		let (abs, is_symbol) = try!(resolve_numeric!(arg.expr => i16 as RelAddr : arg.symbols));
		if is_symbol { Ok(RelAddr(abs.0 - arg.placement as i16)) }
		else { Ok(abs) }
	}
}
