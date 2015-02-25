//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use asm::assembly::*;

pub struct MnemoArg<'a> {
	pub expr: &'a str,
	pub symbols: &'a SymbolTable,
	pub placement: usize,
}

pub trait FromMnemoArg {
	type Err;
	fn from_mnemo_arg(arg: &MnemoArg) -> Result<Self, Self::Err>;
}

#[macro_export]
macro_rules! assemble_mnemo(
	($i:path => nullary from $a:expr) => ({
		if $a.len() != 0 { return Err(format!("expected 0 arguments, {} given", $a.len())) }
		Ok($i)
	});
	($i:path => unary from $a:expr, $s:expr, $p:expr) => ({
		if $a.len() != 1 { return Err(format!("expected 1 arguments, {} given", $a.len())) }
		let arg = MnemoArg { expr: &$a[0][..], symbols: $s, placement: $p };
		let target = match FromMnemoArg::from_mnemo_arg(&arg) {
			Ok(r) => r,
			Err(e) => return Err(format!("invalid argument: {}", e)),
		};
		Ok($i(target))
	});
	($i:path => binary from $a:expr, $s:expr, $p:expr) => ({
		if $a.len() != 2 { return Err(format!("expected 2 arguments, {} given", $a.len())) }
		let arg1 = MnemoArg { expr: &$a[0][..], symbols: $s, placement: $p };
		let dest = match FromMnemoArg::from_mnemo_arg(&arg1) {
			Ok(r) => r,
			Err(e) => return Err(format!("invalid destination argument: {}", e)),
		};
		let arg2 = MnemoArg { expr: &$a[1][..], symbols: $s, placement: $p };
		let src = match FromMnemoArg::from_mnemo_arg(&arg2) {
			Ok(r) => r,
			Err(e) => return Err(format!("invalid source argument: {}", e)),
		};
		Ok($i(dest, src))
	});
);
