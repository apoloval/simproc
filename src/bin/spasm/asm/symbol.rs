//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashMap;

pub type SymbolTable = HashMap<String, i64>;

macro_rules! symbols {
	($($k:expr => $v:expr),*) => ({
		let mut symbols = $crate::asm::symbol::SymbolTable::new();
		$(
			symbols.insert($k.to_string(), $v);
		)*
		symbols
	});
}