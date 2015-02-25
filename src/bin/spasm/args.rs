//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::os;

pub struct Options {
	pub input: String,
}

pub fn parse_args() -> Result<Options, String> {
	let args = os::args();
	match args.len() {
		2 => Ok(Options { input: args[1].to_string() }),
		_ => Err("invalid arguments".to_string())
	}
}
