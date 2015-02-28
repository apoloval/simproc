//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::io;

use simproc::inst::Encode;

use asm::assembly::*;

pub trait Assembler<I: Encode> {

	fn assemble<R : io::Read>(&self, input: R) -> Result<Assembly<I>, AssemblyError>;
}
