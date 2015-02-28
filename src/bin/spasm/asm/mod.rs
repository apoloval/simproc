//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate simproc;

mod parser;

pub mod assembly;
pub mod sp80;

use std::io;

use simproc::inst::Encode;

pub use self::assembly::*;

pub trait Assembler<I: Encode> {

	fn assemble<R : io::Read>(&self, input: R) -> Result<Assembly<I>, AssemblyError>;
}

#[cfg(test)]
mod parser_test;
