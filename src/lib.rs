//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(io)]

use std::old_io::IoResult;

pub mod sp80;

/// A SimProc instruction
trait Inst {
	fn encode<W: Writer>(&self, w: &mut W) -> IoResult<()>;
}

#[cfg(test)]
mod sp80_test;
