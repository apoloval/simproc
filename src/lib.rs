//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![feature(io)]
#![feature(rustc_private)]

use std::io;

pub mod sp80;

/// A SimProc instruction
pub trait Inst {
	fn len(&self) -> usize;
	fn encode<W: io::Write>(&self, w: &mut W) -> io::Result<usize>;
}

#[cfg(test)]
mod sp80_test;
