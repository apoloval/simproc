//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::io;

/// A SimProc instruction
pub trait Inst {
    fn len(&self) -> usize;
}

/// A SimProc instruction that can be encoded into bytes
pub trait Encode where Self : Inst {
    fn encode<W: io::Write>(&self, w: &mut W) -> io::Result<usize>;
}

/// An instruction that can be assembled
pub trait Assemble<OperandMapper> where Self : Inst {
    type ToInst;
    type Err;
    fn assemble(&self, mapper: &OperandMapper) -> Result<Self::ToInst, Self::Err>;
}
