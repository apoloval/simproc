//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ops::{Range};
use std::u8;

use cpu::*;
use mem::*;

/// An immediate value that comes after an opcode.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Immediate(pub u8);

impl Immediate {
    pub fn range() -> Range<i64> { u8::MIN as i64 .. u8::MAX as i64 }

    pub fn from_i64(n: i64) -> Option<Immediate> {
        if n < u8::MIN as i64 || n > u8::MAX as i64 { None }
        else { Some(Immediate(n as u8)) }
    }
}

pub trait Operands {
    type Immediate;
    type Addr;
    type RelAddr;
    type Reg;
    type AddrReg;
}

#[derive(Debug, PartialEq)]
pub struct RuntimeOperands;

impl Operands for RuntimeOperands {
    type Immediate = Immediate;
    type Addr = Addr;
    type RelAddr = RelAddr;
    type Reg = Reg;
    type AddrReg = AddrReg;
}
