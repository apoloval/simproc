//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use simproc::inst::*;

use asm::expr::*;

pub mod len;
pub mod pre;
pub mod full;

#[derive(Debug, PartialEq)]
pub struct PreAssembledOperands;

impl Operands for PreAssembledOperands {
    type Immediate = Expr;
    type Addr = Expr;
    type RelAddr = Expr;
    type Reg = Expr;
    type AddrReg = Expr;
    type IoPort = Expr;
}

pub type PreAssembledInst = Inst<PreAssembledOperands>;
