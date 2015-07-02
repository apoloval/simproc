//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Display;

use simproc::inst::Inst;

pub trait FromMnemo where Self : Inst {
    type Err : Display;
    fn from_mnemo(mnemo: &str, ops: &[String]) -> Result<Self, Self::Err>;
}
