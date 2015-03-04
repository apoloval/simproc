//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use simproc::inst::Inst;

pub trait FromMnemo where Self : Inst {
    type Err;
    fn from_mnemo(mnemo: &str, args: &[String]) -> Result<Self, Self::Err>;
}
