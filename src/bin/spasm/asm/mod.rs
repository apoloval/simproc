//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#[macro_use]
mod parser;

mod data;
mod dir;
mod inst;
mod ops;

pub mod assembly;
pub use self::assembly::*;

pub mod assembler;
pub use self::assembler::Assembler;

pub mod err;
pub use self::err::{AssemblyError, ProgramError};



#[macro_use]
mod lexer;

#[macro_use]
mod new_parser;

mod pre;
