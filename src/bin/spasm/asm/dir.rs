//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::{Display, Error, Formatter};

/// An assembly language directive
/// Directives are used in assembly language to declare relevant information
/// about the assembly process, like:
///
/// * `Org(address: usize)`: indicates the memory address where the following
/// instructions will be assembled.
pub enum Directive {
	Org(usize)
}

/// A directive import error.
/// One of:
///
/// * `Unknown(directive: String)`: the given directive is unknown
pub enum DirImportErr {
	Unknown(String)
}

impl Display for DirImportErr {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match self {
            &DirImportErr::Unknown(ref dir) =>
                write!(fmt, "Unknown directive `{}`", dir),
        }
    }
}

impl Directive {

	/// Retrieve a directive from the parsed token
	pub fn from_token(dir: &str, args: &Vec<String>) -> Result<Directive, DirImportErr> {
		Err(DirImportErr::Unknown(dir.to_string()))
	}
}
