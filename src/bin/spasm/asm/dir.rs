//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::{Display, Error, Formatter};

use asm::parser::{Parameterized, parse_num};

/// An assembly language directive
/// Directives are used in assembly language to declare relevant information
/// about the assembly process, like:
///
/// * `Origin(address: usize)`: indicates the memory address where the following
/// instructions will be assembled.
pub enum Directive {
    Origin(usize)
}

/// A directive import error.
/// One of:
///
/// * `BadArguments(directive: String, args: Vec<String>)`: invalid arguments for given directive
/// * `Unknown(directive: String)`: the given directive is unknown
pub enum DirImportErr {
    BadParams(Parameterized),
    Unknown(String),
}

impl Display for DirImportErr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match self {
            &DirImportErr::BadParams(ref par) =>
                write!(fmt, "Invalid arguments in `{}`", par),
            &DirImportErr::Unknown(ref dir) =>
                write!(fmt, "Unknown directive `{}`", dir),
        }
    }
}

impl Directive {

    /// Retrieve a directive from the parsed token
    pub fn from_params(par: &Parameterized) -> Result<Directive, DirImportErr> {
        match par.elem() {
            "org" | "ORG" => Directive::org_from_params(par),
            elem => Err(DirImportErr::Unknown(elem.to_string())),
        }
    }

    fn org_from_params(par: &Parameterized) -> Result<Directive, DirImportErr> {
        let params = par.params();
        if params.len() == 1 {
            match parse_num(&params[0][..]) {
                Some(addr) => return Ok(Directive::Origin(addr as usize)),
                None => {},
            }
        }
        Err(DirImportErr::BadParams(par.clone()))
    }
}
