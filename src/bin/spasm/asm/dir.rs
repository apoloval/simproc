//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::{Display, Error, Formatter};

use asm::assembly::SymbolicAssembly;
use asm::parser::{Parameterized, parse_num};

/// An assembly language directive
/// Directives are used in assembly language to declare relevant information
/// about the assembly process, like:
///
/// * `Origin(address: usize)`: indicates the memory address where the following
/// instructions will be assembled.
#[derive(Debug, PartialEq)]
pub enum Directive {
    Origin(usize)
}

/// A directive import error.
/// One of:
///
/// * `BadArguments(directive: String, args: Vec<String>)`: invalid arguments for given directive
/// * `Unknown(directive: String)`: the given directive is unknown
#[derive(Debug, PartialEq)]
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
            match parse_num(&params[0]) {
                Some(addr) => return Ok(Directive::Origin(addr as usize)),
                None => {},
            }
        }
        Err(DirImportErr::BadParams(par.clone()))
    }

    pub fn apply(&self, asm: &mut SymbolicAssembly) {
        match self {
            &Directive::Origin(ref orig) => asm.ctx_mut().set_addr(*orig),
        }
    }
}

#[cfg(test)]
mod test {

    use asm::assembly::SymbolicAssembly;
    use asm::parser::Parameterized;

    use super::*;

    #[test]
    fn should_make_org_from_params() {
        assert_eq!(
            Ok(Directive::Origin(0x8000)),
            Directive::from_params(&param!("org", "0x8000")));
    }

    #[test]
    fn should_fail_make_org_from_invalid_params() {
        should_fail_make_from_invalid_params(&param!("org"));
        should_fail_make_from_invalid_params(&param!("org", "foobar"));
    }

    #[test]
    fn should_fail_make_from_unknown_directive() {
        assert_eq!(
            Err(DirImportErr::Unknown("foobar".to_string())),
            Directive::from_params(&param!("foobar")));
    }

    #[test]
    fn should_apply_origin() {
        let mut asm = SymbolicAssembly::new();
        let dir = Directive::Origin(0x8000);
        dir.apply(&mut asm);
        assert_eq!(0x8000, asm.ctx().curr_addr());
    }

    fn should_fail_make_from_invalid_params(params: &Parameterized) {
        assert_eq!(
            Err(DirImportErr::BadParams(params.clone())),
            Directive::from_params(&params));
    }
}
