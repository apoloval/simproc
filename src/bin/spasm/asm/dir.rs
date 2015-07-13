//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;

use asm::assembly::SymbolicAssembly;
use asm::err::Error;
use asm::parser::{Parameterized, parse_num};

/// An assembly language directive
/// Directives are used in assembly language to declare relevant information
/// about the assembly process, like:
///
/// * `DefData(data_size: u8, bytes: Vec<String>)`: defines a data vector comprised by
/// elements of _data_size_ bytes.
/// * `Origin(address: usize)`: indicates the memory address where the following
/// instructions will be assembled.
#[derive(Debug, PartialEq)]
pub enum Directive {
    DefData(u8, Vec<String>),
    Origin(usize)
}

impl Directive {

    /// Retrieve a directive from the parsed token
    pub fn from_params(par: &Parameterized) -> Result<Directive, Error> {
        match par.elem().to_ascii_lowercase().trim() {
            "org" => Directive::org_from_params(par),
            "db" => Directive::data_from_params(1, par),
            "dw" => Directive::data_from_params(2, par),
            elem => Err(Error::UnknownDirective(elem.to_string())),
        }
    }

    fn org_from_params(par: &Parameterized) -> Result<Directive, Error> {
        let params = par.params();
        if params.len() != 1 {
            return Err(Error::BadOperandsCount(1))
        }
        let param0 = &params[0];
        match parse_num(param0) {
            Some(addr) => Ok(Directive::Origin(addr as usize)),
            None => Err(Error::BadNumber(param0.clone())),
        }
    }

    fn data_from_params(dsize: u8, par: &Parameterized) -> Result<Directive, Error> {
        let params = par.params();
        Ok(Directive::DefData(dsize, params.clone()))
    }

    pub fn apply(&self, asm: &mut SymbolicAssembly) {
        match self {
            &Directive::DefData(_, _) => {},
            &Directive::Origin(ref orig) => asm.ctx_mut().set_addr(*orig),
        }
    }
}

#[cfg(test)]
mod test {

    use asm::assembly::SymbolicAssembly;
    use asm::err::Error;
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
        should_fail_make_from_invalid_param_count(&param!("org"), 1);
        should_fail_make_from_invalid_param_number(&param!("org", "foobar"), "foobar");
    }

    #[test]
    fn should_make_db_from_params() {
        assert_eq!(
            Ok(Directive::DefData(1, vec!("1".to_string(), "2".to_string(), "foo".to_string()))),
            Directive::from_params(&param!("db", "1", "2", "foo")));
    }

    #[test]
    fn should_make_dw_from_params() {
        assert_eq!(
            Ok(Directive::DefData(2, vec!("1".to_string(), "2".to_string(), "foo".to_string()))),
            Directive::from_params(&param!("dw", "1", "2", "foo")));
    }

    #[test]
    fn should_fail_make_from_unknown_directive() {
        assert_eq!(
            Err(Error::UnknownDirective("foobar".to_string())),
            Directive::from_params(&param!("foobar")));
    }

    #[test]
    fn should_apply_origin() {
        let mut asm = SymbolicAssembly::new();
        let dir = Directive::Origin(0x8000);
        dir.apply(&mut asm);
        assert_eq!(0x8000, asm.ctx().curr_addr());
    }

    fn should_fail_make_from_invalid_param_count(params: &Parameterized, expected: usize) {
        assert_eq!(
            Err(Error::BadOperandsCount(expected)),
            Directive::from_params(&params));
    }

    fn should_fail_make_from_invalid_param_number(params: &Parameterized, number: &str) {
        assert_eq!(
            Err(Error::BadNumber(number.to_string())),
            Directive::from_params(&params));
    }
}
