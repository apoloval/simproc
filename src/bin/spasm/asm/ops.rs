//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use simproc::inst::*;

use asm::assembly::AssemblyContext;
use asm::err::Error;
use asm::parser;

pub struct OperandAssembler<'a> {
    context: &'a AssemblyContext
}

macro_rules! parse_num {
    ($s:expr) => {
        match parser::parse_num(&$s) {
            Some(n) => Ok(n),
            None => Err(Error::BadNumber($s.clone())),
        }
    }
}

macro_rules! parse_symbol {
    ($syms:expr => $s:expr) => {
        match $syms.get($s) {
            Some(val) => Ok(*val),
            None => Err(Error::NoSuchSymbol($s.clone())),
        }
    }
}

macro_rules! to_i10 {
    ($n:expr) => (if ($n > 0x1ff) || ($n < -0x200) { None } else { Some($n as i16) })
}

fn to_i8(i: i64) -> Option<i8> {
    if i >= -128 && i < 127 { Some(i as i8) } else { None }
}

fn to_u8(i: i64) -> Option<u8> {
    if i >= 0 && i < 256 { Some(i as u8) } else { None }
}

fn to_u16(i: i64) -> Option<u16> {
    if i >= 0 && i < 65536 { Some(i as u16) } else { None }
}

impl<'a> OperandAssembler<'a> {

    pub fn with_context(context: &'a AssemblyContext) -> OperandAssembler<'a> {
        OperandAssembler { context: context }
    }

    pub fn map_immediate(&self, src: &String) -> Result<Immediate, Error> {
        let lit = parse_num!(src);
        let sym = parse_symbol!(self.context.symbols() => src);
        let k = try!(lit.or(sym));
        match to_i8(k) {
            Some(v) => Ok(Immediate(v as u8)),
            None => match to_u8(k) {
                Some(v) => Ok(Immediate(v)),
                None => Err(Error::OutOfRange(src.clone()))
            },
        }
    }

    pub fn map_addr(&self, src: &String) -> Result<Addr, Error> {
        let lit = parse_num!(src);
        let sym = parse_symbol!(self.context.symbols() => src);
        let k = try!(lit.or(sym));
        match to_u16(k) {
            Some(v) => Ok(Addr(v)),
            None => Err(Error::OutOfRange(src.clone())),
        }
    }

    pub fn map_rel_addr(&self, src: &String) -> Result<RelAddr, Error> {
        let lit = parse_num!(src);
        let sym = parse_symbol!(self.context.symbols() => src);
        let k = try!(lit.or(sym));
        match to_i10!(k - self.context.curr_addr() as i64) {
            Some(v) => Ok(RelAddr(v)),
            None => Err(Error::OutOfRange(src.clone())),
        }
    }

    pub fn map_reg(&self, src: &String) -> Result<Reg, Error> {
        match src.trim() {
            "r0" | "R0" => Ok(Reg::R0),
            "r1" | "R1" => Ok(Reg::R1),
            "r2" | "R2" => Ok(Reg::R2),
            "r3" | "R3" => Ok(Reg::R3),
            "r4" | "R4" => Ok(Reg::R4),
            "r5" | "R5" => Ok(Reg::R5),
            "r6" | "R6" => Ok(Reg::R6),
            "r7" | "R7" => Ok(Reg::R7),
            _ => Err(Error::BadReg(src.clone()))
        }
    }

    pub fn map_addr_reg(&self, src: &String) -> Result<AddrReg, Error> {
        match src.trim() {
            "a0" | "A0" => Ok(AddrReg::A0),
            "a1" | "A1" => Ok(AddrReg::A1),
            "a2" | "A2" => Ok(AddrReg::A2),
            "a3" | "A3" => Ok(AddrReg::A3),
            _ => Err(Error::BadAddrReg(src.clone()))
        }
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::assembly::AssemblyContext;
    use asm::err::Error;

    use super::*;

    macro_rules! with_symbols {
        () => (AssemblyContext::new() );
        ($([$sym:expr, $val:expr]),+) => ({
            let mut context = AssemblyContext::new();
            $(context.define_value($sym, $val);)*;
            context
        });
    }

    macro_rules! resolve {
        ($a:expr, $e:expr => $f:ident) => ($a.$f(&$e.to_string()).ok())
    }

    macro_rules! assert_map_eq {
        ($([$k:expr, $v:expr]),* => $expected:expr, $given:expr => $f:ident) => ({
            let context = with_symbols!($([$k, $v])*);
            let asmblr = OperandAssembler::with_context(&context);
            assert_eq!(Some($expected), resolve!(asmblr, $given => $f));
        });
    }

    macro_rules! assert_map_fail {
        ($([$k:expr, $v:expr]),* => $expected:expr, $given:expr => $f:ident) => ({
            let context = with_symbols!($([$k, $v])*);
            let asmblr = OperandAssembler::with_context(&context);
            assert_eq!(Some($expected), asmblr.$f(&$given.to_string()).err());
        });
    }

    #[test]
    fn should_map_literal_immediate() {
        assert_map_eq!(=>
            Immediate(123),
            "123" => map_immediate);
    }

    #[test]
    fn should_map_neg_literal_immediate() {
        assert_map_eq!(=>
            Immediate(-123i8 as u8),
            "-123" => map_immediate);
    }

    #[test]
    fn should_map_symbolic_immediate() {
        assert_map_eq!(["foo", 123] =>
            Immediate(123),
            "foo" => map_immediate);
    }

    #[test]
    fn should_map_neg_symbolic_immediate() {
        assert_map_eq!(["foo", -123] =>
            Immediate(-123i8 as u8),
            "foo" => map_immediate);
    }

    #[test]
    fn should_fail_to_map_out_of_bounds_literal_immediate() {
        assert_map_fail!(=>
            Error::OutOfRange("1234".to_string()),
            "1234" => map_immediate);
    }

    #[test]
    fn should_fail_to_map_out_of_bounds_symbolic_immediate() {
        assert_map_fail!(["foo", 1234] =>
            Error::OutOfRange("foo".to_string()),
            "foo" => map_immediate);
    }

    #[test]
    fn should_map_literal_address() {
        assert_map_eq!(=>
            Addr(0x1234),
            "0x1234" => map_addr);
    }

    #[test]
    fn should_fail_to_map_neg_literal_address() {
        assert_map_fail!(=>
            Error::OutOfRange("-0x1234".to_string()),
            "-0x1234" => map_addr);
    }

    #[test]
    fn should_map_symbolic_address() {
        assert_map_eq!(["foo", 0x1234] =>
            Addr(0x1234),
            "foo" => map_addr);
    }

    #[test]
    fn should_fail_to_map_neg_symbolic_address() {
        assert_map_fail!(["foo", -1234] =>
            Error::OutOfRange("foo".to_string()),
            "foo" => map_addr);
    }

    #[test]
    fn should_fail_to_map_out_of_bounds_literal_address() {
        assert_map_fail!(=>
            Error::OutOfRange("0x123456".to_string()),
            "0x123456" => map_addr);
    }

    #[test]
    fn should_fail_to_map_out_of_bounds_symbolic_address() {
        assert_map_fail!(["foo", 0x123456] =>
            Error::OutOfRange("foo".to_string()),
            "foo" => map_addr);
    }

    #[test]
    fn should_map_literal_rel_addr() {
        let mut context = AssemblyContext::new();
        context.set_addr(0x100);
        let asmblr = OperandAssembler::with_context(&context);
        let rel_addr = asmblr.map_rel_addr(&"0x120".to_string()).ok();
        assert_eq!(Some(RelAddr(0x20)), rel_addr);
    }

    #[test]
    fn should_map_symbolic_rel_addr() {
        let mut context = with_symbols!(["foo", 0x120]);
        context.set_addr(0x100);
        let asmblr = OperandAssembler::with_context(&context);
        let rel_addr = asmblr.map_rel_addr(&"foo".to_string()).ok();
        assert_eq!(Some(RelAddr(0x20)), rel_addr);
    }

    #[test]
    fn should_fail_to_map_out_of_bounds_literal_rel_addr() {
        let mut context = AssemblyContext::new();
        context.set_addr(0x100);
        let asmblr = OperandAssembler::with_context(&context);
        let err = asmblr.map_rel_addr(&"0x500".to_string()).err();
        assert_eq!(Some(Error::OutOfRange("0x500".to_string())), err);
    }

    #[test]
    fn should_fail_to_map_out_of_bounds_symbolic_rel_addr() {
        let mut context = with_symbols!(["foo", 0x500]);
        context.set_addr(0x100);
        let asmblr = OperandAssembler::with_context(&context);
        let err = asmblr.map_rel_addr(&"foo".to_string()).err();
        assert_eq!(Some(Error::OutOfRange("foo".to_string())), err);
    }
}
