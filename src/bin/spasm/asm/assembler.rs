//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::iter::FromIterator;
use std::slice;

use asm::full::*;
use asm::lexer::*;
use asm::parser::*;
use asm::pre::*;

pub type AssemblerInput = ScannerInput;
pub type AssemblerOutput = FullAssemblerOutput;

pub fn assemble<C, T>(chars: C) -> (T, SymbolTable) where
    C: IntoIterator<Item=AssemblerInput>,
    T: FromIterator<AssemblerOutput>,
{
    let mut symbols = SymbolTable::new();
    let result = {
        let scanner = Scanner::scan(chars);
        let parser = Parser::parse(scanner);
        let pre: Vec<_> = PreAssembler::from_parser(parser).pre_assemble(&mut symbols);
        let full = FullAssembler::from(pre, &symbols);
        T::from_iter(full)
    };
    (result, symbols)
}

pub struct Assembly {
    results: Vec<FullAssemblerOutput>,
    symbols: SymbolTable,
}

impl Assembly {

    pub fn assemble<C>(chars: C) -> Self where C: IntoIterator<Item=AssemblerInput> {
        let (out, symbols): (Vec<_>, _) = assemble(chars);
        Assembly {
            results: out,
            symbols: symbols,
        }
    }

    pub fn has_errors(&self) -> bool { self.errors().count() != 0 }

    pub fn symbols(&self) -> &SymbolTable { &self.symbols }

    pub fn code<'a>(&'a self) -> Code<'a> {
        Code { results: self.results.iter() }
    }

    pub fn errors<'a>(&'a self) -> Errors<'a> {
        Errors { results: self.results.iter() }
    }
}

pub struct Code<'a> {
    results: slice::Iter<'a, FullAssemblerOutput>,
}

impl<'a> Iterator for Code<'a> {
    type Item = &'a FullAssembled;

    fn next(&mut self) -> Option<&'a FullAssembled> {
        loop {
            match self.results.next() {
                Some(&Ok(ref c)) => return Some(c),
                Some(&Err(_)) => {},
                None => return None,
            }
        }
    }
}

pub struct Errors<'a> {
    results: slice::Iter<'a, FullAssemblerOutput>,
}

impl<'a> Iterator for Errors<'a> {
    type Item = &'a FullAssembleError;

    fn next(&mut self) -> Option<&'a FullAssembleError> {
        loop {
            match self.results.next() {
                Some(&Ok(_)) => {},
                Some(&Err(ref e)) => return Some(e),
                None => return None,
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn should_() {
        let prog = "\
            begin: nop \n\
                   noop ; this is an error \n\
                   nop \n\
        ";
        let asm = Assembly::assemble(prog.chars());
        assert!(asm.has_errors());
        assert_eq!(asm.code().count(), 2);
        assert_eq!(asm.errors().count(), 1);
        assert_eq!(asm.symbols().keys().count(), 1);
    }
}
