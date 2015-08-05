//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use simproc::inst::*;

use asm::lexer::*;
use asm::pre::*;

#[derive(Debug, PartialEq)]
pub enum FullAssembled {
    Inst { loc: TextLoc, base_addr: Addr, inst: RuntimeInst },
}

#[derive(Debug, PartialEq)]
pub enum FullAssembleError {
    // Pre(PreAssembleError),
    NotImplemented,
}

pub type FullAssemblerInput = PreAssemblerOutput;
pub type FullAssemblerOutput = Result<FullAssembled, FullAssembleError>;

pub struct FullAssembler<'a, I: Iterator<Item=FullAssemblerInput>> {
    input: I,
    symbols: &'a SymbolTable,
}

impl<'a, I: Iterator<Item=FullAssemblerInput>> FullAssembler<'a, I> {

    pub fn from<P>(pre: P, symbols: &'a SymbolTable) -> Self
        where P: IntoIterator<Item=FullAssemblerInput, IntoIter=I>
    {
        FullAssembler { input: pre.into_iter(), symbols: symbols }
    }
}

impl<'a, I: Iterator<Item=FullAssemblerInput>> Iterator for FullAssembler<'a, I> {

    type Item = FullAssemblerOutput;

    fn next(&mut self) -> Option<FullAssemblerOutput> {
        match self.input.next() {
            Some(Ok(PreAssembled::Inst { loc, base_addr, inst, .. })) => {
                Some(full_assemble_inst(inst, self.symbols).map(|i| FullAssembled::Inst {
                    loc: loc, base_addr: base_addr, inst: i,
                }))
            },
            _ => None,
        }
    }
}

pub fn full_assemble_inst(
    inst: PreAssembledInst,
    _symbols: &SymbolTable) -> Result<RuntimeInst, FullAssembleError>
{
    match inst {
        Inst::Nop => Ok(Inst::Nop),
        _ => Err(FullAssembleError::NotImplemented),
    }
}

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::lexer::TextLoc;
    use asm::pre::*;

    use super::*;

    #[test]
    fn should_assemble_empty_program() {
        let input = Vec::new();
        let symbols = SymbolTable::new();
        let mut full = FullAssembler::from(input, &symbols);
        assert_eq!(full.next(), None);
    }

    #[test]
    fn should_assemble_inst() {
        let input = vec![
            Ok(PreAssembled::Inst {
                loc: loc!(1, 1, "nop"),
                base_addr: Addr(0x100),
                inst: Inst::Nop,
            }),
        ];
        let symbols = SymbolTable::new();
        let mut full = FullAssembler::from(input, &symbols);
        assert_eq!(full.next(), Some(Ok(FullAssembled::Inst {
            loc: loc!(1, 1, "nop"),
            base_addr: Addr(0x100),
            inst: Inst::Nop,
        })));
    }
}
