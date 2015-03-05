//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Display;
use std::fs::File;

use asm::assembly::*;
use asm::err::{AssemblyError, ProgramError};
use asm::inst::FromMnemo;
use asm::parser;
use asm::parser::Token;

use simproc::inst::{Inst, Encode};

pub trait Assembler {
    type AssemblyInst : Inst + FromMnemo;
    type RuntimeInst : Inst + Encode;
    type AssemblyErr : Display;

    fn new() -> Self;

    fn assemble_inst(from: &Self::AssemblyInst, 
                     symbols: &SymbolTable, 
                     placement: usize) -> Result<Self::RuntimeInst, Self::AssemblyErr>;

    fn assemble(&self, input_file: &str) -> Result<Assembly<Self::RuntimeInst>, AssemblyError> {
        let input = try!(File::open(input_file));
        let lines = try!(parser::read_lines(input));
        let mut symbols: SymbolTable = SymbolTable::new();
        let mut placement = 0 as usize;
        let mut errors: Vec<ProgramError> = Vec::new();
        let mut assembled: Vec<Assembled<Self::AssemblyInst>> = Vec::new();

        let tokens = parser::tokenize(&lines);

        // First loop, gather assembled elements and errors
        for i in 0..tokens.len() {
            let tk = &tokens[i];
            let line = &lines[i];
            match tk {
                &Token::Label(ref label) => { 
                    symbols.insert(label.clone(), placement as i64);
                    assembled.push(Assembled::Ignored(line.clone()));
                },
                &Token::Mnemonic(ref mnemo, ref args) => {
                    let from_mnemo: Result<Self::AssemblyInst, _> = 
                        FromMnemo::from_mnemo(mnemo, args);
                    match from_mnemo {
                        Ok(inst) => { 
                            let next_placement = placement + inst.len(); 
                            assembled.push(Assembled::Inst(line.clone(), placement, inst));
                            placement = next_placement;
                        },
                        Err(err) => { 
                            errors.push(ProgramError::new(i, &line[..], &format!("{}", err)[..]));
                            assembled.push(Assembled::Ignored(line.clone()));
                        },
                    }
                },
                &Token::Blank => {
                    assembled.push(Assembled::Ignored(line.clone()));
                },
                &Token::LexicalError => {
                    errors.push(ProgramError::new_lexical_error(i, &line[..]));
                    assembled.push(Assembled::Ignored(line.clone()));
                },
            };
        }

        // Second loop, encode assembled instructions
        let mut assembly = Assembly::with_symbols(&symbols);
        placement = 0;
        for i in 0..assembled.len() {
            let a = &assembled[i];
            match a {
                &Assembled::Inst(ref l, p, ref inst) => {
                    match Self::assemble_inst(inst, &symbols, placement) {
                        Ok(asm_inst) => 
                            assembly.push(Assembled::Inst(l.clone(), p, asm_inst)),
                        Err(e) => errors.push(
                            ProgramError::new(i, l.trim(), &format!("{}", e)[..])),
                    };
                    placement += inst.len();
                },
                &Assembled::Ignored(ref ign) => {
                    assembly.push(Assembled::Ignored(ign.clone()));
                },
            }
        }


        if errors.is_empty() { Ok(assembly) }
        else { Err(AssemblyError::BadProgram(errors)) }
    }
}
