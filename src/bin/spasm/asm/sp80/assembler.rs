//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fs::File;
use std::io;
use std::slice::SliceExt;

use simproc::inst::{Inst, Encode};
use simproc::sp80;
use simproc::sp80::{AssemblyArgs, RuntimeArgs};

use asm::{Assembly, AssemblyError, Assembled, ProgramError, SymbolTable};
use asm::inst::FromMnemo;
use asm::parser;
use asm::parser::Token;
use asm::sp80::args;

pub struct Assembler;

pub type RuntimeAssembly = Assembly<sp80::RuntimeInst>;

impl Assembler {

    pub fn new() -> Assembler { Assembler }

    pub fn assemble_as_text<W : io::Write>(&self, 
                                           input_file: &str, 
                                           output: &mut W) -> Result<(), AssemblyError> {
        let asm = try!(self.assemble(input_file));
        for line in asm.assembled().iter() {
            match line {
                &Assembled::Inst(ref line, place, ref inst) => {
                    let mut buff: Vec<u8> = Vec::new();
                    let nbytes = inst.encode(&mut buff).unwrap();
                    try!(write!(output, "0x{:04x} : ", place as u16));
                    for b in buff.iter() {            
                        try!(write!(output, "{:02x} ", b));
                    }
                    for _ in 0..(10 - 3*nbytes) { try!(write!(output, " ")); }
                    try!(writeln!(output, "{}", line));
                },
                &Assembled::Ignored(ref line) => 
                    try!(writeln!(output, "                   {}", line)),
            }
        }

        let symbols = asm.symbols();
        try!(writeln!(output, "\nSymbol table:"));
        if symbols.is_empty() { try!(writeln!(output, "  Empty")); }
        else {
            for (sym, val) in symbols.iter() {
                try!(writeln!(output, "  {} : 0x{:04x}", sym, val));
            }
        }
        Ok(())
    }

    pub fn assemble(&self, input_file: &str) -> Result<RuntimeAssembly, AssemblyError> {
        let input = try!(File::open(input_file));
        let lines = try!(parser::read_lines(input));
        let mut symbols: SymbolTable = SymbolTable::new();
        let mut placement = 0 as usize;
        let mut errors: Vec<ProgramError> = Vec::new();
        let mut assembled: Vec<Assembled<sp80::Inst<AssemblyArgs>>> = Vec::new();

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
                    let from_mnemo: Result<sp80::Inst<AssemblyArgs>, String> = 
                        FromMnemo::from_mnemo(mnemo, args);
                    match from_mnemo {
                        Ok(inst) => { 
                            let next_placement = placement + inst.len(); 
                            assembled.push(Assembled::Inst(line.clone(), placement, inst));
                            placement = next_placement;
                        },
                        Err(err) => { 
                            errors.push(ProgramError::new(i, &line[..], &err[..]));
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
        let mut arg_asmblr = args::ArgAssembler::with_symbols(&symbols);
        let mut assembly = Assembly::with_symbols(&symbols);
        placement = 0;
        for i in 0..assembled.len() {
            let a = &assembled[i];
            match a {
                &Assembled::Inst(ref l, p, ref inst) => {
                    arg_asmblr.set_location(placement);
                    match inst.assemble(&arg_asmblr) {
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
