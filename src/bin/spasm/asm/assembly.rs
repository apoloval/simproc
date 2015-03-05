//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashMap;
use std::io;

use simproc::inst::{Inst, Encode};

pub type SymbolTable = HashMap<String, i64>;

pub enum Assembled<I: Inst> {
    Inst(String, usize, I), // (line: String, placement: usize, inst: I)
    Ignored(String), // (line: String)
}

pub struct Assembly<I: Inst> {
    symbols: SymbolTable,
    assembled: Vec<Assembled<I>>,
}

impl<I: Inst> Assembly<I> {

    pub fn with_symbols(symbols: &SymbolTable) -> Assembly<I> {
        Assembly {
            symbols: symbols.clone(),
            assembled: Vec::new(),
        }
    }

    pub fn push(&mut self, code: Assembled<I>) { self.assembled.push(code) }
}

impl<I: Inst + Encode> Assembly<I> {

    pub fn write_as_bin<W : io::Write>(&self, output: &mut W) -> io::Result<()> {
        for item in self.assembled.iter() {
            match item {
                &Assembled::Inst(_, _, ref inst) => {                     
                    try!(inst.encode(output)); 
                },
                &Assembled::Ignored(_) => (),
            }
        }
        Ok(())
    }

    pub fn write_as_text<W : io::Write>(&self, output: &mut W) -> io::Result<()> {
        for item in self.assembled.iter() {
            match item {
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

        try!(writeln!(output, "\nSymbol table:"));
        if self.symbols.is_empty() { try!(writeln!(output, "  Empty")); }
        else {
            for (sym, val) in self.symbols.iter() {
                try!(writeln!(output, "  {} : 0x{:04x}", sym, val));
            }
        }
        Ok(())        
    }    
}
