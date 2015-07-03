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

/// The symbol table associates symbols with memory addresses.
pub type SymbolTable = HashMap<String, i64>;

/// The assembly context stores information regarding assembly process.
/// As the assembler proceeds, it keeps relevant information as the symbol
/// table or the instruction placement in the assembly context. As the code
/// is parsed, the directives & instructions uses the context to determine
/// how the code should be assembled.
pub struct AssemblyContext {
    symbols: SymbolTable,
    placement: usize,
}

impl AssemblyContext {

    /// Create a new empty assembly context.
    /// With no symbol defined and ready to put code at address 0x0000.
    pub fn new() -> AssemblyContext {
        AssemblyContext {
            symbols: SymbolTable::new(),
            placement: 0,
        }
    }

    /// Returns the symbols table.
    pub fn symbols(&self) -> &SymbolTable { &self.symbols }

    /// Return the current memory address where next element will be assembled.
    pub fn curr_addr(&self) -> usize { self.placement }

    /// Increments the memory address where next element will be assembled.
    pub fn inc_addr(&mut self, nbytes: usize) { self.placement += nbytes }

    /// Resets the memory address where next element will be assembled (sets it to 0x0000).
    pub fn reset_addr(&mut self) { self.placement = 0 }

    /// Define a new symbol using the current placement.
    /// A new symbol with the given label will be defined in the symbol table. Its value
    /// will be the current placement stored in the context.
    pub fn define(&mut self, label: &str) {
        self.symbols.insert(label.to_string(), self.placement as i64);
    }
}

/// An assembled text line.
/// This type enumerates the result of assembling a single line of text. One of:
/// * `Inst(line: String, placement: usize, inst: I)`: a successfully assembled instruction, where
///   * `line` is the source text line
///   * `placement` is the memory address where the instruction is allocated
///   * `inst` is the assembled instruction
/// * `Ignored(line: String)`: an ignored source text line (i.e. a comment, assembler directive...)
pub enum Assembled<I: Inst> {
    Inst(String, usize, I), // (line: String, placement: usize, inst: I)
    Ignored(String), // (line: String)
}

/// A unit of assembled code.
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
