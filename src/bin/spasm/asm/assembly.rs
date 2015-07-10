//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashMap;
use std::io;

use simproc::inst::*;

/// The symbol table associates symbols with memory addresses.
pub type SymbolTable = HashMap<String, i64>;

/// The assembly context stores information regarding assembly process.
/// As the assembler proceeds, it keeps relevant information as the symbol
/// table or the next memory address to be assembled in the assembly context.
/// As the code is parsed, the directives & instructions uses the context to determine
/// how the code should be assembled.
pub struct AssemblyContext {
    symbols: SymbolTable,
    curr_addr: usize,
}

impl AssemblyContext {

    /// Create a new empty assembly context.
    /// With no symbol defined and ready to put code at address 0x0000.
    pub fn new() -> AssemblyContext {
        AssemblyContext { symbols: SymbolTable::new(), curr_addr: 0, }
    }

    /// Create a new assembly context with given symbol table.
    pub fn with_symbols(symbols: &SymbolTable) -> AssemblyContext {
        AssemblyContext { symbols: symbols.clone(), curr_addr: 0, }
    }

    /// Returns the symbols table.
    pub fn symbols(&self) -> &SymbolTable { &self.symbols }

    /// Return the current memory address where next element will be assembled.
    pub fn curr_addr(&self) -> usize { self.curr_addr }

    /// Increments the memory address where next element will be assembled.
    pub fn inc_addr(&mut self, nbytes: usize) { self.curr_addr += nbytes }

    /// Sets the memory address where next element will be assembled.
    pub fn set_addr(&mut self, addr: usize) { self.curr_addr = addr }

    /// Define a new symbol using the next address to be assembled.
    /// A new symbol with the given label will be defined in the symbol table. Its value
    /// will be the current address where next element will be assembled.
    pub fn define(&mut self, label: &str) {
        let value = self.curr_addr() as i64;
        self.define_value(label, value)
    }

    /// Define a new symbol using the given label and value.
    pub fn define_value(&mut self, label: &str, value: i64) {
        self.symbols.insert(label.to_string(), value);
    }
}

/// An assembled text line.
/// This type enumerates the result of assembling a single line of text. One of:
/// * `Inst(line: String, addr: usize, inst: I)`: a successfully assembled instruction, where
///   * `line` is the source text line
///   * `addr` is the memory address where the instruction is allocated
///   * `inst` is the assembled instruction
/// * `Ignored(line: String)`: an ignored source text line (i.e. a comment, assembler directive...)
pub enum Assembled<O: Operands> {
    Inst(String, usize, Inst<O>),
    Ignored(String),
}

/// A unit of assembled code.
pub struct Assembly<O: Operands> {
    context: AssemblyContext,
    assembled: Vec<Assembled<O>>,
}

pub type SymbolicAssembly = Assembly<SymbolicOperands>;
pub type RuntimeAssembly = Assembly<RuntimeOperands>;

impl<O: Operands> Assembly<O> {

    pub fn with_symbols(symbols: &SymbolTable) -> Assembly<O> {
        Assembly {
            context: AssemblyContext::with_symbols(symbols),
            assembled: Vec::new(),
        }
    }

    /// Create a new empty assembly.
    /// With no symbol defined, no assembled elements and ready to put code at address 0x0000.
    pub fn new() -> Assembly<O> {
        Assembly {
            context: AssemblyContext::new(),
            assembled: Vec::new(),
        }
    }

    /// Returns the assembly context.
    pub fn ctx(&self) -> &AssemblyContext { &self.context }

    /// Returns the assembly context.
    pub fn ctx_mut(&mut self) -> &mut AssemblyContext { &mut self.context }

    pub fn assembled(&self) -> &Vec<Assembled<O>> { &self.assembled }

    /// Define a new symbol in the context.
    pub fn define(&mut self, label: &str) { self.context.define(label) }

    /// Increments the memory address where next element will be assembled.
    pub fn inc_addr(&mut self, nbytes: usize) { self.context.inc_addr(nbytes) }

    /// Put a new assembled element.
    pub fn push(&mut self, code: Assembled<O>) { self.assembled.push(code) }
}

impl Assembly<RuntimeOperands> {

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
        if self.context.symbols.is_empty() { try!(writeln!(output, "  Empty")); }
        else {
            for (sym, val) in self.context.symbols.iter() {
                try!(writeln!(output, "  {} : 0x{:04x}", sym, val));
            }
        }
        Ok(())
    }
}
