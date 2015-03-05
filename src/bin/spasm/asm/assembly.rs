//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::collections::HashMap;

use simproc::inst::Inst;

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

    pub fn symbols(&self) -> &SymbolTable { &self.symbols }

    pub fn push(&mut self, code: Assembled<I>) { self.assembled.push(code) }

    pub fn assembled(&self) -> &[Assembled<I>] { &self.assembled[..] }
}
