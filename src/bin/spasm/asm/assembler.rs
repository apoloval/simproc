//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fs::File;
use std::io::Read;

use asm::assembly::*;
use asm::dir::Directive;
use asm::err::{AssemblyError, ProgramError};
use asm::inst::*;
use asm::parser;
use asm::parser::Parsed;

pub struct Assembler;

impl Assembler {

    pub fn new() -> Assembler { Assembler }

    pub fn assemble_file(&self, input_file: &str) -> Result<RuntimeAssembly, AssemblyError> {
        let input = try!(File::open(input_file));
        self.assemble(input)
    }

    pub fn assemble<R: Read>(&self, input: R) -> Result<RuntimeAssembly, AssemblyError> {
        let lines = try!(parser::read_lines(input));
        let mut errors: Vec<ProgramError> = Vec::new();
        let mut pre: SymbolicAssembly = Assembly::new();

        let parsed = parser::parse(&lines);

        // First loop, pre-processing: gather assembled elements and errors
        for (i, p) in parsed.iter().enumerate() {
            let line = &lines[i];
            match p {
                &Parsed::Label(ref label) => {
                    pre.define(&label);
                },
                &Parsed::Mnemonic(ref par) => {
                    match from_mnemo(par) {
                        Ok(inst) => {
                            let inst_len = inst.len();
                            let curr_addr = pre.ctx().curr_addr();
                            pre.push(Assembled::Inst(line.clone(), curr_addr, inst));
                            pre.inc_addr(inst_len);
                        },
                        Err(err) => {
                            errors.push(ProgramError::new(i, &line, &format!("{}", err)));
                            pre.push(Assembled::Ignored(line.clone()));
                        },
                    }
                },
                &Parsed::Directive(ref par) => {
                    match Directive::from_params(par) {
                        Ok(dir) => dir.apply(&mut pre),
                        Err(err) => {
                            errors.push(ProgramError::new(i, &line, &format!("{}", err)));
                            pre.push(Assembled::Ignored(line.clone()));
                        },
                    }
                },
                &Parsed::Blank => {
                    pre.push(Assembled::Ignored(line.clone()));
                },
                &Parsed::LexicalError => {
                    errors.push(ProgramError::new_lexical_error(i, &line));
                    pre.push(Assembled::Ignored(line.clone()));
                },
            };
        }

        // Second loop, post-processing: encode assembled instructions
        let mut post = Assembly::with_symbols(pre.ctx().symbols());
        for (i, a) in pre.assembled().iter().enumerate() {
            match a {
                &Assembled::Inst(ref l, p, ref inst) => {
                    match assemble_inst(inst, post.ctx_mut()) {
                        Ok(asm_inst) =>
                            post.push(Assembled::Inst(l.clone(), p, asm_inst)),
                        Err(e) => errors.push(
                            ProgramError::new(i, l.trim(), &format!("{}", e))),
                    };
                    post.inc_addr(inst.len());
                },
                &Assembled::Ignored(ref ign) => {
                    post.push(Assembled::Ignored(ign.clone()));
                },
            }
        }

        if errors.is_empty() { Ok(post) }
        else { Err(AssemblyError::BadProgram(errors)) }
    }
}
