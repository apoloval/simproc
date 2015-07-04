//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt::Display;
use std::fs::File;
use std::io::Read;

use asm::assembly::*;
use asm::dir::Directive;
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

    fn assemble_inst(from: &Self::AssemblyInst, context: &mut AssemblyContext) ->
            Result<Self::RuntimeInst, Self::AssemblyErr>;

    fn assemble_file(&self, input_file: &str) -> Result<Assembly<Self::RuntimeInst>, AssemblyError> {
        let input = try!(File::open(input_file));
        self.assemble(input)
    }

    fn assemble<R: Read>(&self, input: R) -> Result<Assembly<Self::RuntimeInst>, AssemblyError> {
        let lines = try!(parser::read_lines(input));
        let mut context = AssemblyContext::new();
        let mut errors: Vec<ProgramError> = Vec::new();
        let mut assembled: Vec<Assembled<Self::AssemblyInst>> = Vec::new();

        let tokens = parser::tokenize(&lines);

        // First loop, gather assembled elements and errors
        for i in 0..tokens.len() {
            let tk = &tokens[i];
            let line = &lines[i];
            match tk {
                &Token::Label(ref label) => {
                    context.define(&label[..]);
                },
                &Token::Mnemonic(ref mnemo, ref ops) => {
                    let from_mnemo: Result<Self::AssemblyInst, _> =
                        FromMnemo::from_mnemo(mnemo, ops);
                    match from_mnemo {
                        Ok(inst) => {
                            let inst_len = inst.len();
                            assembled.push(Assembled::Inst(line.clone(), context.curr_addr(), inst));
                            context.inc_addr(inst_len);
                        },
                        Err(err) => {
                            errors.push(ProgramError::new(i, &line[..], &format!("{}", err)[..]));
                            assembled.push(Assembled::Ignored(line.clone()));
                        },
                    }
                },
                &Token::Directive(ref dirname, ref ops) => {
                    match Directive::from_token(dirname, ops) {
                        Ok(dir) => {

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
        let mut assembly = Assembly::with_symbols(context.symbols());
        context.reset_addr();
        for i in 0..assembled.len() {
            let a = &assembled[i];
            match a {
                &Assembled::Inst(ref l, p, ref inst) => {
                    match Self::assemble_inst(inst, &mut context) {
                        Ok(asm_inst) =>
                            assembly.push(Assembled::Inst(l.clone(), p, asm_inst)),
                        Err(e) => errors.push(
                            ProgramError::new(i, l.trim(), &format!("{}", e)[..])),
                    };
                    context.inc_addr(inst.len());
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
