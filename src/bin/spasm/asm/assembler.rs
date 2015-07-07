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
use asm::parser;
use asm::parser::Parameterized;
use asm::parser::Parsed;

use simproc::inst::*;

pub trait Assembler {
    type AssemblyErr : Display;

    fn new() -> Self;

    fn assemble_inst(from: &SymbolicInst, context: &mut AssemblyContext) ->
            Result<RuntimeInst, Self::AssemblyErr>;

    fn assemble_file(&self, input_file: &str) -> Result<RuntimeAssembly, AssemblyError> {
        let input = try!(File::open(input_file));
        self.assemble(input)
    }

    fn assemble<R: Read>(&self, input: R) -> Result<RuntimeAssembly, AssemblyError> {
        let lines = try!(parser::read_lines(input));
        let mut errors: Vec<ProgramError> = Vec::new();
        let mut pre: SymbolicAssembly = Assembly::new();

        let parsed = parser::parse(&lines);

        // First loop, pre-processing: gather assembled elements and errors
        for (i, p) in parsed.iter().enumerate() {
            let line = &lines[i];
            match p {
                &Parsed::Label(ref label) => {
                    pre.define(&label[..]);
                },
                &Parsed::Mnemonic(ref par) => {
                    let from_mnemo: Result<SymbolicInst, _> = self.from_mnemo(par);
                    match from_mnemo {
                        Ok(inst) => {
                            let inst_len = inst.len();
                            let curr_addr = pre.ctx().curr_addr();
                            pre.push(Assembled::Inst(line.clone(), curr_addr, inst));
                            pre.inc_addr(inst_len);
                        },
                        Err(err) => {
                            errors.push(ProgramError::new(i, &line[..], &format!("{}", err)[..]));
                            pre.push(Assembled::Ignored(line.clone()));
                        },
                    }
                },
                &Parsed::Directive(ref par) => {
                    match Directive::from_params(par) {
                        Ok(_) => {
                            // TODO: apply the directive
                        },
                        Err(err) => {
                            errors.push(ProgramError::new(i, &line[..], &format!("{}", err)[..]));
                            pre.push(Assembled::Ignored(line.clone()));
                        },
                    }
                },
                &Parsed::Blank => {
                    pre.push(Assembled::Ignored(line.clone()));
                },
                &Parsed::LexicalError => {
                    errors.push(ProgramError::new_lexical_error(i, &line[..]));
                    pre.push(Assembled::Ignored(line.clone()));
                },
            };
        }

        // Second loop, post-processing: encode assembled instructions
        let mut post = Assembly::with_symbols(pre.ctx().symbols());
        for (i, a) in pre.assembled().iter().enumerate() {
            match a {
                &Assembled::Inst(ref l, p, ref inst) => {
                    match Self::assemble_inst(inst, post.ctx_mut()) {
                        Ok(asm_inst) =>
                            post.push(Assembled::Inst(l.clone(), p, asm_inst)),
                        Err(e) => errors.push(
                            ProgramError::new(i, l.trim(), &format!("{}", e)[..])),
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

    fn from_mnemo(&self, par: &Parameterized) -> Result<SymbolicInst, String> {
        let mnemo = par.elem();
        let args = par.params();
        match mnemo {
            "add" | "ADD" => Ok(Inst::Add(args[0].clone(), args[1].clone())),
            "adc" | "ADC" => Ok(Inst::Adc(args[0].clone(), args[1].clone())),
            "addi" | "ADDI" => Ok(Inst::Addi(args[0].clone(), args[1].clone())),
            "sub" | "SUB" => Ok(Inst::Sub(args[0].clone(), args[1].clone())),
            "sbc" | "SBC" => Ok(Inst::Sbc(args[0].clone(), args[1].clone())),
            "subi" | "SUBI" => Ok(Inst::Subi(args[0].clone(), args[1].clone())),
            "mulw" | "MULW" => Ok(Inst::Mulw(args[0].clone(), args[1].clone())),
            "and" | "AND" => Ok(Inst::And(args[0].clone(), args[1].clone())),
            "or" | "OR" => Ok(Inst::Or(args[0].clone(), args[1].clone())),
            "xor" | "XOR" => Ok(Inst::Xor(args[0].clone(), args[1].clone())),
            "lsl" | "LSL" => Ok(Inst::Lsl(args[0].clone(), args[1].clone())),
            "lsr" | "LSR" => Ok(Inst::Lsr(args[0].clone(), args[1].clone())),
            "asr" | "ASR" => Ok(Inst::Asr(args[0].clone(), args[1].clone())),
            "not" | "NOT" => Ok(Inst::Not(args[0].clone())),
            "comp" | "COMP" => Ok(Inst::Comp(args[0].clone())),
            "inc" | "INC" => Ok(Inst::Inc(args[0].clone())),
            "incw" | "INCW" => Ok(Inst::Incw(args[0].clone())),
            "dec" | "DEC" => Ok(Inst::Dec(args[0].clone())),
            "decw" | "DECW" => Ok(Inst::Decw(args[0].clone())),
            "mov" | "MOV" => Ok(Inst::Mov(args[0].clone(), args[1].clone())),
            "ld" | "LD" => Ok(Inst::Ld(args[0].clone(), args[1].clone())),
            "st" | "ST" => Ok(Inst::St(args[0].clone(), args[1].clone())),
            "ldd" | "LDD" => Ok(Inst::Ldd(args[0].clone(), args[1].clone())),
            "std" | "STD" => Ok(Inst::Std(args[0].clone(), args[1].clone())),
            "ldi" | "LDI" => Ok(Inst::Ldi(args[0].clone(), args[1].clone())),
            "ldsp" | "LDSP" => Ok(Inst::Ldsp(args[0].clone())),
            "push" | "PUSH" => Ok(Inst::Push(args[0].clone())),
            "pop" | "POP" => Ok(Inst::Pop(args[0].clone())),
            "je" | "JE" => Ok(Inst::Je(args[0].clone())),
            "jne" | "JNE" => Ok(Inst::Jne(args[0].clone())),
            "jl" | "JL" => Ok(Inst::Jl(args[0].clone())),
            "jge" | "JGE" => Ok(Inst::Jge(args[0].clone())),
            "jcc" | "JCC" => Ok(Inst::Jcc(args[0].clone())),
            "jcs" | "JCS" => Ok(Inst::Jcs(args[0].clone())),
            "jvc" | "JVC" => Ok(Inst::Jvc(args[0].clone())),
            "jvs" | "JVS" => Ok(Inst::Jvs(args[0].clone())),
            "jmp" | "JMP" => Ok(Inst::Jmp(args[0].clone())),
            "rjmp" | "RJMP" => Ok(Inst::Rjmp(args[0].clone())),
            "ijmp" | "IJMP" => Ok(Inst::Ijmp(args[0].clone())),
            "call" | "CALL" => Ok(Inst::Call(args[0].clone())),
            "rcall" | "RCALL" => Ok(Inst::Rcall(args[0].clone())),
            "icall" | "ICALL" => Ok(Inst::Icall(args[0].clone())),
            "ret" | "RET" => Ok(Inst::Ret),
            "reti" | "RETI" => Ok(Inst::Reti),
            "nop" | "NOP" => Ok(Inst::Nop),
            "halt" | "HALT" => Ok(Inst::Halt),
            _ => Err(format!("unknown mnemonic: `{}`", mnemo))
        }
    }
}
