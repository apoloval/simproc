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
use asm::ops::*;
use asm::parser;
use asm::parser::Parameterized;
use asm::parser::Parsed;

use simproc::inst::*;

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
                    match self.assemble_inst(inst, post.ctx_mut()) {
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

    fn assemble_inst(&self, from: &SymbolicInst, context: &mut AssemblyContext) ->
            Result<RuntimeInst, OpAssemblyError> {
        let mapper = OperandAssembler::with_context(context);
        match from {
            &Inst::Add(ref r1, ref r2) =>
                Ok(Inst::Add(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Adc(ref r1, ref r2) =>
                Ok(Inst::Adc(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Addi(ref r, ref k) =>
                Ok(Inst::Addi(try!(mapper.map_reg(r)), try!(mapper.map_immediate(k)))),
            &Inst::Sub(ref r1, ref r2) =>
                Ok(Inst::Sub(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Sbc(ref r1, ref r2) =>
                Ok(Inst::Sbc(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Subi(ref r, ref k) =>
                Ok(Inst::Subi(try!(mapper.map_reg(r)), try!(mapper.map_immediate(k)))),
            &Inst::Mulw(ref a1, ref a2) =>
                Ok(Inst::Mulw(try!(mapper.map_addr_reg(a1)), try!(mapper.map_addr_reg(a2)))),
            &Inst::And(ref r1, ref r2) =>
                Ok(Inst::And(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Or(ref r1, ref r2) =>
                Ok(Inst::Or(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Xor(ref r1, ref r2) =>
                Ok(Inst::Xor(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Lsl(ref r1, ref r2) =>
                Ok(Inst::Lsl(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Lsr(ref r1, ref r2) =>
                Ok(Inst::Lsr(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Asr(ref r1, ref r2) =>
                Ok(Inst::Asr(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Not(ref r) =>
                Ok(Inst::Not(try!(mapper.map_reg(r)))),
            &Inst::Comp(ref r) =>
                Ok(Inst::Comp(try!(mapper.map_reg(r)))),
            &Inst::Inc(ref r) =>
                Ok(Inst::Inc(try!(mapper.map_reg(r)))),
            &Inst::Incw(ref a) =>
                Ok(Inst::Incw(try!(mapper.map_addr_reg(a)))),
            &Inst::Dec(ref r) =>
                Ok(Inst::Dec(try!(mapper.map_reg(r)))),
            &Inst::Decw(ref a) =>
                Ok(Inst::Decw(try!(mapper.map_addr_reg(a)))),

            &Inst::Mov(ref r1, ref r2) =>
                Ok(Inst::Mov(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &Inst::Ld(ref r, ref a) =>
                Ok(Inst::Ld(try!(mapper.map_reg(r)), try!(mapper.map_addr_reg(a)))),
            &Inst::St(ref a, ref r) =>
                Ok(Inst::St(try!(mapper.map_addr_reg(a)), try!(mapper.map_reg(r)))),
            &Inst::Ldd(ref r, ref a) =>
                Ok(Inst::Ldd(try!(mapper.map_reg(r)), try!(mapper.map_addr(a)))),
            &Inst::Std(ref a, ref r) =>
                Ok(Inst::Std(try!(mapper.map_addr(a)), try!(mapper.map_reg(r)))),
            &Inst::Ldi(ref r, ref k) =>
                Ok(Inst::Ldi(try!(mapper.map_reg(r)), try!(mapper.map_immediate(k)))),
            &Inst::Ldsp(ref a) =>
                Ok(Inst::Ldsp(try!(mapper.map_addr_reg(a)))),
            &Inst::Push(ref r) =>
                Ok(Inst::Push(try!(mapper.map_reg(r)))),
            &Inst::Pop(ref r) =>
                Ok(Inst::Pop(try!(mapper.map_reg(r)))),
            &Inst::Je(ref o) =>
                Ok(Inst::Je(try!(mapper.map_rel_addr(o)))),
            &Inst::Jne(ref o) =>
                Ok(Inst::Jne(try!(mapper.map_rel_addr(o)))),
            &Inst::Jl(ref o) =>
                Ok(Inst::Jl(try!(mapper.map_rel_addr(o)))),
            &Inst::Jge(ref o) =>
                Ok(Inst::Jge(try!(mapper.map_rel_addr(o)))),
            &Inst::Jcc(ref o) =>
                Ok(Inst::Jcc(try!(mapper.map_rel_addr(o)))),
            &Inst::Jcs(ref o) =>
                Ok(Inst::Jcs(try!(mapper.map_rel_addr(o)))),
            &Inst::Jvc(ref o) =>
                Ok(Inst::Jvc(try!(mapper.map_rel_addr(o)))),
            &Inst::Jvs(ref o) =>
                Ok(Inst::Jvs(try!(mapper.map_rel_addr(o)))),
            &Inst::Jmp(ref a) =>
                Ok(Inst::Jmp(try!(mapper.map_addr(a)))),
            &Inst::Rjmp(ref o) =>
                Ok(Inst::Rjmp(try!(mapper.map_rel_addr(o)))),
            &Inst::Ijmp(ref a) =>
                Ok(Inst::Ijmp(try!(mapper.map_addr_reg(a)))),
            &Inst::Call(ref a) =>
                Ok(Inst::Call(try!(mapper.map_addr(a)))),
            &Inst::Rcall(ref o) =>
                Ok(Inst::Rcall(try!(mapper.map_rel_addr(o)))),
            &Inst::Icall(ref a) =>
                Ok(Inst::Icall(try!(mapper.map_addr_reg(a)))),
            &Inst::Ret =>
                Ok(Inst::Ret),
            &Inst::Reti =>
                Ok(Inst::Reti),
            &Inst::Nop =>
                Ok(Inst::Nop),
            &Inst::Halt =>
                Ok(Inst::Halt),
        }
    }
}
