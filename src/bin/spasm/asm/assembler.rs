//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
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

        macro_rules! ensure_args {
            ($par:expr => $expected:expr) => {
                if $par.params().len() != $expected {
                    return Err(format!("invalid operand count in `{}`: {} operand(s) expected",
                        $par, $expected));
                }
            }
        }

        fn nullary_inst(par: &Parameterized, inst: SymbolicInst) -> Result<SymbolicInst, String> {
            ensure_args!(par => 0);
            Ok(inst)
        }

        fn unary_inst<F>(par: &Parameterized, inst: F) -> Result<SymbolicInst, String>
                where F: FnOnce(String) -> SymbolicInst {
            ensure_args!(par => 1);
            let arg0 = par.params()[0].clone();
            Ok(inst(arg0))
        }

        fn binary_inst<F>(par: &Parameterized, inst: F) -> Result<SymbolicInst, String>
                where F: FnOnce(String, String) -> SymbolicInst {
            ensure_args!(par => 2);
            let arg0 = par.params()[0].clone();
            let arg1 = par.params()[1].clone();
            Ok(inst(arg0, arg1))
        }

        let mnemo = par.elem();
        match &mnemo.to_ascii_lowercase()[..] {
            "add" => binary_inst(par, Inst::Add),
            "adc" => binary_inst(par, Inst::Adc),
            "addi" => binary_inst(par, Inst::Addi),
            "sub" => binary_inst(par, Inst::Sub),
            "sbc" => binary_inst(par, Inst::Sbc),
            "subi" => binary_inst(par, Inst::Subi),
            "mulw" => binary_inst(par, Inst::Mulw),
            "and" => binary_inst(par, Inst::And),
            "or" => binary_inst(par, Inst::Or),
            "xor" => binary_inst(par, Inst::Xor),
            "lsl" => binary_inst(par, Inst::Lsl),
            "lsr" => binary_inst(par, Inst::Lsr),
            "asr" => binary_inst(par, Inst::Asr),
            "not" => unary_inst(par, Inst::Not),
            "comp" => unary_inst(par, Inst::Comp),
            "inc" => unary_inst(par, Inst::Inc),
            "incw" => unary_inst(par, Inst::Incw),
            "dec" => unary_inst(par, Inst::Dec),
            "decw" => unary_inst(par, Inst::Decw),
            "mov" => binary_inst(par, Inst::Mov),
            "ld" => binary_inst(par, Inst::Ld),
            "st" => binary_inst(par, Inst::St),
            "ldd" => binary_inst(par, Inst::Ldd),
            "std" => binary_inst(par, Inst::Std),
            "ldi" => binary_inst(par, Inst::Ldi),
            "ldsp" => unary_inst(par, Inst::Ldsp),
            "push" => unary_inst(par, Inst::Push),
            "pop" => unary_inst(par, Inst::Pop),
            "je" => unary_inst(par, Inst::Je),
            "jne" => unary_inst(par, Inst::Jne),
            "jl" => unary_inst(par, Inst::Jl),
            "jge" => unary_inst(par, Inst::Jge),
            "jcc" => unary_inst(par, Inst::Jcc),
            "jcs" => unary_inst(par, Inst::Jcs),
            "jvc" => unary_inst(par, Inst::Jvc),
            "jvs" => unary_inst(par, Inst::Jvs),
            "jmp" => unary_inst(par, Inst::Jmp),
            "rjmp" => unary_inst(par, Inst::Rjmp),
            "ijmp" => unary_inst(par, Inst::Ijmp),
            "call" => unary_inst(par, Inst::Call),
            "rcall" => unary_inst(par, Inst::Rcall),
            "icall" => unary_inst(par, Inst::Icall),
            "ret" => nullary_inst(par, Inst::Ret),
            "reti" => nullary_inst(par, Inst::Reti),
            "nop" => nullary_inst(par, Inst::Nop),
            "halt" => nullary_inst(par, Inst::Halt),
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
