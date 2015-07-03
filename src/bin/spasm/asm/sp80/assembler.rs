//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use simproc::sp80;

use asm::{Assembly, AssemblyContext};
use asm::assembler;
use asm::sp80::ops;

pub struct Assembler;

pub type RuntimeAssembly = Assembly<sp80::RuntimeInst>;

impl assembler::Assembler for Assembler {
    type AssemblyInst = sp80::AssemblyInst;
    type RuntimeInst = sp80::RuntimeInst;
    type AssemblyErr = ops::OpAssemblyError;

    fn new() -> Assembler { Assembler }

    fn assemble_inst(from: &sp80::AssemblyInst, context: &mut AssemblyContext) ->
            Result<sp80::RuntimeInst, ops::OpAssemblyError> {
        let mapper = ops::OperandAssembler::with_context(context);
        match from {
            &sp80::Inst::Add(ref r1, ref r2) =>
                Ok(sp80::Inst::Add(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Adc(ref r1, ref r2) =>
                Ok(sp80::Inst::Adc(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Addi(ref r, ref k) =>
                Ok(sp80::Inst::Addi(try!(mapper.map_reg(r)), try!(mapper.map_immediate(k)))),
            &sp80::Inst::Sub(ref r1, ref r2) =>
                Ok(sp80::Inst::Sub(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Sbc(ref r1, ref r2) =>
                Ok(sp80::Inst::Sbc(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Subi(ref r, ref k) =>
                Ok(sp80::Inst::Subi(try!(mapper.map_reg(r)), try!(mapper.map_immediate(k)))),
            &sp80::Inst::Mulw(ref a1, ref a2) =>
                Ok(sp80::Inst::Mulw(try!(mapper.map_addr_reg(a1)), try!(mapper.map_addr_reg(a2)))),
            &sp80::Inst::And(ref r1, ref r2) =>
                Ok(sp80::Inst::And(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Or(ref r1, ref r2) =>
                Ok(sp80::Inst::Or(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Xor(ref r1, ref r2) =>
                Ok(sp80::Inst::Xor(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Lsl(ref r1, ref r2) =>
                Ok(sp80::Inst::Lsl(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Lsr(ref r1, ref r2) =>
                Ok(sp80::Inst::Lsr(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Asr(ref r1, ref r2) =>
                Ok(sp80::Inst::Asr(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Not(ref r) =>
                Ok(sp80::Inst::Not(try!(mapper.map_reg(r)))),
            &sp80::Inst::Comp(ref r) =>
                Ok(sp80::Inst::Comp(try!(mapper.map_reg(r)))),
            &sp80::Inst::Inc(ref r) =>
                Ok(sp80::Inst::Inc(try!(mapper.map_reg(r)))),
            &sp80::Inst::Incw(ref a) =>
                Ok(sp80::Inst::Incw(try!(mapper.map_addr_reg(a)))),
            &sp80::Inst::Dec(ref r) =>
                Ok(sp80::Inst::Dec(try!(mapper.map_reg(r)))),
            &sp80::Inst::Decw(ref a) =>
                Ok(sp80::Inst::Decw(try!(mapper.map_addr_reg(a)))),

            &sp80::Inst::Mov(ref r1, ref r2) =>
                Ok(sp80::Inst::Mov(try!(mapper.map_reg(r1)), try!(mapper.map_reg(r2)))),
            &sp80::Inst::Ld(ref r, ref a) =>
                Ok(sp80::Inst::Ld(try!(mapper.map_reg(r)), try!(mapper.map_addr_reg(a)))),
            &sp80::Inst::St(ref a, ref r) =>
                Ok(sp80::Inst::St(try!(mapper.map_addr_reg(a)), try!(mapper.map_reg(r)))),
            &sp80::Inst::Ldd(ref r, ref a) =>
                Ok(sp80::Inst::Ldd(try!(mapper.map_reg(r)), try!(mapper.map_addr(a)))),
            &sp80::Inst::Std(ref a, ref r) =>
                Ok(sp80::Inst::Std(try!(mapper.map_addr(a)), try!(mapper.map_reg(r)))),
            &sp80::Inst::Ldi(ref r, ref k) =>
                Ok(sp80::Inst::Ldi(try!(mapper.map_reg(r)), try!(mapper.map_immediate(k)))),
            &sp80::Inst::Ldsp(ref a) =>
                Ok(sp80::Inst::Ldsp(try!(mapper.map_addr_reg(a)))),
            &sp80::Inst::Push(ref r) =>
                Ok(sp80::Inst::Push(try!(mapper.map_reg(r)))),
            &sp80::Inst::Pop(ref r) =>
                Ok(sp80::Inst::Pop(try!(mapper.map_reg(r)))),
            &sp80::Inst::Je(ref o) =>
                Ok(sp80::Inst::Je(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Jne(ref o) =>
                Ok(sp80::Inst::Jne(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Jl(ref o) =>
                Ok(sp80::Inst::Jl(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Jge(ref o) =>
                Ok(sp80::Inst::Jge(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Jcc(ref o) =>
                Ok(sp80::Inst::Jcc(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Jcs(ref o) =>
                Ok(sp80::Inst::Jcs(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Jvc(ref o) =>
                Ok(sp80::Inst::Jvc(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Jvs(ref o) =>
                Ok(sp80::Inst::Jvs(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Jmp(ref a) =>
                Ok(sp80::Inst::Jmp(try!(mapper.map_addr(a)))),
            &sp80::Inst::Rjmp(ref o) =>
                Ok(sp80::Inst::Rjmp(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Ijmp(ref a) =>
                Ok(sp80::Inst::Ijmp(try!(mapper.map_addr_reg(a)))),
            &sp80::Inst::Call(ref a) =>
                Ok(sp80::Inst::Call(try!(mapper.map_addr(a)))),
            &sp80::Inst::Rcall(ref o) =>
                Ok(sp80::Inst::Rcall(try!(mapper.map_rel_addr(o)))),
            &sp80::Inst::Icall(ref a) =>
                Ok(sp80::Inst::Icall(try!(mapper.map_addr_reg(a)))),
            &sp80::Inst::Ret =>
                Ok(sp80::Inst::Ret),
            &sp80::Inst::Reti =>
                Ok(sp80::Inst::Reti),
            &sp80::Inst::Nop =>
                Ok(sp80::Inst::Nop),
            &sp80::Inst::Halt =>
                Ok(sp80::Inst::Halt),
        }
    }
}
