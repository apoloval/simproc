//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;

use simproc::inst::*;

use asm::assembly::*;
use asm::err::Error;
use asm::ops::*;
use asm::parser::Parameterized;

macro_rules! ensure_args {
    ($par:expr => $expected:expr) => {
        if $par.params().len() != $expected {
            return Err(Error::BadOperandsCount($expected));
        }
    }
}

fn nullary_inst(par: &Parameterized, inst: SymbolicInst) -> Result<SymbolicInst, Error> {
    ensure_args!(par => 0);
    Ok(inst)
}

fn unary_inst<F>(par: &Parameterized, inst: F) -> Result<SymbolicInst, Error>
        where F: FnOnce(String) -> SymbolicInst {
    ensure_args!(par => 1);
    let arg0 = par.params()[0].clone();
    Ok(inst(arg0))
}

fn binary_inst<F>(par: &Parameterized, inst: F) -> Result<SymbolicInst, Error>
        where F: FnOnce(String, String) -> SymbolicInst {
    ensure_args!(par => 2);
    let arg0 = par.params()[0].clone();
    let arg1 = par.params()[1].clone();
    Ok(inst(arg0, arg1))
}

/// Returns a symbolic instruction from a parameterized mnemotechnic
pub fn from_mnemo(par: &Parameterized) -> Result<SymbolicInst, Error> {
    let mnemo = par.elem();
    match mnemo.to_ascii_lowercase().trim() {
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
        _ => Err(Error::UnknownMnemo(mnemo.to_string()))
    }
}

pub fn assemble_inst(from: &SymbolicInst, context: &mut AssemblyContext) ->
        Result<RuntimeInst, Error> {
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

#[cfg(test)]
mod test {

    use simproc::inst::*;

    use asm::err::Error;
    use asm::parser::Parameterized;

    use super::*;

    #[test]
    fn should_convert_add_from_mnemo() {
        should_convert_binary_from_mnemo("add", Inst::Add);
    }

    #[test]
    fn should_convert_adc_from_mnemo() {
        should_convert_binary_from_mnemo("adc", Inst::Adc);
    }

    #[test]
    fn should_convert_addi_from_mnemo() {
        should_convert_binary_from_mnemo("addi", Inst::Addi);
    }

    #[test]
    fn should_convert_sub_from_mnemo() {
        should_convert_binary_from_mnemo("sub", Inst::Sub);
    }

    #[test]
    fn should_convert_sbc_from_mnemo() {
        should_convert_binary_from_mnemo("sbc", Inst::Sbc);
    }

    #[test]
    fn should_convert_subi_from_mnemo() {
        should_convert_binary_from_mnemo("subi", Inst::Subi);
    }

    #[test]
    fn should_convert_mulw_from_mnemo() {
        should_convert_binary_from_mnemo("mulw", Inst::Mulw);
    }

    #[test]
    fn should_convert_and_from_mnemo() {
        should_convert_binary_from_mnemo("and", Inst::And);
    }

    #[test]
    fn should_convert_or_from_mnemo() {
        should_convert_binary_from_mnemo("or", Inst::Or);
    }

    #[test]
    fn should_convert_xor_from_mnemo() {
        should_convert_binary_from_mnemo("xor", Inst::Xor);
    }

    #[test]
    fn should_convert_lsl_from_mnemo() {
        should_convert_binary_from_mnemo("lsl", Inst::Lsl);
    }

    #[test]
    fn should_convert_lsr_from_mnemo() {
        should_convert_binary_from_mnemo("lsr", Inst::Lsr);
    }

    #[test]
    fn should_convert_asr_from_mnemo() {
        should_convert_binary_from_mnemo("asr", Inst::Asr);
    }

    #[test]
    fn should_convert_not_from_mnemo() {
        should_convert_unary_from_mnemo("not", Inst::Not);
    }

    #[test]
    fn should_convert_comp_from_mnemo() {
        should_convert_unary_from_mnemo("comp", Inst::Comp);
    }

    #[test]
    fn should_convert_inc_from_mnemo() {
        should_convert_unary_from_mnemo("inc", Inst::Inc);
    }

    #[test]
    fn should_convert_incw_from_mnemo() {
        should_convert_unary_from_mnemo("incw", Inst::Incw);
    }

    #[test]
    fn should_convert_dec_from_mnemo() {
        should_convert_unary_from_mnemo("dec", Inst::Dec);
    }

    #[test]
    fn should_convert_decw_from_mnemo() {
        should_convert_unary_from_mnemo("decw", Inst::Decw);
    }

    #[test]
    fn should_convert_mov_from_mnemo() {
        should_convert_binary_from_mnemo("mov", Inst::Mov);
    }

    #[test]
    fn should_convert_ld_from_mnemo() {
        should_convert_binary_from_mnemo("ld", Inst::Ld);
    }

    #[test]
    fn should_convert_st_from_mnemo() {
        should_convert_binary_from_mnemo("st", Inst::St);
    }

    #[test]
    fn should_convert_ldd_from_mnemo() {
        should_convert_binary_from_mnemo("ldd", Inst::Ldd);
    }

    #[test]
    fn should_convert_std_from_mnemo() {
        should_convert_binary_from_mnemo("std", Inst::Std);
    }

    #[test]
    fn should_convert_ldi_from_mnemo() {
        should_convert_binary_from_mnemo("ldi", Inst::Ldi);
    }

    #[test]
    fn should_convert_ldsp_from_mnemo() {
        should_convert_unary_from_mnemo("ldsp", Inst::Ldsp);
    }

    #[test]
    fn should_convert_push_from_mnemo() {
        should_convert_unary_from_mnemo("push", Inst::Push);
    }

    #[test]
    fn should_convert_pop_from_mnemo() {
        should_convert_unary_from_mnemo("pop", Inst::Pop);
    }

    #[test]
    fn should_convert_je_from_mnemo() {
        should_convert_unary_from_mnemo("je", Inst::Je);
    }

    #[test]
    fn should_convert_jne_from_mnemo() {
        should_convert_unary_from_mnemo("jne", Inst::Jne);
    }

    #[test]
    fn should_convert_jl_from_mnemo() {
        should_convert_unary_from_mnemo("jl", Inst::Jl);
    }

    #[test]
    fn should_convert_jge_from_mnemo() {
        should_convert_unary_from_mnemo("jge", Inst::Jge);
    }

    #[test]
    fn should_convert_jcc_from_mnemo() {
        should_convert_unary_from_mnemo("jcc", Inst::Jcc);
    }

    #[test]
    fn should_convert_jcs_from_mnemo() {
        should_convert_unary_from_mnemo("jcs", Inst::Jcs);
    }

    #[test]
    fn should_convert_jvc_from_mnemo() {
        should_convert_unary_from_mnemo("jvc", Inst::Jvc);
    }

    #[test]
    fn should_convert_jvs_from_mnemo() {
        should_convert_unary_from_mnemo("jvs", Inst::Jvs);
    }

    #[test]
    fn should_convert_jmp_from_mnemo() {
        should_convert_unary_from_mnemo("jmp", Inst::Jmp);
    }

    #[test]
    fn should_convert_rjmp_from_mnemo() {
        should_convert_unary_from_mnemo("rjmp", Inst::Rjmp);
    }

    #[test]
    fn should_convert_ijmp_from_mnemo() {
        should_convert_unary_from_mnemo("ijmp", Inst::Ijmp);
    }

    #[test]
    fn should_convert_call_from_mnemo() {
        should_convert_unary_from_mnemo("call", Inst::Call);
    }

    #[test]
    fn should_convert_rcall_from_mnemo() {
        should_convert_unary_from_mnemo("rcall", Inst::Rcall);
    }

    #[test]
    fn should_convert_icall_from_mnemo() {
        should_convert_unary_from_mnemo("icall", Inst::Icall);
    }

    #[test]
    fn should_convert_ret_from_mnemo() {
        should_convert_nullary_from_mnemo("ret", Inst::Ret);
    }

    #[test]
    fn should_convert_reti_from_mnemo() {
        should_convert_nullary_from_mnemo("reti", Inst::Reti);
    }

    #[test]
    fn should_convert_nop_from_mnemo() {
        should_convert_nullary_from_mnemo("nop", Inst::Nop);
    }

    #[test]
    fn should_convert_halt_from_mnemo() {
        should_convert_nullary_from_mnemo("halt", Inst::Halt);
    }

    fn should_convert_nullary_from_mnemo(iname: &str, inst: SymbolicInst) {
        assert_eq!(Ok(inst), from_mnemo(&param!(iname)));
        should_fail_with_invalid_operands(&param!(iname, "p1"), 0);
        should_fail_with_invalid_operands(&param!(iname, "p1", "p2"), 0);
    }

    fn should_convert_unary_from_mnemo<I>(iname: &str, inst: I)
            where I: FnOnce(String) -> SymbolicInst {
        assert_eq!(
            Ok(inst("p1".to_string())),
            from_mnemo(&param!(iname, "p1")));
        should_fail_with_invalid_operands(&param!(iname), 1);
        should_fail_with_invalid_operands(&param!(iname, "p1", "p2"), 1);
    }

    fn should_convert_binary_from_mnemo<I>(iname: &str, inst: I)
            where I: FnOnce(String, String) -> SymbolicInst {
        assert_eq!(
            Ok(inst("p1".to_string(), "p2".to_string())),
            from_mnemo(&param!(iname, "p1", "p2")));
        should_fail_with_invalid_operands(&param!(iname), 2);
        should_fail_with_invalid_operands(&param!(iname, "p1"), 2);
        should_fail_with_invalid_operands(&param!(iname, "p1", "p2", "p3"), 2);
    }

    fn should_fail_with_invalid_operands(params: &Parameterized, expected: usize) {
        assert_eq!(
            Err(Error::BadOperandsCount(expected)),
            from_mnemo(params));
    }
}
