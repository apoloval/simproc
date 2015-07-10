//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ascii::AsciiExt;
use std::fmt;

use simproc::inst::*;

use asm::parser::Parameterized;

#[derive(Debug, PartialEq)]
pub enum FromMnemoError {
    UnknownMnemo(Parameterized),
    InvalidOperandsCount(Parameterized, usize),
}

impl fmt::Display for FromMnemoError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &FromMnemoError::UnknownMnemo(ref par) =>
                write!(fmt, "unknown mnemonic in `{}`", par),
            &FromMnemoError::InvalidOperandsCount(ref par, ref c) =>
                write!(fmt, "invalid operand count in `{}`: {} operand(s) expected", par, c),
        }
    }
}

macro_rules! ensure_args {
    ($par:expr => $expected:expr) => {
        if $par.params().len() != $expected {
            return Err(FromMnemoError::InvalidOperandsCount($par.clone(), $expected));
        }
    }
}

fn nullary_inst(par: &Parameterized, inst: SymbolicInst) -> Result<SymbolicInst, FromMnemoError> {
    ensure_args!(par => 0);
    Ok(inst)
}

fn unary_inst<F>(par: &Parameterized, inst: F) -> Result<SymbolicInst, FromMnemoError>
        where F: FnOnce(String) -> SymbolicInst {
    ensure_args!(par => 1);
    let arg0 = par.params()[0].clone();
    Ok(inst(arg0))
}

fn binary_inst<F>(par: &Parameterized, inst: F) -> Result<SymbolicInst, FromMnemoError>
        where F: FnOnce(String, String) -> SymbolicInst {
    ensure_args!(par => 2);
    let arg0 = par.params()[0].clone();
    let arg1 = par.params()[1].clone();
    Ok(inst(arg0, arg1))
}

/// Returns a symbolic instruction from a parameterized mnemotechnic
pub fn from_mnemo(par: &Parameterized) -> Result<SymbolicInst, FromMnemoError> {
    match par.elem().to_ascii_lowercase().trim() {
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
        _ => Err(FromMnemoError::UnknownMnemo(par.clone()))
    }
}

#[cfg(test)]
mod test {

    use asm::parser::Parameterized;

    use simproc::inst::*;

    use super::*;

    #[test]
    fn should_display_unknown_mnemo() {
        let disp = format!("{}", FromMnemoError::UnknownMnemo(param!("foobar")));
        assert_eq!("unknown mnemonic in `foobar`", disp);
    }

    #[test]
    fn should_display_invalid_operands_count() {
        let disp = format!("{}", FromMnemoError::InvalidOperandsCount(param!("foobar"), 1));
        assert_eq!("invalid operand count in `foobar`: 1 operand(s) expected", disp);
    }

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
            Err(FromMnemoError::InvalidOperandsCount(params.clone(), expected)),
            from_mnemo(params));
    }
}
