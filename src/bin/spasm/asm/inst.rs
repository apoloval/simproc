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
    match &par.elem().to_ascii_lowercase()[..] {
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

    use super::*;

    #[test]
    fn should_display_unknown_mnemo() {
        let par = Parameterized::from_strings("foobar".to_string(), Vec::new());
        let disp = format!("{}", FromMnemoError::UnknownMnemo(par));
        assert_eq!("unknown mnemonic in `foobar`", disp);
    }

    #[test]
    fn should_display_invalid_operands_count() {
        let par = Parameterized::from_strings("foobar".to_string(), Vec::new());
        let disp = format!("{}", FromMnemoError::InvalidOperandsCount(par, 1));
        assert_eq!("invalid operand count in `foobar`: 1 operand(s) expected", disp);
    }
}