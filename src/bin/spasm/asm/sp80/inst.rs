//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use asm::inst::FromMnemo;

use simproc::sp80::{AssemblyArgs, Inst};

impl FromMnemo for Inst<AssemblyArgs> {
    type Err = String;

    fn from_mnemo(mnemo: &str, args: &[String]) -> Result<Inst<AssemblyArgs>, String> {
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
