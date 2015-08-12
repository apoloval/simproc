//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use simproc::inst::*;

use asm::lexer::*;
use asm::new_parser::Expr;
use asm::pre::*;

#[derive(Debug, PartialEq)]
pub enum FullAssembled {
    Inst { loc: TextLoc, base_addr: Addr, inst: RuntimeInst },
}

#[derive(Debug, PartialEq)]
pub enum FullAssembleError {
    // Pre(PreAssembleError),
    TypeMismatch { loc: TextLoc, expected: String },
    NotImplemented,
}

pub type FullAssemblerInput = PreAssemblerOutput;
pub type FullAssemblerOutput = Result<FullAssembled, FullAssembleError>;

pub struct FullAssembler<'a, I: Iterator<Item=FullAssemblerInput>> {
    input: I,
    symbols: &'a SymbolTable,
}

impl<'a, I: Iterator<Item=FullAssemblerInput>> FullAssembler<'a, I> {

    pub fn from<P>(pre: P, symbols: &'a SymbolTable) -> Self
        where P: IntoIterator<Item=FullAssemblerInput, IntoIter=I>
    {
        FullAssembler { input: pre.into_iter(), symbols: symbols }
    }
}

impl<'a, I: Iterator<Item=FullAssemblerInput>> Iterator for FullAssembler<'a, I> {

    type Item = FullAssemblerOutput;

    fn next(&mut self) -> Option<FullAssemblerOutput> {
        match self.input.next() {
            Some(Ok(PreAssembled::Inst { loc, base_addr, inst, .. })) => {
                Some(full_assemble_inst(inst, self.symbols).map(|i| FullAssembled::Inst {
                    loc: loc, base_addr: base_addr, inst: i,
                }))
            },
            _ => None,
        }
    }
}

pub fn full_assemble_inst(
    inst: PreAssembledInst,
    symbols: &SymbolTable) -> Result<RuntimeInst, FullAssembleError>
{
    match inst {
        Inst::Add(r1, r2) => Ok(Inst::Add(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Adc(r1, r2) => Ok(Inst::Adc(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Addi(r, l) => Ok(Inst::Addi(try!(to_reg(r)), try!(to_immediate(l, symbols)))),
        Inst::Sub(r1, r2) => Ok(Inst::Sub(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Sbc(r1, r2) => Ok(Inst::Sbc(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Subi(r, l) => Ok(Inst::Subi(try!(to_reg(r)), try!(to_immediate(l, symbols)))),
        Inst::Mulw(r1, r2) => Ok(Inst::Mulw(try!(to_areg(r1)), try!(to_areg(r2)))),
        Inst::And(r1, r2) => Ok(Inst::And(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Or(r1, r2) => Ok(Inst::Or(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Xor(r1, r2) => Ok(Inst::Xor(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Lsl(r1, r2) => Ok(Inst::Lsl(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Lsr(r1, r2) => Ok(Inst::Lsr(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Asr(r1, r2) => Ok(Inst::Asr(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Not(r) => Ok(Inst::Not(try!(to_reg(r)))),
        Inst::Comp(r) => Ok(Inst::Comp(try!(to_reg(r)))),
        Inst::Inc(r) => Ok(Inst::Inc(try!(to_reg(r)))),
        Inst::Incw(r) => Ok(Inst::Incw(try!(to_areg(r)))),
        Inst::Dec(r) => Ok(Inst::Dec(try!(to_reg(r)))),
        Inst::Decw(r) => Ok(Inst::Decw(try!(to_areg(r)))),
        Inst::Mov(r1, r2) => Ok(Inst::Mov(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Ldi(r, l) => Ok(Inst::Ldi(try!(to_reg(r)), try!(to_immediate(l, symbols)))),
        Inst::Ldsp(r) => Ok(Inst::Ldsp(try!(to_areg(r)))),
        Inst::Push(r) => Ok(Inst::Push(try!(to_reg(r)))),
        Inst::Pop(r) => Ok(Inst::Pop(try!(to_reg(r)))),

        Inst::Ijmp(r) => Ok(Inst::Ijmp(try!(to_areg(r)))),
        Inst::Icall(r) => Ok(Inst::Icall(try!(to_areg(r)))),
        Inst::Nop => Ok(Inst::Nop),
        _ => Err(FullAssembleError::NotImplemented),
    }
}

fn to_reg(e: Expr) -> Result<Reg, FullAssembleError> {
    match e {
        Expr::Reg(_, reg) => Ok(reg),
        e => Err(FullAssembleError::TypeMismatch {
            loc: e.loc().clone(),
            expected: "register name".to_string()
        })
    }
}

fn to_areg(e: Expr) -> Result<AddrReg, FullAssembleError> {
    match e {
        Expr::AddrReg(_, reg) => Ok(reg),
        e => Err(FullAssembleError::TypeMismatch {
            loc: e.loc().clone(),
            expected: "address register name".to_string()
        })
    }
}

fn to_immediate(e: Expr, _symbols: &SymbolTable) -> Result<Immediate, FullAssembleError> {
    match e {
        Expr::Number(_, n) => Ok(Immediate(n as u8)),
        e => Err(FullAssembleError::TypeMismatch {
            loc: e.loc().clone(),
            expected: "immediate value".to_string()
        })
    }
}

#[cfg(test)]
mod test {

    use quickcheck::*;
    use simproc::inst::*;

    use asm::lexer::TextLoc;
    use asm::new_parser::Expr;
    use asm::pre::*;

    use super::*;

    macro_rules! should_assemble_type_mismatch {
        ($i:expr, $e1:expr) => ({
            let symbols = SymbolTable::new();
            match full_assemble_inst($i($e1), &symbols) {
                Err(FullAssembleError::TypeMismatch { loc: _, expected: _ }) =>
                    TestResult::passed(),
                r => TestResult::error(format!("unexpected result: {:?}", r)),
            }
        });
        ($i:expr, $e1:expr, $e2:expr) => ({
            let symbols = SymbolTable::new();
            match full_assemble_inst($i($e1, $e2), &symbols) {
                Err(FullAssembleError::TypeMismatch { loc: _, expected: _ }) =>
                    TestResult::passed(),
                r => TestResult::error(format!("unexpected result: {:?}", r)),
            }
        });
    }

    macro_rules! should_assemble_reg_reg {
        ($i:expr) => ({
            fn assemble_with_operands(op1: Expr, op2: Expr) -> TestResult {
                match (op1, op2) {
                    (Expr::Reg(l1, r1), Expr::Reg(l2, r2)) => {
                        let symbols = SymbolTable::new();
                        let expected = Ok($i(r1, r2));
                        let actual = full_assemble_inst(
                            $i(Expr::Reg(l1, r1), Expr::Reg(l2, r2)),
                            &symbols);
                        TestResult::from_bool(actual == expected)
                    },
                    (e1, e2) => { should_assemble_type_mismatch!($i, e1, e2) },
                }
            }
            quickcheck(assemble_with_operands as fn(Expr, Expr) -> TestResult);
        })
    }

    macro_rules! should_assemble_reg_imm {
        ($i:path) => ({
            fn assemble_with_operands(op1: Expr, op2: Expr) -> TestResult {
                match (op1, op2) {
                    (Expr::Reg(l1, r), Expr::Number(l2, n)) => {
                        let symbols = SymbolTable::new();
                        let pre = $i(
                            Expr::Reg(l1, r),
                            Expr::Number(l2, n));
                        match full_assemble_inst(pre, &symbols) {
                            Ok($i(rr, _)) =>
                                TestResult::from_bool(r == rr),
                            r => TestResult::error(format!("unexpected result: {:?}", r)),
                        }
                    },
                    (e1, e2) => { should_assemble_type_mismatch!($i, e1, e2) },
                }
            }
            quickcheck(assemble_with_operands as fn(Expr, Expr) -> TestResult);
        })
    }

    macro_rules! should_assemble_areg_areg {
        ($i:expr) => ({
            fn assemble_with_operands(op1: Expr, op2: Expr) -> TestResult {
                match (op1, op2) {
                    (Expr::AddrReg(l1, r1), Expr::AddrReg(l2, r2)) => {
                        let symbols = SymbolTable::new();
                        let expected = Ok($i(r1, r2));
                        let actual = full_assemble_inst(
                            $i(Expr::AddrReg(l1, r1), Expr::AddrReg(l2, r2)),
                            &symbols);
                        TestResult::from_bool(actual == expected)
                    },
                    (e1, e2) => { should_assemble_type_mismatch!($i, e1, e2) },
                }
            }
            quickcheck(assemble_with_operands as fn(Expr, Expr) -> TestResult);
        })
    }

    macro_rules! should_assemble_reg {
        ($i:expr) => ({
            fn assemble_with_operands(op1: Expr) -> TestResult {
                match op1 {
                    Expr::Reg(l1, r1) => {
                        let symbols = SymbolTable::new();
                        let expected = Ok($i(r1));
                        let actual = full_assemble_inst(
                            $i(Expr::Reg(l1, r1)),
                            &symbols);
                        TestResult::from_bool(actual == expected)
                    },
                    e1 => { should_assemble_type_mismatch!($i, e1) },
                }
            }
            quickcheck(assemble_with_operands as fn(Expr) -> TestResult);
        })
    }

    macro_rules! should_assemble_areg {
        ($i:expr) => ({
            fn assemble_with_operands(op1: Expr) -> TestResult {
                match op1 {
                    Expr::AddrReg(l1, r1) => {
                        let symbols = SymbolTable::new();
                        let expected = Ok($i(r1));
                        let actual = full_assemble_inst(
                            $i(Expr::AddrReg(l1, r1)),
                            &symbols);
                        TestResult::from_bool(actual == expected)
                    },
                    e1 => { should_assemble_type_mismatch!($i, e1) },
                }
            }
            quickcheck(assemble_with_operands as fn(Expr) -> TestResult);
        })
    }

    #[test]
    fn should_assemble_empty_program() {
        let input = Vec::new();
        let symbols = SymbolTable::new();
        let mut full = FullAssembler::from(input, &symbols);
        assert_eq!(full.next(), None);
    }

    #[test]
    fn should_assemble_inst() {
        let input = vec![
            Ok(PreAssembled::Inst {
                loc: loc!(1, 1, "nop"),
                base_addr: Addr(0x100),
                inst: Inst::Nop,
            }),
        ];
        let symbols = SymbolTable::new();
        let mut full = FullAssembler::from(input, &symbols);
        assert_eq!(full.next(), Some(Ok(FullAssembled::Inst {
            loc: loc!(1, 1, "nop"),
            base_addr: Addr(0x100),
            inst: Inst::Nop,
        })));
    }

    #[test]
    fn should_assemble_add() { should_assemble_reg_reg!(Inst::Add) }

    #[test]
    fn should_assemble_adc() { should_assemble_reg_reg!(Inst::Adc) }

    #[test]
    fn should_assemble_addi() { should_assemble_reg_imm!(Inst::Addi) }

    #[test]
    fn should_assemble_sub() { should_assemble_reg_reg!(Inst::Sub) }

    #[test]
    fn should_assemble_sbc() { should_assemble_reg_reg!(Inst::Sbc) }

    #[test]
    fn should_assemble_subi() { should_assemble_reg_imm!(Inst::Subi) }

    #[test]
    fn should_assemble_mulw() { should_assemble_areg_areg!(Inst::Mulw) }

    #[test]
    fn should_assemble_and() { should_assemble_reg_reg!(Inst::And) }

    #[test]
    fn should_assemble_or() { should_assemble_reg_reg!(Inst::Or) }

    #[test]
    fn should_assemble_xor() { should_assemble_reg_reg!(Inst::Xor) }

    #[test]
    fn should_assemble_lsl() { should_assemble_reg_reg!(Inst::Lsl) }

    #[test]
    fn should_assemble_lsr() { should_assemble_reg_reg!(Inst::Lsr) }

    #[test]
    fn should_assemble_asr() { should_assemble_reg_reg!(Inst::Asr) }

    #[test]
    fn should_assemble_not() { should_assemble_reg!(Inst::Not) }

    #[test]
    fn should_assemble_comp() { should_assemble_reg!(Inst::Comp) }

    #[test]
    fn should_assemble_inc() { should_assemble_reg!(Inst::Inc) }

    #[test]
    fn should_assemble_incw() { should_assemble_areg!(Inst::Incw) }

    #[test]
    fn should_assemble_dec() { should_assemble_reg!(Inst::Dec) }

    #[test]
    fn should_assemble_decw() { should_assemble_areg!(Inst::Decw) }

    #[test]
    fn should_assemble_mov() { should_assemble_reg_reg!(Inst::Mov) }

    #[test]
    fn should_assemble_ldi() { should_assemble_reg_imm!(Inst::Ldi) }

    #[test]
    fn should_assemble_ldsp() { should_assemble_areg!(Inst::Ldsp) }

    #[test]
    fn should_assemble_push() { should_assemble_reg!(Inst::Push) }

    #[test]
    fn should_assemble_pop() { should_assemble_reg!(Inst::Pop) }

    #[test]
    fn should_assemble_ijmp() { should_assemble_areg!(Inst::Ijmp) }

    #[test]
    fn should_assemble_icall() { should_assemble_areg!(Inst::Icall) }

    impl Arbitrary for Expr {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let choices = [
                Expr::reg(1, 1, *g.choose(&[
                    Reg::R0, Reg::R1, Reg::R2, Reg::R3,
                    Reg::R4, Reg::R5, Reg::R6, Reg::R7]).unwrap()),
                Expr::areg(1, 1, *g.choose(&[
                    AddrReg::A0, AddrReg::A1, AddrReg::A2, AddrReg::A3]).unwrap()),
                Expr::num(1, 1, g.next_u64() as i64),
            ];
            g.choose(&choices).unwrap().clone()
        }
    }
}
