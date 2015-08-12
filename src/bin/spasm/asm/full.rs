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
        Inst::And(r1, r2) => Ok(Inst::And(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Or(r1, r2) => Ok(Inst::Or(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Xor(r1, r2) => Ok(Inst::Xor(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Lsl(r1, r2) => Ok(Inst::Lsl(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Lsr(r1, r2) => Ok(Inst::Lsr(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Asr(r1, r2) => Ok(Inst::Asr(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Mov(r1, r2) => Ok(Inst::Mov(try!(to_reg(r1)), try!(to_reg(r2)))),
        Inst::Ldi(r, l) => Ok(Inst::Ldi(try!(to_reg(r)), try!(to_immediate(l, symbols)))),
        Inst::Nop => Ok(Inst::Nop),
        _ => Err(FullAssembleError::NotImplemented),
    }
}

fn to_reg(e: Expr) -> Result<Reg, FullAssembleError> {
    match e {
        Expr::Reg { loc: _, reg } => Ok(reg),
        e => Err(FullAssembleError::TypeMismatch {
            loc: e.loc().clone(),
            expected: "register name".to_string()
        })
    }
}

fn to_immediate(e: Expr, _symbols: &SymbolTable) -> Result<Immediate, FullAssembleError> {
    match e {
        Expr::Number { loc: _, num: n } => Ok(Immediate(n as u8)),
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
        ($i:expr, $e1:expr, $e2:expr) => ({
            let symbols = SymbolTable::new();
            match full_assemble_inst($i($e1, $e2), &symbols) {
                Err(FullAssembleError::TypeMismatch { loc: _, expected: _ }) =>
                    TestResult::passed(),
                r => TestResult::error(format!("unexpected result: {:?}", r)),
            }
        })
    }

    macro_rules! should_assemble_reg_reg {
        ($i:expr) => ({
            fn assembles_reg_reg(op1: Expr, op2: Expr) -> TestResult {
                match (op1, op2) {
                    (Expr::Reg { loc: l1, reg: r1 }, Expr::Reg { loc: l2, reg: r2 }) => {
                        let symbols = SymbolTable::new();
                        let expected = Ok($i(r1, r2));
                        let actual = full_assemble_inst(
                            $i(Expr::Reg { loc: l1, reg: r1 }, Expr::Reg { loc: l2, reg: r2 }),
                            &symbols);
                        TestResult::from_bool(actual == expected)
                    },
                    (e1, e2) => { should_assemble_type_mismatch!($i, e1, e2) },
                }
            }
            quickcheck(assembles_reg_reg as fn(Expr, Expr) -> TestResult);
        })
    }

    macro_rules! should_assemble_reg_imm {
        ($i:path) => ({
            fn assembles_reg_reg(op1: Expr, op2: Expr) -> TestResult {
                match (op1, op2) {
                    (Expr::Reg { loc: l1, reg: r }, Expr::Number { loc: l2, num: n }) => {
                        let symbols = SymbolTable::new();
                        let pre = $i(
                            Expr::Reg { loc: l1, reg: r },
                            Expr::Number { loc: l2, num: n });
                        match full_assemble_inst(pre, &symbols) {
                            Ok($i(rr, _)) =>
                                TestResult::from_bool(r == rr),
                            r => TestResult::error(format!("unexpected result: {:?}", r)),
                        }
                    },
                    (e1, e2) => { should_assemble_type_mismatch!($i, e1, e2) },
                }
            }
            quickcheck(assembles_reg_reg as fn(Expr, Expr) -> TestResult);
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
    fn should_assemble_mov() { should_assemble_reg_reg!(Inst::Mov) }

    #[test]
    fn should_assemble_ldi() { should_assemble_reg_imm!(Inst::Ldi) }

    impl Arbitrary for Expr {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let choices = [
                Expr::reg(1, 1, *g.choose(&[
                    Reg::R0, Reg::R1, Reg::R2, Reg::R3,
                    Reg::R4, Reg::R5, Reg::R6, Reg::R7]).unwrap()),
                Expr::num(1, 1, g.next_u64() as i64),
            ];
            g.choose(&choices).unwrap().clone()
        }
    }
}
