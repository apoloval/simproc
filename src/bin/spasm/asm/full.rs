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

pub trait ExprAssembler {
    fn to_reg(&self, e: Expr) -> Result<Reg, FullAssembleError>;
    fn to_areg(&self, e: Expr) -> Result<AddrReg, FullAssembleError>;
    fn to_immediate(&self, e: Expr) -> Result<Immediate, FullAssembleError>;
    fn to_addr(&self, e: Expr) -> Result<Addr, FullAssembleError>;
}

pub fn full_assemble_inst(
    inst: PreAssembledInst,
    symbols: &SymbolTable) -> Result<RuntimeInst, FullAssembleError>
{
    let expr_asm = StdExprAssembler::from_symbols(symbols);
    match inst {
        Inst::Add(r1, r2) => Ok(Inst::Add(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Adc(r1, r2) => Ok(Inst::Adc(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Addi(r, l) => Ok(Inst::Addi(try!(expr_asm.to_reg(r)), try!(expr_asm.to_immediate(l)))),
        Inst::Sub(r1, r2) => Ok(Inst::Sub(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Sbc(r1, r2) => Ok(Inst::Sbc(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Subi(r, l) => Ok(Inst::Subi(try!(expr_asm.to_reg(r)), try!(expr_asm.to_immediate(l)))),
        Inst::Mulw(r1, r2) => Ok(Inst::Mulw(try!(expr_asm.to_areg(r1)), try!(expr_asm.to_areg(r2)))),
        Inst::And(r1, r2) => Ok(Inst::And(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Or(r1, r2) => Ok(Inst::Or(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Xor(r1, r2) => Ok(Inst::Xor(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Lsl(r1, r2) => Ok(Inst::Lsl(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Lsr(r1, r2) => Ok(Inst::Lsr(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Asr(r1, r2) => Ok(Inst::Asr(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Not(r) => Ok(Inst::Not(try!(expr_asm.to_reg(r)))),
        Inst::Comp(r) => Ok(Inst::Comp(try!(expr_asm.to_reg(r)))),
        Inst::Inc(r) => Ok(Inst::Inc(try!(expr_asm.to_reg(r)))),
        Inst::Incw(r) => Ok(Inst::Incw(try!(expr_asm.to_areg(r)))),
        Inst::Dec(r) => Ok(Inst::Dec(try!(expr_asm.to_reg(r)))),
        Inst::Decw(r) => Ok(Inst::Decw(try!(expr_asm.to_areg(r)))),

        Inst::Mov(r1, r2) => Ok(Inst::Mov(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Ld(r1, r2) => Ok(Inst::Ld(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_areg(r2)))),
        Inst::St(r1, r2) => Ok(Inst::St(try!(expr_asm.to_areg(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Ldd(r1, r2) => Ok(Inst::Ldd(try!(expr_asm.to_reg(r1)), try!(expr_asm.to_addr(r2)))),
        Inst::Std(r1, r2) => Ok(Inst::Std(try!(expr_asm.to_addr(r1)), try!(expr_asm.to_reg(r2)))),
        Inst::Ldi(r, l) => Ok(Inst::Ldi(try!(expr_asm.to_reg(r)), try!(expr_asm.to_immediate(l)))),
        Inst::Ldsp(r) => Ok(Inst::Ldsp(try!(expr_asm.to_areg(r)))),
        Inst::Push(r) => Ok(Inst::Push(try!(expr_asm.to_reg(r)))),
        Inst::Pop(r) => Ok(Inst::Pop(try!(expr_asm.to_reg(r)))),

        Inst::Ijmp(r) => Ok(Inst::Ijmp(try!(expr_asm.to_areg(r)))),
        Inst::Icall(r) => Ok(Inst::Icall(try!(expr_asm.to_areg(r)))),
        Inst::Ret => Ok(Inst::Ret),
        Inst::Reti => Ok(Inst::Reti),

        Inst::Nop => Ok(Inst::Nop),
        Inst::Halt => Ok(Inst::Halt),
        _ => Err(FullAssembleError::NotImplemented),
    }
}

pub struct StdExprAssembler<'a> {
    _symbols: &'a SymbolTable,
}

impl<'a> StdExprAssembler<'a> {
    fn from_symbols(symbols: &'a SymbolTable) -> Self {
        StdExprAssembler { _symbols: symbols }
    }
}

impl<'a> ExprAssembler for StdExprAssembler<'a> {

    fn to_reg(&self, e: Expr) -> Result<Reg, FullAssembleError> {
        match e {
            Expr::Reg(_, reg) => Ok(reg),
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "register name".to_string()
            })
        }
    }

    fn to_areg(&self, e: Expr) -> Result<AddrReg, FullAssembleError> {
        match e {
            Expr::AddrReg(_, reg) => Ok(reg),
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "address register name".to_string()
            })
        }
    }

    fn to_immediate(&self, e: Expr) -> Result<Immediate, FullAssembleError> {
        match e {
            Expr::Number(_, n) => Ok(Immediate(n as u8)),
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "immediate value".to_string()
            })
        }
    }

    fn to_addr(&self, e: Expr) -> Result<Addr, FullAssembleError> {
        match e {
            Expr::Number(_, n) => Ok(Addr(n as u16)),
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "memory address".to_string()
            })
        }
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

    #[test]
    fn should_asm_expr_to_reg() {
        let symbols = SymbolTable::new();
        let asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_reg(Expr::reg(1, 1, Reg::R0)),
            Ok(Reg::R0));
        assert_eq!(
            asm.to_reg(Expr::num(1, 1, 100)),
            Err(FullAssembleError::TypeMismatch {
                loc: loc!(1, 1, "100"),
                expected: "register name".to_string()
            }));
    }

    #[test]
    fn should_asm_expr_to_areg() {
        let symbols = SymbolTable::new();
        let asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_areg(Expr::areg(1, 1, AddrReg::A0)),
            Ok(AddrReg::A0));
        assert_eq!(
            asm.to_areg(Expr::num(1, 1, 100)),
            Err(FullAssembleError::TypeMismatch {
                loc: loc!(1, 1, "100"),
                expected: "address register name".to_string()
            }));
    }

    #[test]
    fn should_asm_expr_to_immediate() {
        let symbols = SymbolTable::new();
        let asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_immediate(Expr::num(1, 1, 100)),
            Ok(Immediate(100)));
        assert_eq!(
            asm.to_immediate(Expr::reg(1, 1, Reg::R0)),
            Err(FullAssembleError::TypeMismatch {
                loc: loc!(1, 1, "R0"),
                expected: "immediate value".to_string()
            }));
    }

    #[test]
    fn should_asm_expr_to_addr() {
        let symbols = SymbolTable::new();
        let asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_addr(Expr::num(1, 1, 100)),
            Ok(Addr(100)));
        assert_eq!(
            asm.to_addr(Expr::reg(1, 1, Reg::R0)),
            Err(FullAssembleError::TypeMismatch {
                loc: loc!(1, 1, "R0"),
                expected: "memory address".to_string()
            }));
    }

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

    macro_rules! should_assemble {
        ($i:expr) => ({
            let symbols = SymbolTable::new();
            let result = full_assemble_inst($i, &symbols);
            assert_eq!(result, Ok($i));
        });
        ($i:path, $t1:path) => ({
            fn assemble_with_operands(op1: Expr) -> TestResult {
                match op1 {
                    $t1(l1, r1) => {
                        let symbols = SymbolTable::new();
                        match full_assemble_inst($i($t1(l1, r1)), &symbols) {
                            Ok($i(_)) => TestResult::passed(),
                            e => TestResult::error(format!("unexpected result: {:?}", e)),
                        }
                    },
                    e1 => { should_assemble_type_mismatch!($i, e1) },
                }
            }
            quickcheck(assemble_with_operands as fn(Expr) -> TestResult);
        });
        ($i:path, $t1:path, $t2:path) => ({
            fn assemble_with_operands(op1: Expr, op2: Expr) -> TestResult {
                match (op1, op2) {
                    ($t1(l1, r1), $t2(l2, r2)) => {
                        let symbols = SymbolTable::new();
                        match full_assemble_inst($i($t1(l1, r1), $t2(l2, r2)), &symbols) {
                            Ok($i(_, _)) => TestResult::passed(),
                            e => TestResult::error(format!("unexpected result: {:?}", e)),
                        }
                    },
                    (e1, e2) => { should_assemble_type_mismatch!($i, e1, e2) },
                }
            }
            quickcheck(assemble_with_operands as fn(Expr, Expr) -> TestResult);
        });
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
    fn should_assemble_add() { should_assemble!(Inst::Add, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_adc() { should_assemble!(Inst::Adc, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_addi() { should_assemble!(Inst::Addi, Expr::Reg, Expr::Number) }

    #[test]
    fn should_assemble_sub() { should_assemble!(Inst::Sub, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_sbc() { should_assemble!(Inst::Sbc, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_subi() { should_assemble!(Inst::Subi, Expr::Reg, Expr::Number) }

    #[test]
    fn should_assemble_mulw() { should_assemble!(Inst::Mulw, Expr::AddrReg, Expr::AddrReg) }

    #[test]
    fn should_assemble_and() { should_assemble!(Inst::And, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_or() { should_assemble!(Inst::Or, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_xor() { should_assemble!(Inst::Xor, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_lsl() { should_assemble!(Inst::Lsl, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_lsr() { should_assemble!(Inst::Lsr, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_asr() { should_assemble!(Inst::Asr, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_not() { should_assemble!(Inst::Not, Expr::Reg) }

    #[test]
    fn should_assemble_comp() { should_assemble!(Inst::Comp, Expr::Reg) }

    #[test]
    fn should_assemble_inc() { should_assemble!(Inst::Inc, Expr::Reg) }

    #[test]
    fn should_assemble_incw() { should_assemble!(Inst::Incw, Expr::AddrReg) }

    #[test]
    fn should_assemble_dec() { should_assemble!(Inst::Dec, Expr::Reg) }

    #[test]
    fn should_assemble_decw() { should_assemble!(Inst::Decw, Expr::AddrReg) }

    #[test]
    fn should_assemble_mov() { should_assemble!(Inst::Mov, Expr::Reg, Expr::Reg) }

    #[test]
    fn should_assemble_ld() { should_assemble!(Inst::Ld, Expr::Reg, Expr::AddrReg) }

    #[test]
    fn should_assemble_st() { should_assemble!(Inst::St, Expr::AddrReg, Expr::Reg) }

    #[test]
    fn should_assemble_ldd() { should_assemble!(Inst::Ldd, Expr::Reg, Expr::Number) }

    #[test]
    fn should_assemble_std() { should_assemble!(Inst::Std, Expr::Number, Expr::Reg) }

    #[test]
    fn should_assemble_ldi() { should_assemble!(Inst::Ldi, Expr::Reg, Expr::Number) }

    #[test]
    fn should_assemble_ldsp() { should_assemble!(Inst::Ldsp, Expr::AddrReg) }

    #[test]
    fn should_assemble_push() { should_assemble!(Inst::Push, Expr::Reg) }

    #[test]
    fn should_assemble_pop() { should_assemble!(Inst::Pop, Expr::Reg) }

    #[test]
    fn should_assemble_ijmp() { should_assemble!(Inst::Ijmp, Expr::AddrReg) }

    #[test]
    fn should_assemble_icall() { should_assemble!(Inst::Icall, Expr::AddrReg) }

    #[test]
    fn should_assemble_ret() { should_assemble!(Inst::Ret) }

    #[test]
    fn should_assemble_reti() { should_assemble!(Inst::Reti) }

    #[test]
    fn should_assemble_nop() { should_assemble!(Inst::Nop) }

    #[test]
    fn should_assemble_halt() { should_assemble!(Inst::Halt) }

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
