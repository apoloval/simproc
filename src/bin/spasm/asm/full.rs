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

#[derive(Clone, Debug, PartialEq)]
pub enum FullAssembleError {
    // Pre(PreAssembleError),
    TypeMismatch { loc: TextLoc, expected: String },
    Undefined { loc: TextLoc, symbol: String },
}

pub type FullAssemblerInput = PreAssemblerOutput;
pub type FullAssemblerOutput = Result<FullAssembled, FullAssembleError>;

pub struct FullAssembler<'a, I: Iterator<Item=FullAssemblerInput>> {
    input: I,
    symbols: &'a SymbolTable,
    inst_asm: StdInstAssembler<'a>,
}

impl<'a, I: Iterator<Item=FullAssemblerInput>> FullAssembler<'a, I> {

    pub fn from<P>(pre: P, symbols: &'a SymbolTable) -> Self
        where P: IntoIterator<Item=FullAssemblerInput, IntoIter=I>
    {
        FullAssembler {
            input: pre.into_iter(),
            symbols: symbols,
            inst_asm: StdInstAssembler::from_expr_asm(
                StdExprAssembler::from_symbols(symbols)),
        }
    }
}

impl<'a, I: Iterator<Item=FullAssemblerInput>> Iterator for FullAssembler<'a, I> {

    type Item = FullAssemblerOutput;

    fn next(&mut self) -> Option<FullAssemblerOutput> {
        match self.input.next() {
            Some(Ok(PreAssembled::Inst { loc, base_addr, inst, .. })) => {
                Some(self.inst_asm.assemble(inst, base_addr).map(|i| FullAssembled::Inst {
                    loc: loc, base_addr: base_addr, inst: i,
                }))
            },
            _ => None,
        }
    }
}

pub trait ExprAssembler {
    fn to_reg(&mut self, e: Expr) -> Result<Reg, FullAssembleError>;
    fn to_areg(&mut self, e: Expr) -> Result<AddrReg, FullAssembleError>;
    fn to_immediate(&mut self, e: Expr) -> Result<Immediate, FullAssembleError>;
    fn to_addr(&mut self, e: Expr) -> Result<Addr, FullAssembleError>;
    fn to_raddr(&mut self, e: Expr, base: Addr) -> Result<RelAddr, FullAssembleError>;
}

pub struct InstAssembler<E: ExprAssembler> {
    expr_asm: E,
}

pub type StdInstAssembler<'a> = InstAssembler<StdExprAssembler<'a>>;

impl<E: ExprAssembler> InstAssembler<E> {

    pub fn from_expr_asm(expr_asm: E) -> Self {
        InstAssembler { expr_asm: expr_asm }
    }

    pub fn assemble(
        &mut self,
        inst: PreAssembledInst, base: Addr) -> Result<RuntimeInst, FullAssembleError>
    {
        match inst {
            Inst::Add(r1, r2) => Ok(Inst::Add(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Adc(r1, r2) => Ok(Inst::Adc(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Addi(r, l) => Ok(Inst::Addi(try!(self.expr_asm.to_reg(r)), try!(self.expr_asm.to_immediate(l)))),
            Inst::Sub(r1, r2) => Ok(Inst::Sub(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Sbc(r1, r2) => Ok(Inst::Sbc(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Subi(r, l) => Ok(Inst::Subi(try!(self.expr_asm.to_reg(r)), try!(self.expr_asm.to_immediate(l)))),
            Inst::Mulw(r1, r2) => Ok(Inst::Mulw(try!(self.expr_asm.to_areg(r1)), try!(self.expr_asm.to_areg(r2)))),
            Inst::And(r1, r2) => Ok(Inst::And(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Or(r1, r2) => Ok(Inst::Or(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Xor(r1, r2) => Ok(Inst::Xor(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Lsl(r1, r2) => Ok(Inst::Lsl(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Lsr(r1, r2) => Ok(Inst::Lsr(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Asr(r1, r2) => Ok(Inst::Asr(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Not(r) => Ok(Inst::Not(try!(self.expr_asm.to_reg(r)))),
            Inst::Comp(r) => Ok(Inst::Comp(try!(self.expr_asm.to_reg(r)))),
            Inst::Inc(r) => Ok(Inst::Inc(try!(self.expr_asm.to_reg(r)))),
            Inst::Incw(r) => Ok(Inst::Incw(try!(self.expr_asm.to_areg(r)))),
            Inst::Dec(r) => Ok(Inst::Dec(try!(self.expr_asm.to_reg(r)))),
            Inst::Decw(r) => Ok(Inst::Decw(try!(self.expr_asm.to_areg(r)))),

            Inst::Mov(r1, r2) => Ok(Inst::Mov(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Ld(r1, r2) => Ok(Inst::Ld(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_areg(r2)))),
            Inst::St(r1, r2) => Ok(Inst::St(try!(self.expr_asm.to_areg(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Ldd(r1, r2) => Ok(Inst::Ldd(try!(self.expr_asm.to_reg(r1)), try!(self.expr_asm.to_addr(r2)))),
            Inst::Std(r1, r2) => Ok(Inst::Std(try!(self.expr_asm.to_addr(r1)), try!(self.expr_asm.to_reg(r2)))),
            Inst::Ldi(r, l) => Ok(Inst::Ldi(try!(self.expr_asm.to_reg(r)), try!(self.expr_asm.to_immediate(l)))),
            Inst::Ldsp(r) => Ok(Inst::Ldsp(try!(self.expr_asm.to_areg(r)))),
            Inst::Push(r) => Ok(Inst::Push(try!(self.expr_asm.to_reg(r)))),
            Inst::Pop(r) => Ok(Inst::Pop(try!(self.expr_asm.to_reg(r)))),

            Inst::Je(a) => Ok(Inst::Je(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jne(a) => Ok(Inst::Jne(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jl(a) => Ok(Inst::Jl(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jge(a) => Ok(Inst::Jge(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jcc(a) => Ok(Inst::Jcc(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jcs(a) => Ok(Inst::Jcs(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jvc(a) => Ok(Inst::Jvc(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jvs(a) => Ok(Inst::Jvs(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Jmp(a) => Ok(Inst::Jmp(try!(self.expr_asm.to_addr(a)))),
            Inst::Rjmp(a) => Ok(Inst::Rjmp(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Ijmp(r) => Ok(Inst::Ijmp(try!(self.expr_asm.to_areg(r)))),
            Inst::Call(a) => Ok(Inst::Call(try!(self.expr_asm.to_addr(a)))),
            Inst::Rcall(a) => Ok(Inst::Rcall(try!(self.expr_asm.to_raddr(a, base)))),
            Inst::Icall(r) => Ok(Inst::Icall(try!(self.expr_asm.to_areg(r)))),
            Inst::Ret => Ok(Inst::Ret),
            Inst::Reti => Ok(Inst::Reti),

            Inst::Nop => Ok(Inst::Nop),
            Inst::Halt => Ok(Inst::Halt),
        }
    }
}

pub struct StdExprAssembler<'a> {
    symbols: &'a SymbolTable,
}

impl<'a> StdExprAssembler<'a> {
    fn from_symbols(symbols: &'a SymbolTable) -> Self {
        StdExprAssembler { symbols: symbols }
    }
}

impl<'a> ExprAssembler for StdExprAssembler<'a> {

    fn to_reg(&mut self, e: Expr) -> Result<Reg, FullAssembleError> {
        match e {
            Expr::Reg(_, reg) => Ok(reg),
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "register name".to_string()
            })
        }
    }

    fn to_areg(&mut self, e: Expr) -> Result<AddrReg, FullAssembleError> {
        match e {
            Expr::AddrReg(_, reg) => Ok(reg),
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "address register name".to_string()
            })
        }
    }

    fn to_immediate(&mut self, e: Expr) -> Result<Immediate, FullAssembleError> {
        match e {
            Expr::Number(_, n) => Ok(Immediate(n as u8)),
            Expr::Ident(loc, id) => {
                match self.symbols.get(&id) {
                    Some(n) => Ok(Immediate(*n as u8)),
                    _ => Err(FullAssembleError::Undefined { loc: loc, symbol: id }),
                }
            },
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "immediate value".to_string()
            })
        }
    }

    fn to_addr(&mut self, e: Expr) -> Result<Addr, FullAssembleError> {
        match e {
            Expr::Number(_, n) => Ok(Addr(n as u16)),
            Expr::Ident(loc, id) => {
                match self.symbols.get(&id) {
                    Some(n) => Ok(Addr(*n as u16)),
                    _ => Err(FullAssembleError::Undefined { loc: loc, symbol: id }),
                }
            },
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "memory address".to_string()
            })
        }
    }

    fn to_raddr(&mut self, e: Expr, base: Addr) -> Result<RelAddr, FullAssembleError> {
        let Addr(b) = base;
        match e {
            Expr::Number(_, n) => Ok(RelAddr((n as i16) - (b as i16))),
            Expr::Ident(loc, id) => {
                match self.symbols.get(&id) {
                    Some(n) => Ok(RelAddr((*n as i16) - (b as i16))),
                    _ => Err(FullAssembleError::Undefined { loc: loc, symbol: id }),
                }
            },
            e => Err(FullAssembleError::TypeMismatch {
                loc: e.loc().clone(),
                expected: "memory address".to_string()
            })
        }
    }
}

#[cfg(test)]
mod test {

    use std::collections::VecDeque;

    use simproc::inst::*;

    use asm::lexer::TextLoc;
    use asm::new_parser::Expr;
    use asm::pre::*;

    use super::*;

    #[test]
    fn should_asm_expr_to_reg() {
        let symbols = SymbolTable::new();
        let mut asm = StdExprAssembler::from_symbols(&symbols);
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
        let mut asm = StdExprAssembler::from_symbols(&symbols);
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
        let mut symbols = SymbolTable::new();
        symbols.insert("foobar".to_string(), 100);
        let mut asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_immediate(Expr::num(1, 1, 100)),
            Ok(Immediate(100)));
        assert_eq!(
            asm.to_immediate(Expr::id(1, 1, "foobar")),
            Ok(Immediate(100)));
        assert_eq!(
            asm.to_immediate(Expr::id(1, 1, "undefined")),
            Err(FullAssembleError::Undefined {
                loc: loc!(1, 1, "undefined"),
                symbol: "undefined".to_string()
            }));
        assert_eq!(
            asm.to_immediate(Expr::reg(1, 1, Reg::R0)),
            Err(FullAssembleError::TypeMismatch {
                loc: loc!(1, 1, "R0"),
                expected: "immediate value".to_string()
            }));
    }

    #[test]
    fn should_asm_expr_to_addr() {
        let mut symbols = SymbolTable::new();
        symbols.insert("foobar".to_string(), 0x1000);
        let mut asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_addr(Expr::num(1, 1, 100)),
            Ok(Addr(100)));
        assert_eq!(
            asm.to_addr(Expr::id(1, 1, "foobar")),
            Ok(Addr(0x1000)));
        assert_eq!(
            asm.to_addr(Expr::id(1, 1, "undefined")),
            Err(FullAssembleError::Undefined {
                loc: loc!(1, 1, "undefined"),
                symbol: "undefined".to_string()
            }));
        assert_eq!(
            asm.to_addr(Expr::reg(1, 1, Reg::R0)),
            Err(FullAssembleError::TypeMismatch {
                loc: loc!(1, 1, "R0"),
                expected: "memory address".to_string()
            }));
    }

    #[test]
    fn should_asm_expr_to_raddr() {
        let mut symbols = SymbolTable::new();
        symbols.insert("foobar".to_string(), 0x1000);
        let mut asm = StdExprAssembler::from_symbols(&symbols);
        assert_eq!(
            asm.to_raddr(Expr::num(1, 1, 100), Addr(75)),
            Ok(RelAddr(25)));
        assert_eq!(
            asm.to_raddr(Expr::id(1, 1, "foobar"), Addr(0xf00)),
            Ok(RelAddr(0x100)));
        assert_eq!(
            asm.to_raddr(Expr::id(1, 1, "undefined"), Addr(75)),
            Err(FullAssembleError::Undefined {
                loc: loc!(1, 1, "undefined"),
                symbol: "undefined".to_string()
            }));
        assert_eq!(
            asm.to_raddr(Expr::reg(1, 1, Reg::R0), Addr(75)),
            Err(FullAssembleError::TypeMismatch {
                loc: loc!(1, 1, "R0"),
                expected: "memory address".to_string()
            }));
    }

    #[test]
    fn should_asm_add() { should_asm_inst_reg_reg(Inst::Add, Inst::Add); }

    #[test]
    fn should_asm_adc() { should_asm_inst_reg_reg(Inst::Adc, Inst::Adc); }

    #[test]
    fn should_asm_addi() { should_asm_inst_reg_imm(Inst::Addi, Inst::Addi); }

    #[test]
    fn should_asm_sub() { should_asm_inst_reg_reg(Inst::Sub, Inst::Sub); }

    #[test]
    fn should_asm_sbc() { should_asm_inst_reg_reg(Inst::Sbc, Inst::Sbc); }

    #[test]
    fn should_asm_subi() { should_asm_inst_reg_imm(Inst::Subi, Inst::Subi); }

    #[test]
    fn should_asm_mulw() { should_asm_inst_areg_areg(Inst::Mulw, Inst::Mulw); }

    #[test]
    fn should_asm_and() { should_asm_inst_reg_reg(Inst::And, Inst::And); }

    #[test]
    fn should_asm_or() { should_asm_inst_reg_reg(Inst::Or, Inst::Or); }

    #[test]
    fn should_asm_xor() { should_asm_inst_reg_reg(Inst::Xor, Inst::Xor); }

    #[test]
    fn should_asm_lsl() { should_asm_inst_reg_reg(Inst::Lsl, Inst::Lsl); }

    #[test]
    fn should_asm_lsr() { should_asm_inst_reg_reg(Inst::Lsr, Inst::Lsr); }

    #[test]
    fn should_asm_asr() { should_asm_inst_reg_reg(Inst::Asr, Inst::Asr); }

    #[test]
    fn should_asm_not() { should_asm_inst_reg(Inst::Not, Inst::Not); }

    #[test]
    fn should_asm_comp() { should_asm_inst_reg(Inst::Comp, Inst::Comp); }

    #[test]
    fn should_asm_inc() { should_asm_inst_reg(Inst::Inc, Inst::Inc); }

    #[test]
    fn should_asm_incw() { should_asm_inst_areg(Inst::Incw, Inst::Incw); }

    #[test]
    fn should_asm_dec() { should_asm_inst_reg(Inst::Dec, Inst::Dec); }

    #[test]
    fn should_asm_decw() { should_asm_inst_areg(Inst::Decw, Inst::Decw); }

    #[test]
    fn should_asm_mov() { should_asm_inst_reg_reg(Inst::Mov, Inst::Mov); }

    #[test]
    fn should_asm_ld() { should_asm_inst_reg_areg(Inst::Ld, Inst::Ld); }

    #[test]
    fn should_asm_st() { should_asm_inst_areg_reg(Inst::St, Inst::St); }

    #[test]
    fn should_asm_ldd() { should_asm_inst_reg_addr(Inst::Ldd, Inst::Ldd); }

    #[test]
    fn should_asm_std() { should_asm_inst_addr_reg(Inst::Std, Inst::Std); }

    #[test]
    fn should_asm_ldi() { should_asm_inst_reg_imm(Inst::Ldi, Inst::Ldi); }

    #[test]
    fn should_asm_ldsp() { should_asm_inst_areg(Inst::Ldsp, Inst::Ldsp); }

    #[test]
    fn should_asm_push() { should_asm_inst_reg(Inst::Push, Inst::Push); }

    #[test]
    fn should_asm_pop() { should_asm_inst_reg(Inst::Pop, Inst::Pop); }

    #[test]
    fn should_asm_je() { should_asm_inst_raddr(Inst::Je, Inst::Je); }

    #[test]
    fn should_asm_jne() { should_asm_inst_raddr(Inst::Jne, Inst::Jne); }

    #[test]
    fn should_asm_jl() { should_asm_inst_raddr(Inst::Jl, Inst::Jl); }

    #[test]
    fn should_asm_jge() { should_asm_inst_raddr(Inst::Jge, Inst::Jge); }

    #[test]
    fn should_asm_jcc() { should_asm_inst_raddr(Inst::Jcc, Inst::Jcc); }

    #[test]
    fn should_asm_jcs() { should_asm_inst_raddr(Inst::Jcs, Inst::Jcs); }

    #[test]
    fn should_asm_jvc() { should_asm_inst_raddr(Inst::Jvc, Inst::Jvc); }

    #[test]
    fn should_asm_jvs() { should_asm_inst_raddr(Inst::Jvs, Inst::Jvs); }

    #[test]
    fn should_asm_jmp() { should_asm_inst_addr(Inst::Jmp, Inst::Jmp); }

    #[test]
    fn should_asm_rjmp() { should_asm_inst_raddr(Inst::Rjmp, Inst::Rjmp); }

    #[test]
    fn should_asm_ijmp() { should_asm_inst_areg(Inst::Ijmp, Inst::Ijmp); }

    #[test]
    fn should_asm_call() { should_asm_inst_addr(Inst::Call, Inst::Call); }

    #[test]
    fn should_asm_rcall() { should_asm_inst_raddr(Inst::Rcall, Inst::Rcall); }

    #[test]
    fn should_asm_icall() { should_asm_inst_areg(Inst::Icall, Inst::Icall); }

    #[test]
    fn should_asm_ret() { should_asm_nullary_inst(Inst::Ret, Inst::Ret); }

    #[test]
    fn should_asm_reti() { should_asm_nullary_inst(Inst::Reti, Inst::Reti); }

    #[test]
    fn should_asm_nop() { should_asm_nullary_inst(Inst::Nop, Inst::Nop); }

    #[test]
    fn should_asm_halt() { should_asm_nullary_inst(Inst::Halt, Inst::Halt); }

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

    struct MockExprAssembler {
        next_reg: VecDeque<Result<Reg, FullAssembleError>>,
        next_areg: VecDeque<Result<AddrReg, FullAssembleError>>,
        next_imm: VecDeque<Result<Immediate, FullAssembleError>>,
        next_addr: VecDeque<Result<Addr, FullAssembleError>>,
        next_raddr: VecDeque<Result<RelAddr, FullAssembleError>>,
    }

    impl MockExprAssembler {
        fn new() -> Self {
            MockExprAssembler {
                next_reg: VecDeque::new(),
                next_areg: VecDeque::new(),
                next_imm: VecDeque::new(),
                next_addr: VecDeque::new(),
                next_raddr: VecDeque::new(),
            }
        }

        fn with_reg(&mut self, reg: Result<Reg, FullAssembleError>) {
            self.next_reg.push_back(reg);
        }

        fn with_areg(&mut self, areg: Result<AddrReg, FullAssembleError>) {
            self.next_areg.push_back(areg);
        }

        fn with_imm(&mut self, imm: Result<Immediate, FullAssembleError>) {
            self.next_imm.push_back(imm);
        }

        fn with_addr(&mut self, addr: Result<Addr, FullAssembleError>) {
            self.next_addr.push_back(addr);
        }

        fn with_raddr(&mut self, raddr: Result<RelAddr, FullAssembleError>) {
            self.next_raddr.push_back(raddr);
        }
    }

    impl ExprAssembler for MockExprAssembler {
        fn to_reg(&mut self, _e: Expr) -> Result<Reg, FullAssembleError> {
            self.next_reg.pop_front().unwrap()
        }
        fn to_areg(&mut self, _e: Expr) -> Result<AddrReg, FullAssembleError> {
            self.next_areg.pop_front().unwrap()
        }
        fn to_immediate(&mut self, _e: Expr) -> Result<Immediate, FullAssembleError> {
            self.next_imm.pop_front().unwrap()
        }
        fn to_addr(&mut self, _e: Expr) -> Result<Addr, FullAssembleError> {
            self.next_addr.pop_front().unwrap()
        }
        fn to_raddr(&mut self, _e: Expr, _base: Addr) -> Result<RelAddr, FullAssembleError> {
            self.next_raddr.pop_front().unwrap()
        }
    }

    fn should_asm_nullary_inst(pre: PreAssembledInst, full: RuntimeInst) {
        let expr = MockExprAssembler::new();
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(asm.assemble(pre, Addr(1000)), Ok(full));
    }

    fn should_asm_unary_inst<I1, I2, O1, M1>(pre: I1, full: I2, o1: O1, m1: M1) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(O1) -> RuntimeInst,
        O1: Clone,
        M1: Fn(&mut MockExprAssembler, Result<O1, FullAssembleError>),
    {
        let err = FullAssembleError::TypeMismatch {
            loc: loc!(1, 1, "1"),
            expected: "foobar".to_string(),
        };
        let mut expr = MockExprAssembler::new();
        m1(&mut expr, Ok(o1.clone()));
        m1(&mut expr, Err(err.clone()));
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(
            asm.assemble(pre(Expr::num(1, 1, 1)), Addr(1000)),
            Ok(full(o1)));
        assert_eq!(
            asm.assemble(pre(Expr::num(1, 1, 1)), Addr(1000)),
            Err(err.clone()));
    }

    fn should_asm_binary_inst<I1, I2, O1, O2, M1, M2>(pre: I1, full: I2, o1: O1, o2: O2, m1: M1, m2: M2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(O1, O2) -> RuntimeInst,
        O1: Clone,
        O2: Clone,
        M1: Fn(&mut MockExprAssembler, Result<O1, FullAssembleError>),
        M2: Fn(&mut MockExprAssembler, Result<O2, FullAssembleError>),
    {
        let err = FullAssembleError::TypeMismatch {
            loc: loc!(1, 1, "1"),
            expected: "foobar".to_string(),
        };
        let mut expr = MockExprAssembler::new();
        m1(&mut expr, Ok(o1.clone()));
        m2(&mut expr, Ok(o2.clone()));
        m1(&mut expr, Ok(o1.clone()));
        m2(&mut expr, Err(err.clone()));
        m1(&mut expr, Err(err.clone()));
        m2(&mut expr, Ok(o2.clone()));
        let mut asm = InstAssembler::from_expr_asm(expr);
        assert_eq!(
            asm.assemble(pre(Expr::num(1, 1, 1), Expr::num(1, 1, 1)), Addr(1000)),
            Ok(full(o1, o2)));
        assert_eq!(
            asm.assemble(pre(Expr::num(1, 1, 1), Expr::num(1, 1, 1)), Addr(1000)),
            Err(err.clone()));
        assert_eq!(
            asm.assemble(pre(Expr::num(1, 1, 1), Expr::num(1, 1, 1)), Addr(1000)),
            Err(err.clone()));
    }

    fn should_asm_inst_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(Reg) -> RuntimeInst
    {
        should_asm_unary_inst(
            pre, full,
            Reg::R0,
            MockExprAssembler::with_reg);
    }

    fn should_asm_inst_areg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(AddrReg) -> RuntimeInst
    {
        should_asm_unary_inst(
            pre, full,
            AddrReg::A0,
            MockExprAssembler::with_areg);
    }

    fn should_asm_inst_addr<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(Addr) -> RuntimeInst
    {
        should_asm_unary_inst(
            pre, full,
            Addr(100),
            MockExprAssembler::with_addr);
    }

    fn should_asm_inst_raddr<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr) -> PreAssembledInst,
        I2: Fn(RelAddr) -> RuntimeInst
    {
        should_asm_unary_inst(
            pre, full,
            RelAddr(100),
            MockExprAssembler::with_raddr);
    }

    fn should_asm_inst_reg_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, Reg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, Reg::R1,
            MockExprAssembler::with_reg, MockExprAssembler::with_reg);
    }

    fn should_asm_inst_reg_imm<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, Immediate) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, Immediate(100),
            MockExprAssembler::with_reg, MockExprAssembler::with_imm);
    }

    fn should_asm_inst_areg_areg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(AddrReg, AddrReg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            AddrReg::A0, AddrReg::A1,
            MockExprAssembler::with_areg, MockExprAssembler::with_areg);
    }

    fn should_asm_inst_reg_areg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, AddrReg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, AddrReg::A0,
            MockExprAssembler::with_reg, MockExprAssembler::with_areg);
    }

    fn should_asm_inst_areg_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(AddrReg, Reg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            AddrReg::A0, Reg::R0,
            MockExprAssembler::with_areg, MockExprAssembler::with_reg);
    }

    fn should_asm_inst_reg_addr<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Reg, Addr) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Reg::R0, Addr(100),
            MockExprAssembler::with_reg, MockExprAssembler::with_addr);
    }

    fn should_asm_inst_addr_reg<I1, I2>(pre: I1, full: I2) where
        I1: Fn(Expr, Expr) -> PreAssembledInst,
        I2: Fn(Addr, Reg) -> RuntimeInst
    {
        should_asm_binary_inst(
            pre, full,
            Addr(100), Reg::R0,
            MockExprAssembler::with_addr, MockExprAssembler::with_reg);
    }
}
