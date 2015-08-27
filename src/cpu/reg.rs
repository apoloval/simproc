//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use inst::Reg;
use mem::*;

pub struct StatusReg {
    pub carry: bool,
    pub zero: bool,
    pub neg: bool,
    pub overflow: bool,
    pub int: bool,
}

impl StatusReg {
    fn new() -> Self { StatusReg {
        carry: false,
        zero: false,
        neg: false,
        overflow: false,
        int: false,
    }}
}

pub struct Regs {
    pub pc: Addr,
    pub st: StatusReg,
    a0: Addr,
    a1: Addr,
    a2: Addr,
    a3: Addr,
}

impl Regs {
    pub fn new() -> Self {
        Regs { pc: 0, st: StatusReg::new(), a0: 0, a1: 0, a2: 0, a3: 0, }
    }

    pub fn a0(&self) -> Addr { self.a0 }
    pub fn a1(&self) -> Addr { self.a1 }
    pub fn a2(&self) -> Addr { self.a2 }
    pub fn a3(&self) -> Addr { self.a3 }

    pub fn set_a0(&mut self, n: Addr) { self.a0 = n }
    pub fn set_a1(&mut self, n: Addr) { self.a1 = n }
    pub fn set_a2(&mut self, n: Addr) { self.a2 = n }
    pub fn set_a3(&mut self, n: Addr) { self.a3 = n }

    pub fn r0(&self) -> u8 { self.a0 as u8 }
    pub fn r1(&self) -> u8 { (self.a0 >> 8) as u8 }
    pub fn r2(&self) -> u8 { self.a1 as u8 }
    pub fn r3(&self) -> u8 { (self.a1 >> 8) as u8 }
    pub fn r4(&self) -> u8 { self.a2 as u8 }
    pub fn r5(&self) -> u8 { (self.a2 >> 8) as u8 }
    pub fn r6(&self) -> u8 { self.a3 as u8 }
    pub fn r7(&self) -> u8 { (self.a3 >> 8) as u8 }

    pub fn reg(&self, reg: Reg) -> u8 {
        match reg {
            Reg::R0 => self.r0(),
            Reg::R1 => self.r1(),
            Reg::R2 => self.r2(),
            Reg::R3 => self.r3(),
            Reg::R4 => self.r4(),
            Reg::R5 => self.r5(),
            Reg::R6 => self.r6(),
            Reg::R7 => self.r7(),
        }
    }

    pub fn set_r0(&mut self, n: u8) { self.a0 = (self.a0 & 0xff00) + (n as u16) }
    pub fn set_r1(&mut self, n: u8) { self.a0 = (self.a0 & 0x00ff) + ((n as u16) << 8) }
    pub fn set_r2(&mut self, n: u8) { self.a1 = (self.a1 & 0xff00) + (n as u16) }
    pub fn set_r3(&mut self, n: u8) { self.a1 = (self.a1 & 0x00ff) + ((n as u16) << 8) }
    pub fn set_r4(&mut self, n: u8) { self.a2 = (self.a2 & 0xff00) + (n as u16) }
    pub fn set_r5(&mut self, n: u8) { self.a2 = (self.a2 & 0x00ff) + ((n as u16) << 8) }
    pub fn set_r6(&mut self, n: u8) { self.a3 = (self.a3 & 0xff00) + (n as u16) }
    pub fn set_r7(&mut self, n: u8) { self.a3 = (self.a3 & 0x00ff) + ((n as u16) << 8) }

    pub fn set_reg(&mut self, reg: Reg, n: u8) {
        match reg {
            Reg::R0 => self.set_r0(n),
            Reg::R1 => self.set_r1(n),
            Reg::R2 => self.set_r2(n),
            Reg::R3 => self.set_r3(n),
            Reg::R4 => self.set_r4(n),
            Reg::R5 => self.set_r5(n),
            Reg::R6 => self.set_r6(n),
            Reg::R7 => self.set_r7(n),
        }
    }
}

#[cfg(test)]
mod test {

    use inst::Reg;

    use super::*;

    #[test]
    fn should_bind_dataregs_to_aregs() {
        let mut regs = Regs::new();
        regs.set_a0(0x0102);
        assert_eq!(regs.r0(), 0x02);
        assert_eq!(regs.r1(), 0x01);

        regs.set_a1(0x0102);
        assert_eq!(regs.r2(), 0x02);
        assert_eq!(regs.r3(), 0x01);

        regs.set_a2(0x0102);
        assert_eq!(regs.r4(), 0x02);
        assert_eq!(regs.r5(), 0x01);

        regs.set_a3(0x0102);
        assert_eq!(regs.r6(), 0x02);
        assert_eq!(regs.r7(), 0x01);
    }

    #[test]
    fn should_bind_aregs_to_dataregs() {
        let mut regs = Regs::new();
        regs.set_a0(0x0102);
        regs.set_r0(0xff);
        assert_eq!(regs.a0(), 0x01ff);
        regs.set_r1(0xaa);
        assert_eq!(regs.a0(), 0xaaff);

        regs.set_a1(0x0102);
        regs.set_r2(0xff);
        assert_eq!(regs.a1(), 0x01ff);
        regs.set_r3(0xaa);
        assert_eq!(regs.a1(), 0xaaff);

        regs.set_a2(0x0102);
        regs.set_r4(0xff);
        assert_eq!(regs.a2(), 0x01ff);
        regs.set_r5(0xaa);
        assert_eq!(regs.a2(), 0xaaff);

        regs.set_a3(0x0102);
        regs.set_r6(0xff);
        assert_eq!(regs.a3(), 0x01ff);
        regs.set_r7(0xaa);
        assert_eq!(regs.a3(), 0xaaff);
    }

    #[test]
    fn should_get_reg() {
        let mut regs = Regs::new();
        regs.set_r0(0x10);
        assert_eq!(regs.reg(Reg::R0), 0x10);

        regs.set_r1(0x11);
        assert_eq!(regs.reg(Reg::R1), 0x11);

        regs.set_r2(0x12);
        assert_eq!(regs.reg(Reg::R2), 0x12);

        regs.set_r3(0x13);
        assert_eq!(regs.reg(Reg::R3), 0x13);

        regs.set_r4(0x14);
        assert_eq!(regs.reg(Reg::R4), 0x14);

        regs.set_r5(0x15);
        assert_eq!(regs.reg(Reg::R5), 0x15);

        regs.set_r6(0x16);
        assert_eq!(regs.reg(Reg::R6), 0x16);

        regs.set_r7(0x17);
        assert_eq!(regs.reg(Reg::R7), 0x17);
    }

    #[test]
    fn should_set_reg() {
        let mut regs = Regs::new();
        regs.set_reg(Reg::R0, 0x10);
        assert_eq!(regs.r0(), 0x10);

        regs.set_reg(Reg::R1, 0x11);
        assert_eq!(regs.r1(), 0x11);

        regs.set_reg(Reg::R2, 0x12);
        assert_eq!(regs.r2(), 0x12);

        regs.set_reg(Reg::R3, 0x13);
        assert_eq!(regs.r3(), 0x13);

        regs.set_reg(Reg::R4, 0x14);
        assert_eq!(regs.r4(), 0x14);

        regs.set_reg(Reg::R5, 0x15);
        assert_eq!(regs.r5(), 0x15);

        regs.set_reg(Reg::R6, 0x16);
        assert_eq!(regs.r6(), 0x16);

        regs.set_reg(Reg::R7, 0x17);
        assert_eq!(regs.r7(), 0x17);
    }
}
