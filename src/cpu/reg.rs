//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;

/// General purpose 8-bit Regs.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg { R0, R1, R2, R3, R4, R5, R6, R7 }

impl Reg {

    /// Encode a general purpose register into its binary representation
    pub fn encode(&self) -> u8 {
        match self {
            &Reg::R0 => 0x00,
            &Reg::R1 => 0x01,
            &Reg::R2 => 0x02,
            &Reg::R3 => 0x03,
            &Reg::R4 => 0x04,
            &Reg::R5 => 0x05,
            &Reg::R6 => 0x06,
            &Reg::R7 => 0x07,
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Reg::R0 => write!(fmt, "R0"),
            &Reg::R1 => write!(fmt, "R1"),
            &Reg::R2 => write!(fmt, "R2"),
            &Reg::R3 => write!(fmt, "R3"),
            &Reg::R4 => write!(fmt, "R4"),
            &Reg::R5 => write!(fmt, "R5"),
            &Reg::R6 => write!(fmt, "R6"),
            &Reg::R7 => write!(fmt, "R7"),
        }
    }
}

/// 16-bits address Regs.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AddrReg { A0, A1, A2, A3 }

impl AddrReg {

    /// Encode a general purpose register into its binary representation
    pub fn encode(&self) -> u8 {
        match self {
            &AddrReg::A0 => 0x00,
            &AddrReg::A1 => 0x01,
            &AddrReg::A2 => 0x02,
            &AddrReg::A3 => 0x03,
        }
    }
}

impl fmt::Display for AddrReg {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &AddrReg::A0 => write!(fmt, "A0"),
            &AddrReg::A1 => write!(fmt, "A1"),
            &AddrReg::A2 => write!(fmt, "A2"),
            &AddrReg::A3 => write!(fmt, "A3"),
        }
    }
}
