//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::marker::MarkerTrait;

/// An immediate value that comes after an opcode. 
#[derive(Debug, PartialEq)]
pub struct Immediate(pub u8);

/// An address in SP-80 of 16-bits
#[derive(Debug, PartialEq)]
pub struct Addr(pub u16);

/// A relative address, i.e. a delta respect the current PC. 
#[derive(Debug, PartialEq)]
pub struct RelAddr(pub i16);

/// General purpose 8-bit Regs. 
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

/// 16-bits address Regs. 
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

pub trait Args : MarkerTrait {
    type Immediate;
    type Addr;
    type RelAddr;
    type Reg;
    type AddrReg;
}

pub struct RuntimeArgs;

impl Args for RuntimeArgs {
    type Immediate = Immediate;
    type Addr = Addr;
    type RelAddr = RelAddr;
    type Reg = Reg;
    type AddrReg = AddrReg;
}

pub struct AssemblyArgs;

impl Args for AssemblyArgs {
    type Immediate = String;
    type Addr = String;
    type RelAddr = String;
    type Reg = String;
    type AddrReg = String;
}

pub trait ArgMap<S: Args, D: Args, E> {

    fn map_immediate(&self, src: &S::Immediate) -> Result<D::Immediate, E>;
    fn map_addr(&self, src: &S::Addr) -> Result<D::Addr, E>;
    fn map_rel_addr(&self, src: &S::RelAddr) -> Result<D::RelAddr, E>;
    fn map_reg(&self, src: &S::Reg) -> Result<D::Reg, E>;
    fn map_addr_reg(&self, src: &S::AddrReg) -> Result<D::AddrReg, E>;
}
