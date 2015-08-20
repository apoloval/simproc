//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::cell::RefCell;
use std::io;
use std::io::Read;
use std::rc::Rc;

use time::Duration;

use cpu::clock::*;
use cpu::reg::*;
use inst::*;
use mem::*;

pub struct Cpu<M: Memory> {
    clock: ClockFreq,
    mem: Rc<RefCell<M>>,
    regs: Regs,
}

impl<M: Memory> Cpu<M> {

    /// Initialize the CPU with the given memory.
    pub fn with_memory(mem: M) -> Self { Cpu {
        mem: Rc::new(RefCell::new(mem)),
        regs: Regs::new(),
        clock: ClockFreq::default(),
    }}

    /// Returns a shared adapter to the memory attached to this CPU.
    pub fn mem(&self) -> Mem<M> { Mem { mem: self.mem.clone() } }

    /// Returns the registers of this CPU.
    pub fn regs(&self) -> &Regs { &self.regs }

    /// Run one step, executing a single instruction
    /// It returns how much time such step took.
    pub fn step(&mut self) -> Duration {
        let _inst = RuntimeInst::decode(self.inst_fetch().bytes().map(|r| r.ok().unwrap()));
        self.clock.cycles(4)
    }

    fn inst_fetch<'a>(&'a mut self) -> InstFetch<'a, M> { InstFetch {
        mem: self.mem(),
        regs: &mut self.regs
    }}
}

/// An adapter to the memory attached to the CPU
pub struct Mem<M: Memory> {
    mem: Rc<RefCell<M>>,
}

impl<M: Memory> Memory for Mem<M> {
    fn read(&self, addr: Addr) -> u8 { self.mem.borrow().read(addr) }
    fn write(&mut self, addr: Addr, byte: u8) { self.mem.borrow_mut().write(addr, byte) }
}

pub struct InstFetch<'a, M: Memory> {
    mem: Mem<M>,
    regs: &'a mut Regs,
}

impl<'a, M: Memory> io::Read for InstFetch<'a, M> {
    fn read(&mut self, buf: &mut[u8]) -> io::Result<usize> {
        let nread = self.mem.read_bytes(self.regs.pc, buf);
        self.regs.pc += nread as u16;
        Ok(nread)
    }
}

#[cfg(test)]
mod test {

    use mem::{Memory, RamPage};

    use super::*;

    #[test]
    fn should_step_over() {
        let mut cpu = Cpu::with_memory(RamPage::new());
        // 8 nops
        cpu.mem().write_bytes(0x0000, &[0, 0, 0, 0, 0, 0, 0, 0]);
        cpu.step();
        assert_eq!(cpu.regs().pc, 1);
    }
}
