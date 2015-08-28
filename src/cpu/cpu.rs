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

use time::precise_time_ns;

use cpu::clock::*;
use cpu::exec::*;
use cpu::io::*;
use cpu::reg::*;
use inst::*;
use mem::*;

pub struct Cpu<'a, M: Memory> {
    clock: ClockFreq,
    io: Io<'a>,
    mem: Rc<RefCell<M>>,
    regs: Regs,
}

impl<'a, M: Memory> Cpu<'a, M> {

    /// Initialize the CPU with the given memory.
    pub fn with_memory(mem: M) -> Self { Cpu {
        clock: ClockFreq::default(),
        io: Io::new(),
        mem: Rc::new(RefCell::new(mem)),
        regs: Regs::new(),
    }}

    /// Returns the clock frequency for this CPU
    pub fn clock(&self) -> &ClockFreq { &self.clock }

    /// Returns the IO subsystem for this CPU
    pub fn io(&mut self) -> &Io { &mut self.io }

    /// Returns a shared adapter to the memory attached to this CPU.
    pub fn mem(&self) -> Mem<M> { Mem { mem: self.mem.clone() } }

    /// Returns the registers of this CPU.
    pub fn regs(&self) -> &Regs { &self.regs }

    /// Run one step, executing a single instruction
    /// It returns how much time such step took.
    pub fn step(&mut self) {
        let start = precise_time_ns();
        let inst = {
            let fetch = self.inst_fetch().bytes().map(|r| r.ok().unwrap());
            RuntimeInst::decode(fetch).unwrap_or(Inst::Nop)
        };
        let cycles = {
            let mut ctx = self.exec_ctx();
            exec(&inst, &mut ctx)
        };
        let end = start + self.clock.cycles(cycles).num_nanoseconds().unwrap() as u64;
        loop {
            let now = precise_time_ns();
            if end < now { break }
        }
    }

    fn exec_ctx<'b>(&'b mut self) -> Ctx<'b, 'a, M> { Ctx {
        mem: self.mem(),
        regs: &mut self.regs,
        io: &mut self.io,
    }}

    fn inst_fetch(&mut self) -> InstFetch<M> { InstFetch {
        mem: self.mem(),
        pc: self.regs.pc,
    }}
}

pub struct Ctx<'a, 'b: 'a, M: Memory> {
    mem: Mem<M>,
    regs: &'a mut Regs,
    io: &'a mut Io<'b>,
}

impl<'a, 'b: 'a, M: Memory> ExecCtx<'b> for Ctx<'a, 'b, M> {
    type Mem = Mem<M>;
    fn mem(&mut self) -> &mut Mem<M> { &mut self.mem }
    fn regs(&mut self) -> &mut Regs { self.regs }
    fn io(&mut self) -> &mut Io<'b> { self.io }
}

/// An adapter to the memory attached to the CPU
pub struct Mem<M: Memory> {
    mem: Rc<RefCell<M>>,
}

impl<M: Memory> Memory for Mem<M> {
    fn read(&self, addr: Addr) -> u8 { self.mem.borrow().read(addr) }
    fn write(&mut self, addr: Addr, byte: u8) { self.mem.borrow_mut().write(addr, byte) }
}

pub struct InstFetch<M: Memory> {
    mem: Mem<M>,
    pc: Addr,
}

impl<M: Memory> io::Read for InstFetch<M> {
    fn read(&mut self, buf: &mut[u8]) -> io::Result<usize> {
        let nread = self.mem.read_bytes(self.pc, buf);
        self.pc += nread as u16;
        Ok(nread)
    }
}

#[cfg(test)]
mod test {

    use time::Duration;

    use mem::{Memory, RamPage};

    use super::*;

    #[test]
    fn should_step_over() {
        let mut cpu = Cpu::with_memory(RamPage::new());
        // 8 nops
        cpu.mem().write_bytes(0x0000, &[0]);
        assert!(Duration::span(|| cpu.step()) > cpu.clock().cycles(4));
        assert_eq!(cpu.regs().pc, 1);
    }

    #[test]
    fn should_step_interpreting_invalid_opcode_as_nop() {
        let mut cpu = Cpu::with_memory(RamPage::new());
        // 8 nops
        cpu.mem().write_bytes(0x0000, &[0xe8]);
        assert!(Duration::span(|| cpu.step()) > cpu.clock().cycles(4));
        assert_eq!(cpu.regs().pc, 1);
    }
}
