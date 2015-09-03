//
// SimProc Machine
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::io::{stdin, Read, Write};
use std::sync::mpsc::{channel, Receiver};
use std::thread;

use simproc::cpu::{Cpu, IoDevice};
use simproc::mem::{MemBank, RamPage, RomPage};

type Memory = MemBank<RomPage, RamPage, RamPage, RamPage>;

pub struct Machine<'a> {
    cpu: Cpu<'a, Memory>,
}

struct Console<W: Write> {
    output: W,
    input: Receiver<u8>,
}

impl<W: Write> Console<W> {
    pub fn with_output(output: W) -> Self {
        let (tx, rx) = channel();
        thread::spawn(move|| {
            for byte in stdin().bytes() {
                if let Ok(b) = byte {
                    tx.send(b).unwrap();
                }
            }
        });
        Console { output: output, input: rx }
    }
}

impl<W: Write> IoDevice for Console<W> {
    fn read(&mut self) -> u8 {
        match self.input.try_recv() {
            Ok(byte) => byte,
            _ => 0,
        }
    }
    fn write(&mut self, val: u8) {
        let buf = [ val ];
        self.output.write(&buf).unwrap();
        self.output.flush().unwrap();
    }
}

impl<'a> Machine<'a> {

    pub fn with_rom<I: IntoIterator<Item=u8>>(rom: I) -> Self {
        Machine {
            cpu: Cpu::with_memory(MemBank::with_pages(
                RomPage::with_content(rom),
                RamPage::new(),
                RamPage::new(),
                RamPage::new())),
        }
    }

    pub fn attach_console<W: Write + 'a>(&mut self, output: W) {
        let console = Console::with_output(output);
        self.cpu.io().attach(0x10, console);
    }

    pub fn run_until_halt(&mut self) {
        loop {
            let pc_before = self.cpu.regs().pc;
            self.cpu.step();
            let pc_after = self.cpu.regs().pc;
            if pc_before == pc_after {
                return;
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn should_exec() {
        let rom = vec![0, 0, 0, 0, 0x3c];
        let mut m = Machine::with_rom(rom);
        m.run_until_halt();
    }

    #[test]
    fn should_output() {
        let rom = vec![
            0xD8, 0x48, 0xD9, 0x45, 0xDA, 0x4C, 0xDB, 0x4F,
            0xF8, 0x10, 0xF9, 0x10, 0xFA, 0x10, 0xFA, 0x10,
            0xFB, 0x10, 0x3c];
        let mut output = vec![];
        {
            let mut m = Machine::with_rom(rom);
            m.attach_console(&mut output);
            m.run_until_halt();
        }
        assert_eq!(String::from_utf8(output).ok().unwrap(), "HELLO");
    }
}
