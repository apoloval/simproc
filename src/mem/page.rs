//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use mem::addr::*;
use mem::mem::*;

pub static PAGE_SIZE: usize = 0x4000;

pub struct RamPage {
    bytes: Vec<u8>,
}

impl RamPage {
    pub fn new() -> RamPage {
        let mut ram = RamPage { bytes: Vec::with_capacity(PAGE_SIZE) };
        unsafe { ram.bytes.set_len(PAGE_SIZE) }
        ram
    }
}

impl Memory for RamPage {
    fn read(&self, addr: Addr) -> u8 {
        let offset = addr as usize;
        if offset < self.bytes.len() { self.bytes[offset] }
        else { 0 }
    }

    fn write(&mut self, addr: Addr, byte: u8) {
        let offset = addr as usize;
        if offset < self.bytes.len() { self.bytes[offset] = byte }
    }
}

pub struct RomPage {
    bytes: Vec<u8>,
}

impl RomPage {
    pub fn with_content<I: IntoIterator<Item=u8>>(input: I) -> RomPage {
        let mut rom = RomPage { bytes: input.into_iter().collect() };
        rom.bytes.reserve_exact(PAGE_SIZE);
        unsafe { rom.bytes.set_len(PAGE_SIZE) }
        rom
    }
}

impl Memory for RomPage {
    fn read(&self, addr: Addr) -> u8 {
        let offset = addr as usize;
        if offset < self.bytes.len() { self.bytes[offset] }
        else { 0 }
    }

    fn write(&mut self, _: Addr, _: u8) {}
}


#[cfg(test)]
mod test {

    use quickcheck::*;

    use mem::addr::*;
    use mem::mem::*;

    use super::*;

    #[test]
    fn ram_page_should_read_and_write() {
        let mut ram = RamPage::new();
        ram.write(0x100, 1);
        assert_eq!(ram.read(0x100), 1);

        ram.write(0x3fff, 2);
        assert_eq!(ram.read(0x3fff), 2);

        ram.write(0xffff, 3);
        assert_eq!(ram.read(0xffff), 0);
    }

    #[test]
    fn rom_page_should_read() {
        fn rom_contains_is_read(bytes: Vec<u8>) -> bool {
            let rom = RomPage::with_content(bytes.clone());
            for (addr, byte) in bytes.iter().enumerate() {
                if rom.read(addr as u16) != *byte { return false }
            }
            return true;
        }
        quickcheck(rom_contains_is_read as fn(Vec<u8>) -> bool);
    }
}
