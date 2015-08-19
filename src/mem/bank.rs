//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use mem::addr::*;
use mem::mem::*;

pub struct MemBank<P0: Memory, P1: Memory, P2: Memory, P3: Memory> {
    page0: P0,
    page1: P1,
    page2: P2,
    page3: P3,
}

impl<P0: Memory, P1: Memory, P2: Memory, P3: Memory> MemBank<P0, P1, P2, P3> {

    pub fn with_pages(p0: P0, p1: P1, p2: P2, p3: P3) -> Self {
        MemBank { page0: p0, page1: p1, page2: p2, page3: p3 }
    }

    fn select_page(&self, addr: Addr) -> (&Memory, Addr) {
        if addr < 0x4000 { (&self.page0, addr) }
        else if addr < 0x8000 { (&self.page1, (addr - 0x4000) as u16) }
        else if addr < 0xc000 { (&self.page2, (addr - 0x8000) as u16) }
        else { (&self.page3, (addr - 0xc000) as u16) }
    }

    fn select_page_mut(&mut self, addr: Addr) -> (&mut Memory, Addr) {
        if addr < 0x4000 { (&mut self.page0, addr) }
        else if addr < 0x8000 { (&mut self.page1, (addr - 0x4000) as u16) }
        else if addr < 0xc000 { (&mut self.page2, (addr - 0x8000) as u16) }
        else { (&mut self.page3, (addr - 0xc000) as u16) }
    }
}

impl<P0: Memory, P1: Memory, P2: Memory, P3: Memory> Memory for MemBank<P0, P1, P2, P3> {

    fn read(&self, addr: Addr) -> u8 {
        let (page, mapped_addr) = self.select_page(addr);
        page.read(mapped_addr)
    }

    fn write(&mut self, addr: Addr, byte: u8) {
        let (page, mapped_addr) = self.select_page_mut(addr);
        page.write(mapped_addr, byte)
    }
}

#[cfg(test)]
mod test {

    use mem::addr::*;
    use mem::mem::*;
    use mem::page::*;

    use super::*;

    #[test]
    fn should_address_pages() {
        let mut p0 = RamPage::new();
        let mut p1 = RamPage::new();
        let mut p2 = RamPage::new();
        let mut p3 = RamPage::new();
        p0.write(0x0000, 1);
        p1.write(0x0000, 2);
        p2.write(0x0000, 3);
        p3.write(0x0000, 4);

        let bank = MemBank::with_pages(p0, p1, p2, p3);
        assert_eq!(bank.read(0x0000), 1);
        assert_eq!(bank.read(0x4000), 2);
        assert_eq!(bank.read(0x8000), 3);
        assert_eq!(bank.read(0xc000), 4);
    }
}
