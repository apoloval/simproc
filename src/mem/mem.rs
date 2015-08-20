//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use mem::addr::*;

/// A memory device that can be attached to a SimProc CPU
pub trait Memory {
    /// Read a byte from the given memory address
    fn read(&self, addr: Addr) -> u8;

    /// Write a byte into the given memory address
    fn write(&mut self, addr: Addr, byte: u8);

    /// Write the given bytes from the given address
    /// In case of memory overflow, the remaining bytes are just ignored.
    fn write_bytes(&mut self, addr: Addr, bytes: &[u8]) {
        for (offset, byte) in bytes.iter().enumerate() {
            if let Some(dst) = addr_from_usize(addr as usize + offset) {
                self.write(dst, *byte);
            }
        }
    }
}
