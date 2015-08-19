//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ops::{Range};
use std::u16;

pub const ADDR_RANGE: Range<i64> = Range { start: u16::MIN as i64, end:u16::MAX as i64 };

/// An address in SP-80 of 16-bits
pub type Addr = u16;

pub fn addr_from_usize(n: usize) -> Option<Addr> {
    if n < u16::MIN as usize || n > u16::MAX as usize { None }
    else { Some(n as u16) }
}

pub fn addr_from_i64(n: i64) -> Option<Addr> {
    if n < u16::MIN as i64 || n > u16::MAX as i64 { None }
    else { Some(n as u16) }
}

pub fn offset_addr(base: Addr, offset: usize) -> Option<Addr> {
    addr_from_usize(base as usize + offset)
}

pub fn addr_dist(from: Addr, to: Addr) -> Option<RelAddr> {
    let diff = (to as i64) - (from as i64);
    if diff < RADDR_MIN as i64 || diff > RADDR_MAX as i64 { None }
    else { Some(RelAddr(diff as i16)) }
}

/// A relative address, i.e. a delta respect the current PC.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RelAddr(pub i16);

const RADDR_MAX: i16 = 511;
const RADDR_MIN: i16 = -512;
