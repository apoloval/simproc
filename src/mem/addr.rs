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
    if diff < RELADDR_RANGE.start as i64 || diff > RELADDR_RANGE.end as i64 { None }
    else { Some(diff as i16) }
}

pub const RELADDR_RANGE: Range<i64> = Range { start: -512, end: 511 };

pub type RelAddr = i16;
