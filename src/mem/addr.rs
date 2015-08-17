//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::ops::{Range, Sub};
use std::u16;


/// An address in SP-80 of 16-bits
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Addr(pub u16);

impl Addr {
    pub fn range() -> Range<i64> { u16::MIN as i64 .. u16::MAX as i64 }

    pub fn from_i64(n: i64) -> Option<Addr> {
        if n < u16::MIN as i64 || n > u16::MAX as i64 { None }
        else { Some(Addr(n as u16)) }
    }

    pub fn to_u16(&self) -> u16 {
        let &Addr(n) = self;
        n
    }
}

impl Sub for Addr {
    type Output = Option<RelAddr>;

    fn sub(self, other: Addr) -> Option<RelAddr> {
        let Addr(lhs) = self;
        let Addr(rhs) = other;
        let diff = (lhs as i64) - (rhs as i64);
        if diff < RADDR_MIN as i64 || diff > RADDR_MAX as i64 { None }
        else { Some(RelAddr(diff as i16)) }
    }
}

/// A relative address, i.e. a delta respect the current PC.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RelAddr(pub i16);

const RADDR_MAX: i16 = 511;
const RADDR_MIN: i16 = -512;
