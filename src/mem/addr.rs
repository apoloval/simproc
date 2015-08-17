//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::ops::{Add, Range, Sub};
use std::u16;


/// An address in SP-80 of 16-bits
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Addr(pub u16);

impl Addr {
    pub fn range() -> Range<i64> { u16::MIN as i64 .. u16::MAX as i64 }

    pub fn from_usize(n: usize) -> Option<Addr> {
        if n < u16::MIN as usize || n > u16::MAX as usize { None }
        else { Some(Addr(n as u16)) }
    }

    pub fn from_i64(n: i64) -> Option<Addr> {
        if n < u16::MIN as i64 || n > u16::MAX as i64 { None }
        else { Some(Addr(n as u16)) }
    }

    pub fn to_u16(&self) -> u16 {
        let &Addr(n) = self;
        n
    }

    pub fn to_usize(&self) -> usize { self.to_u16() as usize }
    pub fn to_i64(&self) -> i64 { self.to_u16() as i64 }
}

impl Add<usize> for Addr {
    type Output = Option<Addr>;

    fn add(self, other: usize) -> Option<Addr> {
        Addr::from_usize(self.to_usize() + other)
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

impl fmt::LowerHex for Addr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{:x}", self.to_u16())
    }
}

/// A relative address, i.e. a delta respect the current PC.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RelAddr(pub i16);

const RADDR_MAX: i16 = 511;
const RADDR_MIN: i16 = -512;
