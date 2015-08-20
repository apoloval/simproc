//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use time::Duration;

#[derive(Clone, Copy)]
pub struct ClockFreq { mhz: f64, }

impl ClockFreq {
    pub fn khz(val: f64) -> Self { ClockFreq { mhz: val / 1000.0 }}
    pub fn mhz(val: f64) -> Self { ClockFreq { mhz: val }}

    /// Return the duration equivalent to the given cycles for this clock
    pub fn cycles(&self, n: usize) -> Duration {
        let period = 1000.0 / self.mhz;
        Duration::nanoseconds((period * n as f64) as i64)
    }
}

impl Default for ClockFreq {
    fn default() -> Self { ClockFreq::mhz(3.5) }
}

#[cfg(test)]
mod test {

    use time::Duration;

    use super::*;

    #[test]
    fn should_compute_cycles() {
        let clock = ClockFreq::mhz(2.0);
        assert_eq!(clock.cycles(4), Duration::microseconds(2));
        assert_eq!(clock.cycles(11), Duration::nanoseconds(5500));
    }
}
