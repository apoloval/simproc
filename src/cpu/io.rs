//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::boxed::Box;
use std::collections::HashMap;

pub trait IoDevice {
    fn read(&mut self) -> u8;
    fn write(&mut self, val: u8);
}

pub struct Io<'a> { devs: HashMap<u8, Box<IoDevice + 'a>> }

impl<'a> Io<'a> {

    pub fn new() -> Self { Io { devs: HashMap::with_capacity(256) }}

    pub fn attach<D: IoDevice + 'a>(&mut self, port: u8, dev: D) {
        self.devs.insert(port, Box::new(dev));
    }

    pub fn read(&mut self, port: u8) -> u8 {
        self.devs.get_mut(&port).map(|d| d.read()).unwrap_or(0)
    }

    pub fn write(&mut self, port: u8, val: u8) {
        if let Some(dev) = self.devs.get_mut(&port) {
            dev.write(val);
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn should_read_zero_on_unattached() {
        let mut io = Io::new();
        assert_eq!(io.read(10), 0);
    }

    #[test]
    fn should_read_from_attached() {
        let mut val = 42;
        let mut io = Io::new();
        let dev = FakeDev { val: &mut val };
        io.attach(10, dev);

        assert_eq!(io.read(10), 42);
    }

    #[test]
    fn should_write_to_attached() {
        let mut val = 0;
        {
            let mut io = Io::new();
            let dev = FakeDev::with_val(&mut val);
            io.attach(10, dev);
            io.write(10, 42);
        }
        assert_eq!(val, 42);
    }

    struct FakeDev<'a> { pub val: &'a mut u8 }

    impl<'a> FakeDev<'a> {
        fn with_val(val: &'a mut u8) -> Self { FakeDev { val: val }}
    }

    impl<'a> IoDevice for FakeDev<'a> {
        fn read(&mut self) -> u8 { *self.val }
        fn write(&mut self, val: u8) { *self.val = val }
    }
}
