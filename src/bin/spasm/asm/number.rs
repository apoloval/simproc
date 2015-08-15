//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::u8;

pub fn to_u8(n: i64) -> Option<u8> {
	if n <= u8::MAX as i64 && n >= u8::MIN as i64 { Some(n as u8) }
	else { None }
}

#[cfg(test)]
mod test {

	use quickcheck::*;

	use super::*;

	fn to_u8_converts_back(n: i64) -> bool {
		match to_u8(n) {
			Some(c) => (c as i64) == n,
			None => true,
		}
	}

	#[test]
	fn should_convert_to_u8() {
		quickcheck(to_u8_converts_back as fn(i64) -> bool);
	}
}
