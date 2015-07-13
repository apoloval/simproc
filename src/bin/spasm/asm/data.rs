//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use byteorder::{LittleEndian, WriteBytesExt};

use asm::assembly::*;
use asm::err::*;
use asm::parser::*;

pub fn assemble_data(data_size: u8, data: &[&str], ctx: &AssemblyContext) -> Result<Vec<u8>, Error> {
	let mut res = vec!();
	for d in data {
		match data_size {
			1 => try!(assembly_byte(d, ctx, &mut res)),
			2 => try!(assembly_word(d, ctx, &mut res)),
			other => panic!("invalid data size {}", other),
		}
	}
	Ok(res)
}

fn assembly_byte(data: &str, ctx: &AssemblyContext, res: &mut Vec<u8>) -> Result<(), Error> {
	res.write_u8(try!(resolve_data(data, ctx)) as u8).ok().unwrap();
	Ok(())
}

fn assembly_word(data: &str, ctx: &AssemblyContext, res: &mut Vec<u8>) -> Result<(), Error> {
	res.write_u16::<LittleEndian>(try!(resolve_data(data, ctx)) as u16).ok().unwrap();
	Ok(())
}

fn resolve_data(data: &str, ctx: &AssemblyContext) -> Result<i64, Error> {
	parse_num(data)
		.or_else(|| ctx.resolve(data))
		.ok_or(Error::BadNumber(data.to_string()))
}

#[cfg(test)]
mod test {

	use asm::assembly::*;

	use super::*;

	#[test]
	fn should_assemble_bytes() {
		let ctx = AssemblyContext::new();
		let data = vec!("1", "2", "3");
		assert_eq!(
			Ok(vec!(1u8, 2u8, 3u8)),
			assemble_data(1, &data, &ctx));
	}

	#[test]
	fn should_assemble_bytes_by_name() {
		let mut ctx = AssemblyContext::new();
		ctx.define_value("foobar", 80);
		let data = vec!("1", "2", "foobar");
		assert_eq!(
			Ok(vec!(1u8, 2u8, 80u8)),
			assemble_data(1, &data, &ctx));
	}

	#[test]
	fn should_assemble_words() {
		let ctx = AssemblyContext::new();
		let data = vec!("1", "2", "3");
		assert_eq!(
			Ok(vec!(1u8, 0u8, 2u8, 0u8, 3u8, 0u8)),
			assemble_data(2, &data, &ctx));
	}

	#[test]
	fn should_assemble_words_by_name() {
		let mut ctx = AssemblyContext::new();
		ctx.define_value("foobar", 80);
		let data = vec!("1", "2", "foobar");
		assert_eq!(
			Ok(vec!(1u8, 0u8, 2u8, 0u8, 80u8, 0u8)),
			assemble_data(2, &data, &ctx));
	}
}
