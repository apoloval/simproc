//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use docopt::Docopt;

static USAGE: &'static str = "
Usage: spasm --text <input>
       spasm --help
       spasm --version

Options:
    -t, --text                  Write text assembly information into STDOUT 
    -h, --help                  Print this message
    -v, --version               Print the spasm version
";

#[derive(RustcDecodable)]
pub struct Args {
	pub arg_input: String,
	pub flag_help: bool,
    pub flag_version: bool,
}

pub fn parse_args() -> Args {
	Docopt::new(USAGE)
		.and_then(|d| d.decode())
		.unwrap_or_else(|e| e.exit())
}

#[cfg(test)]
mod test {

	use super::*;

	use docopt::Docopt;

	#[test]
	fn should_parse_help() {
		let argv = |&:| vec!["spasm", "--version"];
		let args: Args = Docopt::new(super::USAGE)
                        .and_then(|d| d.argv(argv().into_iter()).decode())
                        .unwrap_or_else(|e| e.exit());
		assert!(args.flag_version);
	}
}