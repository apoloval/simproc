//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use docopt::Docopt;

static USAGE: &'static str = "
Usage: spasm [--bin] <input>
       spasm --text <input>
       spasm --help
       spasm --version

Options:
    -t, --text                  Write text assembly information into STDOUT 
    -h, --help                  Print this message
    -v, --version               Print the spasm version
";

#[derive(RustcDecodable)]
pub struct Args {
    arg_input: String,
    flag_bin: bool,
    flag_text: bool,
    flag_help: bool,
    flag_version: bool,
}

pub enum Action { Help, Version, Bin, Text }

impl Args {

    pub fn action(&self) -> Action {
        if self.flag_help { Action::Help }
        else if self.flag_version { Action::Version }
        else if self.flag_bin { Action::Bin }
        else if self.flag_text { Action::Text }
        else { Action::Bin }
    }

    pub fn input_file(&self) -> &String { &self.arg_input }

    pub fn output_file(&self) -> Option<String> {
        match self.action() {
            Action::Bin => Some(format!("{}.bin", self.arg_input)),
            _ => None,
        }
    }
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
        let argv = || vec!["spasm", "--version"];
        let args: Args = Docopt::new(super::USAGE)
                        .and_then(|d| d.argv(argv().into_iter()).decode())
                        .unwrap_or_else(|e| e.exit());
        assert!(args.flag_version);
    }

    #[test]
    fn should_parse_bin() {
        let argv = || vec!["spasm", "--bin", "foobar.asm"];
        let args: Args = Docopt::new(super::USAGE)
                        .and_then(|d| d.argv(argv().into_iter()).decode())
                        .unwrap_or_else(|e| e.exit());
        assert!(!args.flag_text);
        assert!(args.flag_bin);
    }
}