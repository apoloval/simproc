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
#[allow(dead_code)]
pub struct Args {
    arg_input: String,
    flag_bin: bool,
    flag_text: bool,
    flag_help: bool,
    flag_version: bool,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
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

#[allow(dead_code)]
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
    fn should_produce_help_from_args() {
        let args = Args {
            arg_input: "".to_string(),
            flag_bin: false,
            flag_text: false,
            flag_help: true,
            flag_version: false,
        };
        assert_eq!(Action::Help, args.action());
    }

    #[test]
    fn should_produce_version_from_args() {
        let args = Args {
            arg_input: "".to_string(),
            flag_bin: false,
            flag_text: false,
            flag_help: false,
            flag_version: true,
        };
        assert_eq!(Action::Version, args.action());
    }

    #[test]
    fn should_produce_bin_from_args() {
        let args = Args {
            arg_input: "".to_string(),
            flag_bin: true,
            flag_text: false,
            flag_help: false,
            flag_version: false,
        };
        assert_eq!(Action::Bin, args.action());
    }

    #[test]
    fn should_produce_text_from_args() {
        let args = Args {
            arg_input: "".to_string(),
            flag_bin: false,
            flag_text: true,
            flag_help: false,
            flag_version: false,
        };
        assert_eq!(Action::Text, args.action());
    }

    #[test]
    fn should_produce_input_file_from_args() {
        let args = Args {
            arg_input: "foobar.asm".to_string(),
            flag_bin: true,
            flag_text: false,
            flag_help: false,
            flag_version: false,
        };
        assert_eq!("foobar.asm", args.input_file());
    }

    #[test]
    fn should_produce_output_file_from_args() {
        let args = Args {
            arg_input: "foobar.asm".to_string(),
            flag_bin: true,
            flag_text: false,
            flag_help: false,
            flag_version: false,
        };
        assert_eq!("foobar.asm.bin", args.output_file().unwrap());
    }

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