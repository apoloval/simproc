//
// SimProc Machine
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use docopt::Docopt;

static USAGE: &'static str = "
Usage: spm <romfile>
       spm --help
       spm --version

Options:
    -h, --help                  Print this message
    -v, --version               Print the spasm version
";

#[derive(RustcDecodable)]
#[allow(dead_code)]
pub struct Args {
    arg_romfile: String,
    flag_help: bool,
    flag_version: bool,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Action { Help, Version, Exec }

impl Args {

    pub fn action(&self) -> Action {
        if self.flag_help { Action::Help }
        else if self.flag_version { Action::Version }
        else { Action::Exec }
    }

    pub fn romfile(&self) -> &String { &self.arg_romfile }
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
            arg_romfile: "".to_string(),
            flag_help: true,
            flag_version: false,
        };
        assert_eq!(Action::Help, args.action());
    }

    #[test]
    fn should_produce_version_from_args() {
        let args = Args {
            arg_romfile: "".to_string(),
            flag_help: false,
            flag_version: true,
        };
        assert_eq!(Action::Version, args.action());
    }

    #[test]
    fn should_produce_exec_from_args() {
        let args = Args {
            arg_romfile: "".to_string(),
            flag_help: false,
            flag_version: false,
        };
        assert_eq!(Action::Exec, args.action());
    }

    #[test]
    fn should_produce_romfile_from_args() {
        let args = Args {
            arg_romfile: "foobar.rom".to_string(),
            flag_help: false,
            flag_version: false,
        };
        assert_eq!("foobar.rom", args.romfile());
    }

    #[test]
    fn should_parse_help() {
        let argv = || vec!["spm", "--version"];
        let args: Args = Docopt::new(super::USAGE)
                        .and_then(|d| d.argv(argv().into_iter()).decode())
                        .unwrap_or_else(|e| e.exit());
        assert!(args.flag_version);
        assert!(!args.flag_help);
    }

    #[test]
    fn should_parse_exec() {
        let argv = || vec!["spm", "foobar.rom"];
        let args: Args = Docopt::new(super::USAGE)
                        .and_then(|d| d.argv(argv().into_iter()).decode())
                        .unwrap_or_else(|e| e.exit());
        assert!(!args.flag_version);
        assert!(!args.flag_help);
    }
}
