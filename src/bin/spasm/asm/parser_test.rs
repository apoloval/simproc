//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use asm::parser::*;

#[test]
fn should_read_lines() {
	let input = b"hello\nworld\n";
	let lines = read_lines(input).unwrap();
	assert_eq!(lines[0], "hello");
	assert_eq!(lines[1], "world");
}

#[test]
fn should_read_lines_on_missing_blankend() {
	let input = b"hello\nworld";
	let lines = read_lines(input).unwrap();
	assert_eq!(lines[0], "hello");
	assert_eq!(lines[1], "world");
}

fn should_parse(line: &str, expected: &Token) {
	let actual = parse(line).unwrap();
	assert_eq!(actual, *expected);
}

#[test]
fn should_parse_label() {
	should_parse("main:", &Token::Label("main".to_string()));
}

#[test]
fn should_parse_label_with_comment() {
	should_parse("main: ; comment", &Token::Label("main".to_string()));
}

#[test]
fn should_parse_label_with_extra_spaces() {
	should_parse("   \tmain  \t : \t  ", &Token::Label("main".to_string()));
}

#[test]
fn should_parse_mnemonic0() {
	should_parse("reti", &Token::Mnemonic("reti".to_string(), vec!()));
}

#[test]
fn should_parse_mnemonic0_with_comment() {
	should_parse("reti ; comment", &Token::Mnemonic("reti".to_string(), vec!()));
}

#[test]
fn should_parse_mnemonic0_with_extra_spaces() {
	should_parse("   \t reti   \t  ", &Token::Mnemonic("reti".to_string(), vec!()));
}

#[test]
fn should_parse_mnemonic1() {
	should_parse("push r1", &Token::Mnemonic("push".to_string(), vec!("r1".to_string())));
}

#[test]
fn should_parse_mnemonic1_with_comment() {
	should_parse("push r1 ; comment", &Token::Mnemonic("push".to_string(), vec!("r1".to_string())));
}

#[test]
fn should_parse_mnemonic1_with_extra_spaces() {
	should_parse(" \t push \t r1 \t ", 
		&Token::Mnemonic("push".to_string(), vec!("r1".to_string())));
}

#[test]
fn should_parse_mnemonic2() {
	should_parse("add r1, r2", 
		&Token::Mnemonic("add".to_string(), vec!("r1".to_string(), "r2".to_string())));
}

#[test]
fn should_parse_mnemonic2_with_comment() {
	should_parse("add r1, r2 ; comment", 
		&Token::Mnemonic("add".to_string(), vec!("r1".to_string(), "r2".to_string())));
}

#[test]
fn should_parse_mnemonic2_with_extra_spaces() {
	should_parse(" \t add \t r1 \t ,  \t  r2  \t ", 
		&Token::Mnemonic("add".to_string(), vec!("r1".to_string(), "r2".to_string())));
}

#[test]
fn should_parse_blank() {
	should_parse("", &Token::Blank);
}

#[test]
fn should_parse_blank_with_comments() {
	should_parse("   ; comment", &Token::Blank);
}

#[test]
fn should_parse_blank_with_extra_spaces() {
	should_parse("       ", &Token::Blank);
}

#[test]
fn should_tokenize() {
	let lines = vec!(
		"; This is a test program".to_string(),
		"main:    ; this is the entry point of the program".to_string(),
		"   ldi r0, 5".to_string(),
		"   ldi r1, 4".to_string(),
		"   add r0, r1".to_string(),
	);
	let tokens = tokenize(&lines);
	assert_eq!(5, tokens.len());
}

#[test]
fn should_detect_syntax_errors() {
	let lines = vec!(
		"; This is a test program".to_string(),
		"main:    ; this is the entry point of the program".to_string(),
		" +- ; this is a syntax error".to_string(),
		"++".to_string(),
		"   add r0, r1".to_string(),
	);
	let tokens = tokenize(&lines);
	assert_eq!(Token::LexicalError, tokens[2]);
	assert_eq!(Token::LexicalError, tokens[3]);
}

#[test]
fn should_parse_dec_num() {
	assert_eq!(Some(100), parse_num("100"));
}

#[test]
fn should_parse_neg_dec_num() {
	assert_eq!(Some(-100), parse_num("-100"));
}

#[test]
fn should_parse_hex_num() {
	assert_eq!(Some(0x100), parse_num("0x100"));
}

#[test]
fn should_parse_neg_hex_num() {
	assert_eq!(Some(-0x100), parse_num("-0x100"));
}
