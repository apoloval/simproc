//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::iter::IntoIterator;

use asm::expr::*;
use asm::lexer::*;

type Label = Option<String>;
type DirectName = String;
type DirectArgs = ExprList;
type MnemoName = String;
type MnemoArgs = ExprList;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Empty(Line, Label),
	Direct(Line, Label, DirectName, DirectArgs),
	Mnemo(Line, Label, MnemoName, MnemoArgs),
}

#[derive(Debug, PartialEq)]
pub enum SyntaxError {
	UnexpectedToken(Line, Token),
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    	match self {
    		&SyntaxError::UnexpectedToken(ref line, ref tok) =>
    			write!(fmt, "in line {}: unexpected {}\n\t{}", line.row, tok, line.content),
    	}
    }
}

pub type ParserInput = ScannerOutput;
pub type ParserOutput = Result<Statement, SyntaxError>;

pub struct Parser<I: Iterator<Item=ParserInput>> {
	input: I,
}

impl<I: Iterator<Item=ParserInput>> Parser<I> {

	pub fn parse<T>(input: T) -> Self where T: IntoIterator<Item=ParserInput, IntoIter=I> {
		Parser { input: input.into_iter() }
	}

	fn next_statement(&mut self, allow_label: bool) -> Option<Result<Statement, SyntaxError>> {
		match self.input.next() {
			Some(Token::Ident(id)) => self.next_statement_from_id(id, allow_label),
			Some(Token::Direct(dir)) => self.direct_from(dir),
			Some(Token::Eol(line)) => Some(Ok(Statement::Empty(line, None))),
			Some(other) =>
                Some(Err(SyntaxError::UnexpectedToken(self.discard_until_eol(), other))),
			None => None,
		}
	}

    fn next_statement_from_id(
        &mut self,
        id: String,
        allow_label: bool) -> Option<Result<Statement, SyntaxError>>
    {
    	match self.input.next() {
    		Some(Token::Colon) =>
    			if allow_label { self.next_with_label(id) }
    			else {
                    Some(Err(SyntaxError::UnexpectedToken(
                        self.discard_until_eol(), Token::Colon)))
                },
    		Some(Token::Eol(line)) =>
    			Some(Ok(Statement::Mnemo(line, None, id, vec![]))),
    		Some(other) => {
    			match self.next_expr_list_from(other) {
    				Ok((list, line)) => Some(Ok(Statement::Mnemo(line, None, id, list))),
    				Err(e) => Some(Err(e)),
    			}
    		},
            None => unreachable!(),
    	}
    }

    fn next_with_label(&mut self, label: String) -> Option<Result<Statement, SyntaxError>> {
    	match self.next_statement(false) {
    		Some(Ok(Statement::Direct(l, _, i, a))) =>
    			Some(Ok(Statement::Direct(l, Some(label), i, a))),
    		Some(Ok(Statement::Mnemo(l, _, i, a))) =>
    			Some(Ok(Statement::Mnemo(l, Some(label), i, a))),
    		Some(Ok(Statement::Empty(l, _))) =>
    			Some(Ok(Statement::Empty(l, Some(label)))),
			other => other,
    	}
    }

    fn direct_from(&mut self, dir: String) -> Option<Result<Statement, SyntaxError>> {
    	match self.input.next() {
    		Some(Token::Eol(line)) =>
    			Some(Ok(Statement::Direct(line, None, dir, vec![]))),
    		Some(other) => {
    			match self.next_expr_list_from(other) {
    				Ok((list, line)) => Some(Ok(Statement::Direct(line, None, dir, list))),
    				Err(e) => Some(Err(e)),
    			}
    		},
            None => unreachable!(),
    	}
    }

    fn next_expr(&mut self) -> Result<Expr, SyntaxError> {
    	match self.input.next() {
    		Some(tk) => self.next_expr_from(tk),
    		None => unreachable!(),
    	}
    }

    fn next_expr_from(&mut self, tk: Token) -> Result<Expr, SyntaxError> {
    	match tk {
            Token::AddrReg(reg) => Ok(Expr::AddrReg(reg)),
    		Token::Reg(reg) => Ok(Expr::Reg(reg)),
    		Token::Number(n) => Ok(Expr::Number(n)),
            Token::Ident(id) => Ok(Expr::Ident(id)),
            Token::Eol(line) => Err(SyntaxError::UnexpectedToken(line.clone(), Token::Eol(line))),
    		other => Err(SyntaxError::UnexpectedToken(self.discard_until_eol(), other)),
    	}
    }

    fn next_expr_list_from(&mut self, tk: Token) -> Result<(ExprList, Line), SyntaxError> {
    	let head = try!(self.next_expr_from(tk));
    	let mut list = vec!(head);
    	loop {
	    	match self.input.next() {
	    		Some(Token::Comma) => { list.push(try!(self.next_expr())); },
	    		Some(Token::Eol(line)) => return Ok((list, line)),
	    		Some(other) =>
                    return Err(SyntaxError::UnexpectedToken(self.discard_until_eol(), other)),
                None => unreachable!(),
	    	}
    	}
    }

    fn discard_until_eol(&mut self) -> Line {
        loop {
            match self.input.next() {
                Some(Token::Eol(line)) => return line,
                None => unreachable!(),
                _ => {},
            }
        }
    }
}

impl<I: Iterator<Item=ParserInput>> Iterator for Parser<I> {

	type Item = ParserOutput;

	fn next(&mut self) -> Option<Result<Statement, SyntaxError>> {
		self.next_statement(true)
	}
}

#[cfg(test)]
mod test {

    use simproc::cpu::*;

    use asm::expr::*;
	use asm::lexer::*;

    use super::*;

    #[test]
    fn should_parse_nullary_mnemo() {
    	let input = vec![
    		ident!("nop"),
    		eol!(1, "nop"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Mnemo(
    			sline!(1, "nop"),
    			None,
    			"nop".to_string(),
    			vec![]))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_unary_mnemo() {
    	let input = vec![
    		ident!("push"),
    		Token::Reg(Reg::R0),
    		eol!(1, "push R0"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Mnemo(
    			sline!(1, "push R0"),
	    		None,
    			"push".to_string(),
    			vec!(Expr::Reg(Reg::R0))))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_binary_mnemo() {
    	let input = vec![
    		ident!("add"),
    		Token::Reg(Reg::R0),
    		Token::Comma,
    		Token::Reg(Reg::R1),
    		eol!(1, "add R0, R1"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Mnemo(
    			sline!(1, "add R0, R1"),
    			None,
    			"add".to_string(),
    			vec![Expr::Reg(Reg::R0), Expr::Reg(Reg::R1)]
    			))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_labeled_mnemo() {
    	let input = vec![
    		ident!("foo"),
    		Token::Colon,
    		ident!("nop"),
    		eol!(1, "foo: nop"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Mnemo(
    			sline!(1, "foo: nop"),
    			Some("foo".to_string()),
    			"nop".to_string(),
    			vec![]))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_nullary_directive() {
    	let input = vec![
    		direct!("dir"),
    		eol!(1, ".dir"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Direct(
    			sline!(1, ".dir"),
    			None,
    			"dir".to_string(),
    			vec![]))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_parameterized_directive() {
    	let input = vec![
    		direct!("dir"),
    		Token::Number(1),
    		Token::Comma,
    		Token::Number(2),
    		Token::Comma,
    		Token::Number(3),
    		Token::Comma,
    		Token::Number(4),
    		eol!(1, ".dir 1, 2, 3, 4"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Direct(
    			sline!(1, ".dir 1, 2, 3, 4"),
    			None,
    			"dir".to_string(),
    			vec![Expr::Number(1), Expr::Number(2), Expr::Number(3), Expr::Number(4)]))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_labeled_directive() {
    	let input = vec![
    		ident!("foo"),
    		Token::Colon,
    		direct!("dir"),
    		eol!(1, "foo: .dir"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Direct(
    			sline!(1, "foo: .dir"),
    			Some("foo".to_string()),
    			"dir".to_string(),
    			vec![]))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_empty() {
    	let input = vec![eol!(1, "")];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Empty(sline!(1, ""), None))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_labeled_empty() {
    	let input = vec![
    		ident!("foo"),
    		Token::Colon,
    		eol!(1, "foo:"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Empty(
    			sline!(1, "foo:"),
    			Some("foo".to_string())))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_fail_parse_two_consecutive_labels() {
    	let input = vec![
    		ident!("foo"),
    		Token::Colon,
    		ident!("bar"),
    		Token::Colon,
    		eol!(1, "foo: bar:"),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Err(SyntaxError::UnexpectedToken(sline!(1, "foo: bar:"), Token::Colon))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }
}
