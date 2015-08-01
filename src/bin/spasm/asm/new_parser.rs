//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::iter::IntoIterator;

use simproc::inst::{Reg};

use asm::lexer::*;

#[derive(Debug, PartialEq)]
pub enum Expr {
	Number(TextLoc, i64),
	Reg(TextLoc, Reg),
}

impl TextLocate for Expr {
	fn loc(&self) -> &TextLoc {
		match self {
			&Expr::Number(ref loc, _) => loc,
			&Expr::Reg(ref loc, _) => loc,
		}
	}
}

#[derive(Debug, PartialEq)]
pub struct ExprList {
	loc: TextLoc,
	list: Vec<Expr>,
}

impl ExprList {
	fn empty() -> ExprList {
		ExprList {
			loc: TextLoc::undef(),
			list: Vec::new(),
		}
	}

	fn from_expr(e: Expr) -> ExprList {
		ExprList {
			loc: e.loc().clone(),
			list: vec![e],
		}
	}

	fn append(&mut self, comma_loc: TextLoc, e: Expr) {
		self.loc = self.loc.clone() + &comma_loc + e.loc();
		self.list.push(e);
	}
}

impl TextLocate for ExprList {
	fn loc(&self) -> &TextLoc { &self.loc }
}

#[derive(Debug, PartialEq)]
pub enum Statement {
	DeclLabel(TextLoc, String),
	Direct(TextLoc, String, ExprList),
	Mnemo(TextLoc, String, ExprList),
	Empty(TextLoc),
}

impl TextLocate for Statement {
	fn loc(&self) -> &TextLoc {
		match self {
			&Statement::DeclLabel(ref loc, _) => loc,
			&Statement::Direct(ref loc, _, _) => loc,
			&Statement::Mnemo(ref loc, _, _) => loc,
			&Statement::Empty(ref loc) => loc,
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum SyntaxError {
	UnexpectedToken(Token),
	UnexpectedEof,
}

pub struct Parser<I: Iterator<Item=Token>> {
	input: I,
}

impl<I: Iterator<Item=Token>> Parser<I> {

	pub fn parse<T>(input: T) -> Self where T: IntoIterator<Item=Token, IntoIter=I> {
		Parser { input: input.into_iter() }
	}

    fn next_from_id(&mut self, id_loc: TextLoc, id: String) -> Option<Result<Statement, SyntaxError>> {
    	match self.input.next() {
    		Some(Token::Colon(c_loc)) => Some(Ok(Statement::DeclLabel(id_loc + &c_loc, id))),
    		Some(Token::Eol(_)) | None => Some(Ok(Statement::Mnemo(id_loc, id, ExprList::empty()))),
    		Some(other) => {
    			match self.next_expr_list_from(other) {
    				Ok(list) => Some(Ok(Statement::Mnemo(id_loc + list.loc(), id, list))),
    				Err(e) => Some(Err(e)),
    			}
    		},
    	}
    }

    fn direct_from(&mut self,
    			   dir_loc: TextLoc,
    			   dir: String) -> Option<Result<Statement, SyntaxError>> {
    	match self.input.next() {
    		Some(Token::Eol(_)) | None => Some(Ok(Statement::Direct(dir_loc, dir, ExprList::empty()))),
    		Some(other) => {
    			match self.next_expr_list_from(other) {
    				Ok(list) => Some(Ok(Statement::Direct(dir_loc + list.loc(), dir, list))),
    				Err(e) => Some(Err(e)),
    			}
    		},
    	}
    }

    fn next_expr(&mut self) -> Result<Expr, SyntaxError> {
    	match self.input.next() {
    		Some(tk) => self.next_expr_from(tk),
    		None => Err(SyntaxError::UnexpectedEof),
    	}
    }

    fn next_expr_from(&mut self, tk: Token) -> Result<Expr, SyntaxError> {
    	match tk {
    		Token::Register(loc, reg) => Ok(Expr::Reg(loc, reg)),
    		Token::Number(loc, n) => Ok(Expr::Number(loc, n)),
    		other => return Err(SyntaxError::UnexpectedToken(other)),
    	}
    }

    fn next_expr_list_from(&mut self, tk: Token) -> Result<ExprList, SyntaxError> {
    	let head = try!(self.next_expr_from(tk));
    	let mut list = ExprList::from_expr(head);
    	loop {
	    	match self.input.next() {
	    		Some(Token::Comma(loc)) => { list.append(loc, try!(self.next_expr())) },
	    		Some(Token::Eol(_)) | None => return Ok(list),
	    		Some(other) => return Err(SyntaxError::UnexpectedToken(other)),
	    	}
    	}
    }
}

impl<I: Iterator<Item=Token>> Iterator for Parser<I> {
	type Item = Result<Statement, SyntaxError>;

	fn next(&mut self) -> Option<Result<Statement, SyntaxError>> {
		match self.input.next() {
			Some(Token::Ident(loc, id)) => self.next_from_id(loc, id),
			Some(Token::Direct(loc, dir)) => self.direct_from(loc, dir),
			Some(Token::Eol(loc)) => Some(Ok(Statement::Empty(loc))),
			Some(other) => Some(Err(SyntaxError::UnexpectedToken(other))),
			None => None,
		}
	}
}

#[cfg(test)]
mod test {

	use simproc::inst::*;
	use asm::lexer::*;

    use super::*;

    #[test]
    fn should_parse_label() {
    	let input = vec![
    		ident!(1, 1, "foo"),
    		colon!(1, 4),
    		eol!(1, 5),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(Some(Ok(Statement::DeclLabel(
    		loc!(1, 1, "foo:"), "foo".to_string()))), parser.next());
    	assert_eq!(Some(Ok(Statement::Empty(loc!(1, 5, "\n")))), parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_nullary_mnemo() {
    	let input = vec![
    		ident!(1, 1, "nop"),
    		eol!(1, 6),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Mnemo(
    			loc!(1, 1, "nop"),
    			"nop".to_string(),
    			ExprList::empty()))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_unary_mnemo() {
    	let input = vec![
    		ident!(1, 1, "push"),
    		reg!(1, 6, Reg::R0),
    		eol!(1, 9),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Mnemo(
    			loc!(1, 1, "push R0"),
    			"push".to_string(),
    			ExprList::from_expr(Expr::Reg(loc!(1, 6, "R0"), Reg::R0))))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_binary_mnemo() {
    	let input = vec![
    		ident!(1, 1, "add"),
    		reg!(1, 5, Reg::R0),
    		comma!(1, 7),
    		reg!(1, 9, Reg::R1),
    		eol!(1, 12),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Mnemo(
    			loc!(1, 1, "add R0, R1"),
    			"add".to_string(),
    			ExprList {
    				loc: loc!(1, 5, "R0, R1"),
    				list: vec![
    				Expr::Reg(loc!(1, 5, "R0"), Reg::R0),
    				Expr::Reg(loc!(1, 9, "R1"), Reg::R1)]
    			}))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_nullary_directive() {
    	let input = vec![
    		direct!(1, 1, "dir"),
    		eol!(1, 5),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Direct(loc![1, 1, ".dir"], "dir".to_string(), ExprList::empty()))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_parameterized_directive() {
    	let input = vec![
    		direct!(1, 1, "dir"),
    		number!(1, 6, 1),
    		comma!(1, 7),
    		number!(1, 9, 2),
    		comma!(1, 10),
    		number!(1, 12, 3),
    		comma!(1, 13),
    		number!(1, 15, 4),
    		eol!(1, 16),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Direct(
    			loc![1, 1, ".dir 1, 2, 3, 4"],
    			"dir".to_string(),
    			ExprList {
    				loc: loc!(1, 6, "1, 2, 3, 4"),
    				list: vec![
	    				Expr::Number(loc!(1, 6, "1"), 1),
	    				Expr::Number(loc!(1, 9, "2"), 2),
	    				Expr::Number(loc!(1, 12, "3"), 3),
	    				Expr::Number(loc!(1, 15, "4"), 4)]
    			}))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }
}