//
// SimProc Assembler
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::fmt;
use std::iter::IntoIterator;
use std::ops::Index;

use simproc::inst::{Reg};

use asm::lexer::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
	Number(TextLoc, i64),
	Reg(TextLoc, Reg),
}

impl Expr {
    pub fn number(l: usize, c: usize, n: i64) -> Expr {
        Expr::Number(loc!(l, c, format!("{}", n)), n)
    }

    pub fn reg(l: usize, c: usize, r: Reg) -> Expr {
        Expr::Reg(loc!(l, c, format!("{}", r)), r)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    	write!(fmt, "{}", self.loc().txt)
    }
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
	pub fn empty() -> ExprList {
		ExprList {
			loc: TextLoc::undef(),
			list: Vec::new(),
		}
	}

	pub fn from_expr(e: Expr) -> ExprList {
		ExprList {
			loc: e.loc().clone(),
			list: vec![e],
		}
	}

	pub fn append(&mut self, comma_loc: TextLoc, e: Expr) {
		self.loc = if self.loc.is_undef() { e.loc().clone() }
            else { self.loc.clone() + &comma_loc + e.loc() };
		self.list.push(e);
	}

    pub fn is_empty(&self) -> bool { self.len() == 0 }

	pub fn len(&self) -> usize { self.list.len() }
}

impl Index<usize> for ExprList {
	type Output = Expr;

    fn index<'a>(&self, i: usize) -> &Expr {
        &self.list[i]
    }
}

impl TextLocate for ExprList {
	fn loc(&self) -> &TextLoc { &self.loc }
}

macro_rules! exprlist {
    () => (ExprList::empty());
    ($( $e:expr ),+) => ({
        let mut list = ExprList::empty();
        $(
            if list.is_empty() { list = ExprList::from_expr($e) }
            else {
                let e = $e;
                let cl = {
                    let el = e.loc();
                    loc!(el.line, el.col - 2, ",")
                };
                list.append(cl, e);
            }
        )*
        list
    });
}

type Label = Option<String>;
type DirectName = String;
type DirectArgs = ExprList;
type MnemoName = String;
type MnemoArgs = ExprList;

#[derive(Debug, PartialEq)]
pub enum Statement {
	Direct(TextLoc, Label, DirectName, DirectArgs),
	Mnemo(TextLoc, Label, MnemoName, MnemoArgs),
	Empty(TextLoc, Label),
}

impl Statement {
	pub fn label(&self) -> &Option<String> {
		match self {
			&Statement::Direct(_, ref lab, _, _) => lab,
			&Statement::Mnemo(_, ref lab, _, _) => lab,
			&Statement::Empty(_, ref lab) => lab,
		}
	}
}

impl TextLocate for Statement {
	fn loc(&self) -> &TextLoc {
		match self {
			&Statement::Direct(ref loc, _, _, _) => loc,
			&Statement::Mnemo(ref loc, _, _, _) => loc,
			&Statement::Empty(ref loc, _) => loc,
		}
	}
}

#[derive(Debug, PartialEq)]
pub enum SyntaxError {
	UnexpectedToken(Token),
	UnexpectedEof,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    	match self {
    		&SyntaxError::UnexpectedToken(ref tok) =>
    			write!(fmt, "unexpected token {}", tok.loc().txt),
    		&SyntaxError::UnexpectedEof =>
    			write!(fmt, "unexpected end of file"),
    	}
    }
}

pub struct Parser<I: Iterator<Item=Token>> {
	input: I,
}

impl<I: Iterator<Item=Token>> Parser<I> {

	pub fn parse<T>(input: T) -> Self where T: IntoIterator<Item=Token, IntoIter=I> {
		Parser { input: input.into_iter() }
	}

	fn next_statement(&mut self, allow_label: bool) -> Option<Result<Statement, SyntaxError>> {
		match self.input.next() {
			Some(Token::Ident(loc, id)) => self.next_statement_from_id(loc, id, allow_label),
			Some(Token::Direct(loc, dir)) => self.direct_from(loc, dir),
			Some(Token::Eol(loc)) => Some(Ok(Statement::Empty(loc!(loc.line, 1, ""), None))),
			Some(other) => Some(Err(SyntaxError::UnexpectedToken(other))),
			None => None,
		}
	}

    fn next_statement_from_id(
    	&mut self,
    	id_loc: TextLoc,
    	id: String,
    	allow_label: bool) -> Option<Result<Statement, SyntaxError>>
    {
    	match self.input.next() {
    		Some(Token::Colon(c_loc)) =>
    			if allow_label { self.next_with_label(id_loc + &c_loc, id) }
    			else { Some(Err(SyntaxError::UnexpectedToken(Token::Colon(c_loc)))) },
    		Some(Token::Eol(_)) | None =>
    			Some(Ok(Statement::Mnemo(id_loc, None, id, ExprList::empty()))),
    		Some(other) => {
    			match self.next_expr_list_from(other) {
    				Ok(list) =>
    					Some(Ok(Statement::Mnemo(id_loc + list.loc(), None, id, list))),
    				Err(e) => Some(Err(e)),
    			}
    		},
    	}
    }

    fn next_with_label(
    	&mut self, loc: TextLoc, label: String) -> Option<Result<Statement, SyntaxError>>
    {
    	match self.next_statement(false) {
    		Some(Ok(Statement::Direct(l, _, i, a))) =>
    			Some(Ok(Statement::Direct(loc + &l, Some(label), i, a))),
    		Some(Ok(Statement::Mnemo(l, _, i, a))) =>
    			Some(Ok(Statement::Mnemo(loc + &l, Some(label), i, a))),
    		Some(Ok(Statement::Empty(_, _))) =>
    			Some(Ok(Statement::Empty(loc, Some(label)))),
			other => other,
    	}
    }

    fn direct_from(&mut self,
    			   dir_loc: TextLoc,
    			   dir: String) -> Option<Result<Statement, SyntaxError>> {
    	match self.input.next() {
    		Some(Token::Eol(_)) | None =>
    			Some(Ok(Statement::Direct(dir_loc, None, dir, ExprList::empty()))),
    		Some(other) => {
    			match self.next_expr_list_from(other) {
    				Ok(list) => Some(Ok(Statement::Direct(dir_loc + list.loc(), None, dir, list))),
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
		self.next_statement(true)
	}
}

#[cfg(test)]
mod test {

	use simproc::inst::*;
	use asm::lexer::*;

    use super::*;

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
    			None,
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
	    		None,
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
    			None,
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
    fn should_parse_labeled_mnemo() {
    	let input = vec![
    		ident!(1, 1, "foo"),
    		colon!(1, 4),
    		ident!(1, 6, "nop"),
    		eol!(1, 6),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Mnemo(
    			loc!(1, 1, "foo: nop"),
    			Some("foo".to_string()),
    			"nop".to_string(),
    			ExprList::empty()))),
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
    		Some(Ok(Statement::Direct(
    			loc![1, 1, ".dir"],
    			None,
    			"dir".to_string(),
    			ExprList::empty()))),
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
    			None,
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

    #[test]
    fn should_parse_labeled_directive() {
    	let input = vec![
    		ident!(1, 1, "foo"),
    		colon!(1, 4),
    		direct!(1, 6, "dir"),
    		eol!(1, 9),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Direct(
    			loc![1, 1, "foo: .dir"],
    			Some("foo".to_string()),
    			"dir".to_string(),
    			ExprList::empty()))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_empty() {
    	let input = vec![
    		eol!(1, 6),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Empty(loc!(1, 1, ""), None))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_parse_labeled_empty() {
    	let input = vec![
    		ident!(1, 1, "foo"),
    		colon!(1, 4),
    		eol!(1, 5),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Ok(Statement::Empty(
    			loc!(1, 1, "foo:"),
    			Some("foo".to_string())))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }

    #[test]
    fn should_fail_parse_two_consecutive_labels() {
    	let input = vec![
    		ident!(1, 1, "foo"),
    		colon!(1, 4),
    		ident!(1, 6, "bar"),
    		colon!(1, 9),
    		eol!(1, 10),
    	];
    	let mut parser = Parser::parse(input);
    	assert_eq!(
    		Some(Err(SyntaxError::UnexpectedToken(colon!(1, 9)))),
    		parser.next());
    	assert_eq!(
    		Some(Ok(Statement::Empty(loc!(1, 1, ""), None))),
    		parser.next());
    	assert_eq!(None, parser.next());
    }
}