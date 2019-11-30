//! # Rusk Parser
//!
//! use result of [`scanner`].
//!
//! [`scanner`]: ../scanner/index.html
use crate::scanner;
use crate::ast;
use std::collections::HashSet;


#[derive(Debug)]
struct Parser
{
	source_map: Box<scanner::SourceMap>,
	tokens: Vec<Box<scanner::Token>>,
	pos: usize,
}


struct Save
{
	pos: usize,
}


#[derive(Debug)]
pub enum Error
{
	NotFound(Box<HashSet<NotFound>>),
}


impl Error
{
	fn merge(self: Box<Error>, e2: Box<Error>) -> Box<Error>
	{
		let Error::NotFound(mut set1) = *self;
		let Error::NotFound(set2) = *e2;
		for i in set2.iter()
		{
			set1.insert(*i);
		}
		Box::new(Error::NotFound(set1))
	}

	pub fn report(self: Box<Error>, path: &String, source_map: Box<SourceMap>)
	{
		let Error::NotFound(set) = *self;
		for item in (*set).iter()
		{
			let pos = source_map.position(&item);
			println!("{}:{}:{}:error: expect {:?}", path, pos.line_num(), pos.col_num(),item);
		}
	}
}


#[derive(Debug)]
pub struct SourceMap
{
	input: Box<scanner::SourceMap>,
	tokens: Vec<Box<scanner::Token>>,
}


impl SourceMap
{
	fn position(&self, item: &NotFound) -> scanner::Position
	{
		let mut offset = item.offset();
		if offset == self.tokens.len()
		{
			offset -= 1;
		}
		self.input.position(self.tokens.get(offset).unwrap())
	}
}


type Offset = usize;


#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum NotFound
{
	Name(Offset),
	Number(Offset),
	TokString(Offset),
	Arrow(Offset),
	ParenLeft(Offset),
	ParenRight(Offset),
	Comma(Offset),
	Colon(Offset),
	TransitionArrow(Offset),
	SemiColon(Offset),
	LeftArrow(Offset),
	Equal(Offset),
	DoubleArrow(Offset),
	BracketLeft(Offset),
	BracketRight(Offset),
	BraceLeft(Offset),
	VerticalBar(Offset),
	BraceRight(Offset),
	ChannelSetLeft(Offset),
	ChannelSetRight(Offset),
	TargetSetLeft(Offset),
	TargetSetRight(Offset),
	Ampersand(Offset),

	Any(Offset),
	Case(Offset),
	Const(Offset),
	Eof(Offset),
	Event(Offset),
	Exists(Offset),
	Exists1(Offset),
	Fn(Offset),
	ForAll(Offset),
	If(Offset),
	In(Offset),
	Invariant(Offset),
	Let(Offset),
	Post(Offset),
	State(Offset),
	Target(Offset),
	Transition(Offset),
	Type(Offset),
	Var(Offset),
	When(Offset),
	EventSetPragma(Offset),
	SetPragma(Offset),
}


impl NotFound
{
	fn singleton_boxed(elem: NotFound) -> Box<HashSet<NotFound>>
	{
		let mut set = Box::new(HashSet::new());
		set.insert(elem);
		set
	}

	fn offset(&self) -> usize
	{
		match self
		{
		NotFound::Name(offset) => *offset,
		NotFound::Number(offset) => *offset,
		NotFound::TokString(offset) => *offset,
		NotFound::Arrow(offset) => *offset,
		NotFound::ParenLeft(offset) => *offset,
		NotFound::ParenRight(offset) => *offset,
		NotFound::Comma(offset) => *offset,
		NotFound::Colon(offset) => *offset,
		NotFound::TransitionArrow(offset) => *offset,
		NotFound::SemiColon(offset) => *offset,
		NotFound::LeftArrow(offset) => *offset,
		NotFound::Equal(offset) => *offset,
		NotFound::DoubleArrow(offset) => *offset,
		NotFound::BracketLeft(offset) => *offset,
		NotFound::BracketRight(offset) => *offset,
		NotFound::BraceLeft(offset) => *offset,
		NotFound::VerticalBar(offset) => *offset,
		NotFound::BraceRight(offset) => *offset,
		NotFound::ChannelSetLeft(offset) => *offset,
		NotFound::ChannelSetRight(offset) => *offset,
		NotFound::TargetSetLeft(offset) => *offset,
		NotFound::TargetSetRight(offset) => *offset,
		NotFound::Ampersand(offset) => *offset,

		NotFound::Any(offset) => *offset,
		NotFound::Case(offset) => *offset,
		NotFound::Const(offset) => *offset,
		NotFound::Eof(offset) => *offset,
		NotFound::Event(offset) => *offset,
		NotFound::Exists(offset) => *offset,
		NotFound::Exists1(offset) => *offset,
		NotFound::Fn(offset) => *offset,
		NotFound::ForAll(offset) => *offset,
		NotFound::If(offset) => *offset,
		NotFound::In(offset) => *offset,
		NotFound::Invariant(offset) => *offset,
		NotFound::Let(offset) => *offset,
		NotFound::Post(offset) => *offset,
		NotFound::State(offset) => *offset,
		NotFound::Target(offset) => *offset,
		NotFound::Transition(offset) => *offset,
		NotFound::Type(offset) => *offset,
		NotFound::Var(offset) => *offset,
		NotFound::When(offset) => *offset,
		NotFound::EventSetPragma(offset) => *offset,
		NotFound::SetPragma(offset) => *offset,
		}
	}
}


/// parse tokens.
pub fn parse_tokens(tokens: Box<scanner::Tokenizer>) -> Result<Box<ast::Module>, (Box<SourceMap>, Box<Error>)>
{
	Parser::parse_tokens(tokens)
}


#[cfg(test)]
fn test_parse(input: &str)
{
	parse_tokens(scanner::scan(input.into()).expect("scan")).expect("parse");
}


#[test]
fn test_empty()
{
	test_parse("");
	test_parse("// commment");
	test_parse("/* commment */");
}


#[test]
fn test_event()
{
	test_parse("event e1;");
	test_parse("event e1,;");
	test_parse("event e1, e2;");
	test_parse("event e1, e2,;");

	test_parse("event e1;");
	test_parse("event e1|;");
	test_parse("event e1| e2;");
	test_parse("event e1| e2|;");

	test_parse("event e;");
	test_parse("event e@(summary);");
	test_parse("event e@(-summary-);");
	test_parse("event e(param:Type);");
	test_parse("event e@{-description-};");
	test_parse("event e@(summ)(p1:T1)@{-desc-};");

	test_parse("event e p;");
	test_parse("event e p p;");
	test_parse("event e p p p;");
	test_parse("event e (p:T);");
	test_parse("event e (p q:T U);");
	test_parse("event e (p:T) (p:T);");
	test_parse("event e (p:T) (p:T) (p:T);");
}


#[test]
fn test_type()
{
	test_parse("type x = {};");
	test_parse("type x = { f1: T1 };");
	test_parse("type x@(summary) = { f1: T1 };");
	test_parse("type x = { f1: T1 }@{-description-};");
	test_parse("type x@(summary) = { f1: T1 }@{-description-};");

	test_parse("type x = { f1: T1 @{- desc -}, };");
	test_parse("type x = { f1: T1, @// desc\n};");

	test_parse("type x = { f1: T1, };");
	test_parse("type x = { f1: T1, f2: T2 };");
	test_parse("type x = { f1: T1, f2: T2, };");

	test_parse("type x = a b;");
	test_parse("type x y = a b;");
	test_parse("type Either a b = Left a | Right b;");

	test_parse("type x = a;");
	test_parse("type x = a |;");
	test_parse("type x = a | b;");
	test_parse("type x = a | b |;");
	test_parse("type x = a | b | c {} | {};");
}


#[test]
fn test_const()
{
	test_parse("const x=1;");
	test_parse("const x@(summary)=1;");
	test_parse("const x=1@{-description-};");
	test_parse("const x@(summary)=1@{-description-};");
	//XXX: test_parse("const x(param:Type)=1;");
}


#[test]
fn test_expr()
{
	test_parse("const x = 1;");
	test_parse("const x = ab_c1_23;");
	test_parse("const x = _;");
	test_parse("const x = _zzz;");
	test_parse("const x = (1 + 2) * (3 - 4) / 5;");
	test_parse("const x = 1 == 2 || 3 /= 4 && 5 < 6 || 7 <= 8 || 9 > 10 || 1 >= 12 || a => b;");
	test_parse(r#"const x = "a" ++ "b";"#);
	test_parse("const x = 1 <+> 2;");
	test_parse("const x = a.b.c;");
	test_parse("const f = a.b c.d (e f g);");
	test_parse("const f = a.b c.d $ e f $ g;");

	test_parse("const r = Rec {};");
	test_parse("const r = Rec { a=1 };");
	test_parse("const r = Rec { a=1, };");
	test_parse("const r = Rec { a=1, b=2 };");
	test_parse("const r = Rec { a=1, b=2, };");
	test_parse("const r = r { a <- 1 };");
	test_parse("const r = r { a <- 1, };");
	test_parse("const r = r { a <- 1, b <- 2 };");
	test_parse("const r = r { a <- 1, b <- 2, };");
	test_parse("const r = { r | a <- 1 };"); // compat.
	test_parse("const r = { r | a <- 1, };");
	test_parse("const r = { r | a <- 1, b <- 2 };");
	test_parse("const r = { r | a <- 1, b <- 2, };");

	test_parse("const r = case 1 {};");
	test_parse("const r = case 1 { 1 => 2; };");
	test_parse("const r = case 1 { 1 => 2; Just x => Ok y; };");

	test_parse("const r = if 1 { 2 } else { 3 };");

	test_parse("const r = [];");
	test_parse("const r = [ 1 ];");
	test_parse("const r = [ 1, ];");
	test_parse("const r = [ 1, 2 ];");
	test_parse("const r = [ 1, 2, ];");
	test_parse("const r = [1] ++ [2];");
	test_parse("const r = [1] -- [2];");

	test_parse("const r = [ 1 | 2 ];");
	test_parse("const r = [ 1 | 2, ];");
	test_parse("const r = [ 1 | 2, 3 ];");
	test_parse("const r = [ 1 | 2, 3, ];");

	test_parse("const r = [ f a | a in s & a < threshold ];");
	test_parse("const r = [ f a | a : T & a < threshold ];");

	test_parse("const x = __set {};");
	test_parse("const x = __set { 1 };");
	test_parse("const x = __set { 1, };");
	test_parse("const x = __set { 1, 2 };");
	test_parse("const x = __set { 1, 2, };");
	test_parse("const x = __set { 1, 2, 3 };");
	test_parse("const x = __set { 1, 2, 3, };");

	test_parse("const x = let a = 1 in y;");
	test_parse("const x = let a = 1, in y;");
	test_parse("const x = let a = 1, b = 2 in y;");
	test_parse("const x = let a = 1, b = 2, in y;");
	test_parse("const x = let a = 1, b = 2, c = 3 in y;");
	test_parse("const x = let a = 1, b = 2, c = 3, in y;");
	test_parse("const x = let a : Int = 1 in y;");

	test_parse("const x = {| a |};");
	test_parse("const x = |[ a ]|;");

	test_parse("const x = fn:t -> e;");
	test_parse("const x = fn x :t -> e;");
	test_parse("const x = fn x (y:Y) (z 1: Z 2) :t -> e;");
	test_parse("const x : fn x (y:Y) (z 1: Z 2) :t = fn x (y:Y) (z 1: Z 2) :t -> e;");

	// tuple?
	// bind?, pipe?.
}


#[test]
fn test_proc()
{
	test_parse("const x = p -> q;"); // prefix operator.
	test_parse("const x = p -- ev_set;");
	test_parse("const x = p ||| q;");
	test_parse("const x = STOP;");
	test_parse("const x = SKIP;");

	test_parse("const x = p `__sequential_composition` q;"); // p ; q
	test_parse("const x = p [] q;");

	test_parse("const x = p |[ {ev} ]| q;");
	test_parse("const x = p |[ {| chan |} ]| q;");
	test_parse("const x = p |[ set ]| q;");

	test_parse("const x = p |[ {} ]| q;");
	test_parse("const x = p |[ {| |} ]| q;");
	test_parse("const x = p |[ ]| q;");

	test_parse("const x = p ||| q;");

	test_parse("const x = __event_set {};");
	test_parse("const x = __event_set { a };");
	test_parse("const x = __event_set { a, };");
	test_parse("const x = __event_set { a, b };");
	test_parse("const x = __event_set { a, b, };");

	test_parse("const x = {||};");
	test_parse("const x = {| a |};");
	test_parse("const x = {| a, |};");
	test_parse("const x = {| a, b |};");
	test_parse("const x = {| a, b, |};");

	test_parse("const x = |[]|;");
	test_parse("const x = |[ a ]|;");
	test_parse("const x = |[ a, ]|;");
	test_parse("const x = |[ a, b ]|;");
	test_parse("const x = |[ a, b, ]|;");
}


#[test]
fn test_invariant()
{
	test_parse("invariant {}");
	test_parse("invariant x {}");
	test_parse("invariant @(y) {}");
	test_parse("invariant {}@{-desc-}");
	test_parse("invariant x@(y){}@{-desc-}");

	test_parse("invariant { x }");
	test_parse("invariant { x; }");
	test_parse("invariant { x; y }");
	test_parse("invariant { x; y; }");

	test_parse("invariant { x => y; }");
	test_parse("invariant { a b && c => d (e f); }");
	test_parse("invariant { x; y; z; }");
}


#[test]
fn test_state_var()
{
	test_parse("state s{}");
	test_parse("state s{ var x; }");
	test_parse("state s{ var x=1; }");
	test_parse("state s{ var x@(y)=1; }");
	test_parse("state s{ var x:Int=1; }");
	test_parse("state s{ var x=1@{-desc-}; }");
	test_parse("state s{ var x@(y):Int=1@{-desc-}; }");
}


#[test]
fn test_state_invariant()
{
	test_parse("state s{ invariant {} }");
	test_parse("state s{ invariant x {} }");
	test_parse("state s{ invariant @(y) {} }");
	test_parse("state s{ invariant {}@{-desc-} }");
	test_parse("state s{ invariant x@(y){}@{-desc-} }");

	test_parse("state s{ invariant { x; } }");
	test_parse("state s{ invariant { x => y; } }");
	test_parse("state s{ invariant { a b && c => d (e f); } }");
	test_parse("state s{ invariant { x; y; z; } }");
}


#[test]
fn test_state_transition()
{
	test_parse("state s{ transition a --> { post{ } } }");
	test_parse("state s{ transition a --> { post{ target a; } } }");
	test_parse("state s{ transition a --> { post{ target a; } } }");
	test_parse("state s{ transition a --> { post{ target a,; } } }");
	test_parse("state s{ transition a --> { post{ target a, b; } } }");
	test_parse("state s{ transition a --> { post{ target a, b,; } } }");
	test_parse("state s{ transition a --> { post{ target a; target b; } } }");
	test_parse("state s{ transition a --> { post{ target state; } } }");
	test_parse("state s{ transition a --> { post{ target state, a; } } }");
	test_parse("state s{ transition a --> { post{ target a, state; } } }");

	test_parse("state s{ transition a --> { post{ a' == a } } }");
	test_parse("state s{ transition a --> { post{ a' == a; } } }");
	test_parse("state s{ transition a --> { post{ a' == a; b' == b } } }");
	test_parse("state s{ transition a --> { post{ a' == a; b' == b; } } }");
	test_parse("state s{ transition a --> { post{ state' == state } } }");
	test_parse("state s{ transition a --> { post{ state' == state; } } }");
	test_parse("state s{ transition a --> { post{ a' == a; state' == state } } }");
	test_parse("state s{ transition a --> { post{ a' == a; state' == state; } } }");
	test_parse("state s{ transition a --> { post{ state' == state; a' == a } } }");
	test_parse("state s{ transition a --> { post{ state' == state; a' == a; } } }");

	// compat.
	test_parse("state s{ transition a --> { post{ a' = a } } }");
	test_parse("state s{ transition a --> { post{ a' = a; } } }");
	test_parse("state s{ transition a --> { post{ a' = a; b' = b } } }");
	test_parse("state s{ transition a --> { post{ a' = a; b' = b; } } }");
	test_parse("state s{ transition a --> { post{ state' = state } } }");
	test_parse("state s{ transition a --> { post{ state' = state; } } }");
	test_parse("state s{ transition a --> { post{ a' = a; state' = state } } }");
	test_parse("state s{ transition a --> { post{ a' = a; state' = state; } } }");
	test_parse("state s{ transition a --> { post{ state' = state; a' = a } } }");
	test_parse("state s{ transition a --> { post{ state' = state; a' = a; } } }");
}


struct ExprOpts
{
	record_construction: bool,
	application: bool,
	in_op: bool,
	as_a_op: bool,
	prefix_op: bool,
	imply_op: bool,
}


impl ExprOpts
{
	fn standard() -> ExprOpts
	{
		ExprOpts {
			record_construction: true, // a { ... }.
			application: true, // a b.
			in_op: true, // a in b.
			as_a_op: true, // a : b.
			prefix_op: true,  // a -> b.
			imply_op: true, // a => b.
		}
	}


	fn followed_by_block() -> ExprOpts
	{
		let mut opts = Self::standard();
		opts.record_construction = false;
		opts
	}


	fn no_type() -> ExprOpts
	{
		let mut opts = Self::standard();
		opts.as_a_op = false;
		opts
	}


	fn no_prefix_op() -> ExprOpts
	{
		let mut opts = Self::standard();
		opts.prefix_op = false;
		opts
	}
}


impl Parser
{
	fn parse_tokens(tokens: Box<scanner::Tokenizer>) -> Result<Box<ast::Module>, (Box<SourceMap>, Box<Error>)>
	{
		let (source_map, tokens) = tokens.results();
		let mut me = Box::new(Parser {
			source_map,
			tokens,
			pos: 0,
		});

		let module = match me.module()
		{
			Ok(module) => module,
			Err(err) => return Err((me.source_map(), err)),
		};

		if me.pos != me.tokens.len()
		{
			let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Eof(me.pos))));
			return Err((me.source_map(), err));
		}

		Ok(module)
	}


	fn source_map(self) -> Box<SourceMap>
	{
		Box::new(SourceMap {
			input: self.source_map,
			tokens: self.tokens,
		})
	}


	fn save(&self) -> Save
	{
		Save {
			pos: self.pos,
		}
	}


	fn restore(&mut self, save: &Save)
	{
		self.pos = save.pos;
	}


	fn keyword(&mut self, kind: scanner::KeywordKind) -> bool
	{
		match self.next_token()
		{
			Some(&scanner::Token::Keyword(ref kw)) if kw.kind() == kind =>
				true,

			_ => {
				self.pos -= 1;
				false
			},
		}
	}


	fn punct(&mut self, kind: scanner::PunctuationKind) -> bool
	{
		match self.next_token()
		{
			Some(&scanner::Token::Punctuation(ref punct)) if punct.kind() == kind =>
				true,

			_ => {
				self.pos -= 1;
				false
			},
		}
	}


	fn punct_arrow(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct_oper(scanner::PunctOperKind::Arrow)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Arrow(self.pos)))))
		}
	}


	fn punct_paren_left(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::ParenLeft)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ParenLeft(self.pos)))))
		}
	}


	fn punct_paren_right(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::ParenRight)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ParenRight(self.pos)))))
		}
	}


	fn punct_comma(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::Comma)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Comma(self.pos)))))
		}
	}


	fn punct_transition_arrow(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::TransitionArrow)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::TransitionArrow(self.pos)))))
		}
	}


	fn punct_colon(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct_oper(scanner::PunctOperKind::Colon)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Colon(self.pos)))))
		}
	}


	fn punct_semi_colon(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::SemiColon)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::SemiColon(self.pos)))))
		}
	}


	fn punct_left_arrow(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::LeftArrow)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::LeftArrow(self.pos)))))
		}
	}


	fn punct_equal(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::Equal)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Equal(self.pos)))))
		}
	}


	fn punct_double_arrow(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct_oper(scanner::PunctOperKind::DoubleArrow)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::DoubleArrow(self.pos)))))
		}
	}


	fn punct_brace_left(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::BraceLeft)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::BraceLeft(self.pos)))))
		}
	}


	fn punct_vertical_bar(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::VerticalBar)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::VerticalBar(self.pos)))))
		}
	}


	fn punct_bracket_left(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::BracketLeft)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::BracketLeft(self.pos)))))
		}
	}


	fn punct_bracket_right(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::BracketRight)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::BracketRight(self.pos)))))
		}
	}


	fn punct_brace_right(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::BraceRight)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::BraceRight(self.pos)))))
		}
	}


	fn punct_ampersand(&mut self) -> Result<(), Box<Error>>
	{
		if self.punct(scanner::PunctuationKind::Ampersand)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Ampersand(self.pos)))))
		}
	}


	fn punct_oper(&mut self, kind: scanner::PunctOperKind) -> bool
	{
		match self.next_token()
		{
			Some(&scanner::Token::PunctOper(ref punct)) if punct.kind() == kind =>
				true,

			_ => {
				self.pos -= 1;
				false
			},
		}
	}


	fn operator(&mut self, kind: scanner::OperatorKind) -> bool
	{
		match self.next_token()
		{
			Some(&scanner::Token::Operator(ref op)) if op.kind() == &kind =>
				true,

			_ => {
				self.pos -= 1;
				false
			},
		}
	}


	fn module(&mut self) -> Result<Box<ast::Module>, Box<Error>>
	{
		let mut types = Box::new(Vec::new());
		let mut events = Box::new(Vec::new());
		let mut vars = Box::new(Vec::new());
		let mut states = Box::new(Vec::new());
		let mut invariants = Box::new(Vec::new());

		loop
		{
			if self.pos >= self.tokens.len()
			{
				break;
			}

			let save = self.save();
			let err = match self.type_stmt()
			{
				Ok(stmt) => {
					types.push(stmt);
					continue;
				},
				Err(err) => {
					if self.pos != save.pos
					{
						return Err(err)
					}
					err
				},
			};

			self.restore(&save);
			let err = match self.event_stmt()
			{
				Ok(mut items) => {
					events.extend(items.drain(..));
					continue;
				},
				Err(err2) => {
					if self.pos != save.pos
					{
						return Err(err2)
					}
					err.merge(err2)
				},
			};

			self.restore(&save);
			let err = match self.const_stmt()
			{
				Ok(var) => {
					vars.push(var);
					continue;
				},
				Err(err2) => {
					if self.pos != save.pos
					{
						return Err(err2)
					}
					err.merge(err2)
				},
			};

			self.restore(&save);
			let err = match self.invariant_stmt()
			{
				Ok(stmt) => {
					invariants.push(stmt);
					continue;
				},
				Err(err2) => {
					if self.pos != save.pos
					{
						return Err(err2)
					}
					err.merge(err2)
				},
			};

			self.restore(&save);
			let err = match self.state_stmt()
			{
				Ok(state) => {
					states.push(state);
					continue;
				},
				Err(err2) => {
					if self.pos != save.pos
					{
						return Err(err2)
					}
					err.merge(err2)
				},
			};
			return Err(err);
		}

		let module = ast::Module::new_boxed(types, events, vars, states, invariants);
		Ok(module)
	}


	fn type_stmt(&mut self) -> Result<Box<ast::TypeStmt>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::Type)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Type(self.pos)))));
		}

		let name = self.dotted_name()?;
		let summary = self.at_summary_opt();
		let (args, err) = self.arg_list_opt()?;
		match self.punct_equal()
		{
			Ok(()) => (),
			Err(err2) => return Err(err.merge(err2)),
		}

		let mut items = Vec::new();
		let mut stmt_desc = Box::new(None);

		loop
		{
			let err = match self.punct_semi_colon() {
				Ok(()) => break,
				Err(err) => err,
			};

			let (name, err) = match self.name_string() {
				Ok(name) => (Some(Box::new(name.clone())), err),
				Err(err2) => (None, err.merge(err2)),
			};

			let (fields, err) = match self.punct_brace_left() {
				Ok(()) => (Some(self.record_def_fields()?), err),
				Err(err2) => (None, err.merge(err2)),
			};

			if let Some(fields) = fields
			{
				items.push(ast::TypeItem::new_record_def_boxed(name, fields));
			}else
			{
				match name
				{
					Some(name) => {
						let (args, _err) = self.arg_list_opt()?;
						items.push(ast::TypeItem::new_name_boxed(name, args));
					},
					None =>
						return Err(err),
				}
			}

			match self.punct_vertical_bar()
			{
				Ok(()) => (),
				Err(err) => {
					stmt_desc = self.at_long_description_opt();
					match *stmt_desc
					{
						Some(_) => {
							self.punct_semi_colon()?;
							break;
						},
						None => {
							match self.punct_semi_colon() {
								Ok(()) => break,
								Err(err2) => {
									let err3 = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::SemiColon(self.pos))));
									let err = err.merge(err3);
									return Err(err.merge(err2));
								},
							}
						},
					}
				},
			}
		}

		Ok(ast::TypeStmt::new_boxed(name, summary, args, Box::new(items), stmt_desc))
	}

	fn record_def_fields(&mut self) -> Result<Box<Vec<Box<ast::RecordField>>>, Box<Error>>
	{
		let mut fields = Box::new(Vec::new());
		loop
		{
			let save = self.save();
			match self.punct_brace_right()
			{
				Ok(()) => break,
				Err(_) => (),
			}

			self.restore(&save);
			let name = match self.name() {
				Ok(name) => name,
				// XXX: also maybe at-line description.
				Err(err) => return Err(err),
			};
			let summary = self.at_summary_opt();
			self.punct_colon()?;
			let typ = self.expr()?;
			let mut desc = self.at_short_description_opt();

			// compat.
			if desc.is_none()
			{
				desc = self.at_long_description_opt();
			}

			let save = self.save();
			let err = match self.punct_comma()
			{
				Ok(()) => {
					if desc.is_none()
					{
						desc = self.at_line_description_opt();
					}
					let field = ast::RecordField::new_boxed(name, summary, typ, desc);
					fields.push(field);

					continue;
				},
				Err(err) => err,
			};

			let field = ast::RecordField::new_boxed(name, summary, typ, desc);
			fields.push(field);
			self.restore(&save);
			match self.punct_brace_right()
			{
				Ok(()) => break,
				Err(err2) => return Err(err.merge(err2)),
			}
		}
		Ok(fields)
	}


	fn event_stmt(&mut self) -> Result<Box<Vec<Box<ast::EventItem>>>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::Event)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Event(self.pos)))));
		}

		let mut items = vec![ self.event_item()? ];
		loop
		{
			let save = self.save();

			let err = match self.punct_semi_colon()
			{
				Ok(()) => break,
				Err(err) => err,
			};

			self.restore(&save);
			match self.punct_comma()
			{
				Ok(()) => (),
				Err(err2) => {
					let err = err.merge(err2);
					match self.punct_vertical_bar()
					{
						Ok(()) => (),
						Err(err2) =>
							return Err(err.merge(err2)),
					}
				},
			};

			let err = match self.punct_semi_colon()
			{
				Ok(()) => break,
				Err(err) => err,
			};

			match self.event_item()
			{
				Ok(item) => {
					items.push(item);
					continue
				},
				Err(err2) => return Err(err.merge(err2)),
			}
		}

		Ok(Box::new(items))
	}


	fn event_item(&mut self) -> Result<Box<ast::EventItem>, Box<Error>>
	{
		let name = self.dotted_name()?;
		let summary = self.at_summary_opt();
		let (args, _err) = self.arg_list_opt()?;
		let desc = self.at_long_description_opt();

		Ok(ast::EventItem::new_boxed(name, summary, args, desc))
	}


	fn state_stmt(&mut self) -> Result<Box<ast::State>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::State)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::State(self.pos)))));
		}

		let name = self.name_string()?.clone();
		let summary = self.at_summary_opt();
		let (args, err) = self.arg_list_opt()?;

		match self.punct_brace_left()
		{
			Ok(()) => (),
			Err(err2) => return Err(err.merge(err2)),
		}

		let mut fields = Vec::<Box<ast::Field>>::new();
		loop
		{
			let save = self.save();

			let err = match self.punct_brace_right() {
				Ok(_) => break,
				Err(err) => err,
			};

			self.restore(&save);
			let err = match self.const_stmt() {
				Ok(var) => {
					fields.push(var.into_var_field_boxed());
					continue;
				},
				Err(err2) => {
					if self.pos != save.pos
					{
						return Err(err2);
					}
					err.merge(err2)
				},
			};

			self.restore(&save);
			let err = match self.invariant_stmt() {
				Ok(stmt) => {
					fields.push(Box::new(ast::Field::Invariant(*stmt)));
					continue;
				},
				Err(err2) => {
					if self.pos != save.pos
					{
						return Err(err2);
					}
					err.merge(err2)
				},
			};

			self.restore(&save);
			let err = match self.transition_stmt() {
				Ok(stmt) => {
					fields.push(Box::new(ast::Field::Transition(*stmt)));
					continue;
				},
				Err(err2) => {
					if self.pos != save.pos
					{
						return Err(err2);
					}
					err.merge(err2)
				},
			};

			return Err(err);
		}

		let desc = self.at_long_description_opt();

		let summary = match *summary {
			Some(s) => Some(Box::new(s)),
			None => None,
		};
		let desc = match *desc {
			Some(s) => Some(Box::new(s)),
			None => None,
		};
		Ok(ast::State::new_boxed(name, summary, args, fields, desc))
	}


	fn invariant_stmt(&mut self) -> Result<Box<ast::InvariantField>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::Invariant)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Invariant(self.pos)))));
		}

		let name = {
			let save = self.save();
			match self.name_string()
			{
				Ok(name) => Ok(name.clone()),
				Err(err) => {
					self.restore(&save);
					Err(err)
				},
			}
		};
		let summary = self.at_summary_opt();

		self.punct_brace_left()?;
		let mut formulas = Box::new(Vec::new());

		loop
		{
			let save = self.save();
			let err = match self.punct_brace_right() {
				Ok(_) => break,
				Err(err) => err,
			};

			self.restore(&save);
			let expr = match self.expr() {
				Ok(expr) => expr,
				Err(err2) => return Err(err.merge(err2)),
			};

			formulas.push(expr);

			match self.punct_semi_colon()
			{
				Ok(()) => (),
				Err(err) => match self.punct_brace_right() {
					Ok(()) => break,
					Err(err2) => return Err(err.merge(err2)),
				},
			}
		}

		let desc = self.at_long_description_opt();

		Ok(ast::InvariantField::new_boxed(name.ok(), summary, formulas, desc))
	}


	fn transition_stmt(&mut self) -> Result<Box<ast::TransitionField>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::Transition)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Transition(self.pos)))));
		}

		let name = self.dotted_name()?;
		let (args, err) = self.arg_list_opt()?;

		let save = self.save();
		let guard = if self.keyword(scanner::KeywordKind::When)
		{
			let expr = self.expr()?;
			let desc = self.at_short_description_opt();
			Ok((expr, desc))
		}else
		{
			self.restore(&save);
			let err2 = Box::new(Error::NotFound(NotFound::singleton_boxed(
				NotFound::When(self.pos)
			)));
			Err(err.merge(err2))
		};

		match self.punct_transition_arrow()
		{
			Ok(_) => (),
			Err(err2) => {
				match guard.err()
				{
					Some(err) => return Err(err.merge(err2)),
					None => return Err(err2),
				}
			},
		}
		self.punct_brace_left()?;
		let post_cond = self.post_cond()?;
		self.punct_brace_right()?;

		let desc = self.at_long_description_opt();
		Ok(ast::TransitionField::new_boxed(name, args, guard.ok(), post_cond, desc))
	}


	fn post_cond(&mut self) -> Result<Box<ast::PostCond>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::Post)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Post(self.pos)))));
		}

		let mut targets = Box::new(Vec::new());
		let mut exprs = Box::new(Vec::new());
		self.punct_brace_left()?;
		loop
		{
			let save = self.save();
			if self.keyword(scanner::KeywordKind::Target)
			{
				match self.name_string() {
					Ok(name) => targets.push(name.clone()),
					Err(err) => {
						if self.keyword(scanner::KeywordKind::State)
						{
							targets.push("state".into())
						}else
						{
							let err2 = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::State(self.pos))));
							return Err(err.merge(err2));
						}
					},
				}

				loop
				{
					let save = self.save();
					let err = match self.punct_semi_colon()
					{
						Ok(()) => break,
						Err(err) => err,
					};

					self.restore(&save);
					match self.punct_comma()
					{
						Ok(()) => (),
						Err(err2) => return Err(err.merge(err2)),
					};

					let save = self.save();
					match self.punct_semi_colon()
					{
						Ok(()) => break,
						Err(_) => (),
					};
					self.restore(&save);
					match self.name_string()
					{
						Ok(name) => targets.push(name.clone()),
						Err(err) => if self.keyword(scanner::KeywordKind::State)
							{
								targets.push("state".into())
							}else
							{
								let err2 = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::State(self.pos))));
								return Err(err.merge(err2));
							}
					}
				}
				continue;
			}

			self.restore(&save);
			let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Target(self.pos))));
			let err = match self.punct_brace_right()
			{
				Ok(()) => break,
				Err(err2) => err.merge(err2),
			};

			self.restore(&save);
			let e1 = match self.expr()
			{
				Ok(e1) => e1,
				Err(err2) => return Err(err.merge(err2)),
			};

			let err = match self.punct_equal()
			{
				Ok(()) => {
					let e2 = self.expr()?;
					exprs.push(ast::PostCondItem::new_mutation_boxed(ast::Mutation::new_boxed(e1, None, Box::new("=".into()), e2)));
					None
				},
				Err(err) => {
					exprs.push(ast::PostCondItem::new_expr_boxed(e1));
					Some(err)
				}
			};

			match self.punct_semi_colon()
			{
				Ok(()) => (),
				Err(err2) => match self.punct_brace_right() {
					Ok(()) => break,
					Err(err3) => {
						let err = match err {
							Some(err) => err.merge(err2),
							None => err2,
						};
						return Err(err.merge(err3))
					}
				},
			}
		}

		let desc = self.at_long_description_opt();
		Ok(ast::PostCond::new_boxed(targets, exprs, desc))
	}


	fn arg_list_opt(&mut self) -> Result<(Box<ast::ArgList>, Box<Error>), Box<Error>>
	{
		let mut args = Vec::new();

		loop
		{
			let save = self.save();

			match self.arg()
			{
				Ok(arg) => {
					args.push(arg);
					continue;
				},
				Err(err) => {
					if self.pos == save.pos
					{
						let args = ast::ArgList::new_boxed(Box::new(args));
						return Ok((args, err));
					}else
					{
						return Err(err);
					}
				},
			}
		}
	}


	fn arg(&mut self) -> Result<Box<ast::Arg>, Box<Error>>
	{
		match self.punct_paren_left() {
			Ok(()) => {
				let opts = ExprOpts::no_type();
				let name = self.expr_(&opts)?;
				match self.punct_colon()
				{
					Ok(()) => {
						let typ = self.expr_(&opts)?;
						self.punct_paren_right()?;
						Ok(ast::Arg::new_boxed(name, Some(typ)))
					},
					Err(err) => match self.punct_paren_right() {
						Ok(()) => {
							let name = ast::Expr::new_paren_boxed(name);
							Ok(ast::Arg::new_boxed(name, None))
						},
						Err(err2) => Err(err.merge(err2)),
					},
				}
			},
			Err(err) => {
				match self.name() {
					Ok(name) => Ok(ast::Arg::new_boxed(name, None)),
					Err(err2) => Err(err.merge(err2)),
				}
			},
		}
	}


	fn dotted_name(&mut self) -> Result<Box<ast::DottedName>, Box<Error>>
	{
		let mut names:Vec<String> = match self.name_string() {
			Ok(name) => vec![name.clone()],
			Err(err) => return Err(err),
		};

		loop
		{
			let save = self.save();

			if !self.operator(scanner::OperatorKind::Dot)
			{
				self.restore(&save);
				return Ok(ast::DottedName::new_boxed(names));
			}

			match self.name_string() {
				Ok(name) => names.push(name.clone()),
				Err(err) => return Err(err),
			};
		}
	}

	fn const_stmt(&mut self) -> Result<Box<ast::VarStmt>, Box<Error>>
	{
		let save = self.save();
		if self.keyword(scanner::KeywordKind::Const)
		{
			return self.const_stmt_(ast::VarType::Const);
		}

		self.restore(&save);
		if self.keyword(scanner::KeywordKind::Var)
		{
			return self.const_stmt_(ast::VarType::Var);
		}

		let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Const(self.pos))));
		let err = {
			let err2 = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Var(self.pos))));
			err.merge(err2)
		};
		Err(err)
	}


	fn const_stmt_(&mut self, var_type: ast::VarType) -> Result<Box<ast::VarStmt>, Box<Error>>
	{
		let opts = ExprOpts::no_type();
		let matcher = self.expr_(&opts)?;
		let summary = self.at_summary_opt();

		let save = self.save();
		let typ = match self.punct_colon() {
			Ok(_) => Some(self.expr_(&opts)?),
			Err(_) => {
				self.restore(&save);
				None
			},
		};

		let init = if self.punct(scanner::PunctuationKind::Equal) {
			Some(self.expr()?)
		}else
		{
			None
		};

		let desc = self.at_long_description_opt();
		self.punct_semi_colon()?;

		Ok(ast::VarStmt::new_boxed(
			var_type,
			matcher,
			typ,
			init,
			summary,
			desc,
		))
	}


	fn expr(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let opts = ExprOpts::standard();
		self.or_expr(&opts)
	}


	fn expr_(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		self.or_expr(&opts)
	}


	fn or_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.and_expr(opts)?;
		loop
		{
			let save = &self.save();

			let _op = match self.next_token() {
				Some(&scanner::Token::Operator(ref op)) => {
					match op.kind()
					{
						scanner::OperatorKind::LogiOr =>
							op.clone(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.and_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_or_boxed(lhs, rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn and_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.imply_expr(opts)?;
		loop
		{
			let save = &self.save();

			let _op = match self.next_token() {
				Some(&scanner::Token::Operator(ref op)) => {
					match op.kind()
					{
						scanner::OperatorKind::LogiAnd =>
							op.clone(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.imply_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_and_boxed(lhs, rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn imply_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.comm_expr(opts)?;

		if !opts.imply_op
		{
			return Ok(lhs);
		}

		loop
		{
			let save = &self.save();

			let op:String = match self.next_token() {
				Some(&scanner::Token::PunctOper(ref punct_oper)) => {
					match punct_oper.kind()
					{
						scanner::PunctOperKind::DoubleArrow =>
							punct_oper.to_code().into(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.comm_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_binop_boxed(lhs, op, rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn comm_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.cmp_expr(opts)?;

		if !opts.imply_op
		{
			return Ok(lhs);
		}

		loop
		{
			let save = &self.save();

			let op:String = match self.next_token() {
				Some(&scanner::Token::PunctOper(ref punct_oper)) => {
					match punct_oper.kind()
					{
						scanner::PunctOperKind::Arrow =>
							punct_oper.to_code().into(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.cmp_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_binop_boxed(lhs, op, rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn cmp_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.concat_expr(opts)?;
		loop
		{
			let save = &self.save();

			let op:String = match self.next_token() {
				Some(&scanner::Token::Operator(ref op)) => {
					match op.kind()
					{
						scanner::OperatorKind::CmpEq |
						scanner::OperatorKind::CmpNe |
						scanner::OperatorKind::CmpLt |
						scanner::OperatorKind::CmpLe |
						scanner::OperatorKind::CmpGt |
						scanner::OperatorKind::CmpGe =>
							op.to_code().into(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				Some(&scanner::Token::Keyword(ref kw)) => {
					match kw.kind()
					{
						scanner::KeywordKind::In => {
							if opts.in_op {
								kw.to_code().into()
							}else
							{
								self.restore(save);
								return Ok(lhs);
							}
						},
						_ => {
							self.restore(save);
							return Ok(lhs);
						},
					}
				},
				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.concat_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_cmp_boxed(lhs, op, rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn concat_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.add_expr(opts)?;
		loop
		{
			let save = &self.save();

			let op = match self.next_token() {
				Some(&scanner::Token::Operator(ref op)) => {
					match op.kind()
					{
						scanner::OperatorKind::Concat =>
							op.clone(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.add_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_append_boxed(lhs, op.to_code().into(), rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn add_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.mul_expr(opts)?;
		loop
		{
			let save = &self.save();

			let op = match self.next_token() {
				Some(&scanner::Token::Operator(ref op)) => {
					match op.kind()
					{
						scanner::OperatorKind::Add |
						scanner::OperatorKind::Sub =>
							op.clone(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.mul_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_add_boxed(lhs, op.to_code().into(), rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn mul_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.user_binop(opts)?;
		loop
		{
			let save = &self.save();

			let op = match self.next_token() {
				Some(&scanner::Token::Operator(ref op)) => {
					match op.kind()
					{
						scanner::OperatorKind::Mul |
						scanner::OperatorKind::Div =>
							op.clone(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.user_binop(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_mul_boxed(lhs, op.to_code().into(), rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn user_binop(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.appli_expr(opts)?;
		loop
		{
			let save = &self.save();

			let op = match self.next_token() {
				Some(&scanner::Token::Operator(ref op)) => {
					match op.kind()
					{
						scanner::OperatorKind::UserDefined(ref op) =>
							op.text().clone(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.appli_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_mul_boxed(lhs, op, rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn appli_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.as_a_expr(opts)?;
		if !opts.application
		{
			return Ok(lhs);
		}

		loop
		{
			let save = &self.save();
			match self.as_a_expr(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_appli_boxed(lhs, rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn as_a_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.record_construction(opts)?;
		if !opts.as_a_op
		{
			return Ok(lhs);
		}

		loop
		{
			let save = &self.save();

			let op:String = match self.next_token() {
				Some(&scanner::Token::PunctOper(ref punct_oper)) => {
					match punct_oper.kind()
					{
						scanner::PunctOperKind::Colon =>
							punct_oper.to_code().into(),
						_ => {
							self.restore(save);
							return Ok(lhs)
						},
					}
				},

				_ => {
					self.restore(save);
					return Ok(lhs)
				}
			};

			match self.record_construction(opts)
			{
				Ok(rhs) => {
					lhs = ast::Expr::new_binop_boxed(lhs, op, rhs);
					continue
				},
				Err(_) => {
					self.restore(save);
					return Ok(lhs)
				}
			}
		}
	}


	fn record_construction(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let expr = self.prop_expr(opts)?;

		if !opts.record_construction
		{
			return Ok(expr);
		}

		let save = self.save();
		if self.punct_brace_left().is_err()
		{
			self.restore(&save);
			return Ok(expr);
		}

		let mut elems = Vec::new();
		loop
		{
			let save = self.save();
			let err = match self.punct_brace_right()
			{
				Ok(()) => break,
				Err(err) => err,
			};

			self.restore(&save);
			let name = match self.name()
			{
				Ok(name) => name,
				Err(err2) => return Err(err.merge(err2)),
			};

			let op = match self.punct_equal()
			{
				Ok(_) => "=",
				Err(err) => match self.punct_left_arrow() {
						Ok(_) => "<-",
						Err(err2) => return Err(err.merge(err2)),
					},
			};
			let expr = self.expr()?;

			elems.push(ast::Mutation::new_boxed(name, None, Box::new(op.into()), expr));

			match self.punct_comma()
			{
				Ok(()) => (),
				Err(err) => match self.punct_brace_right() {
					Ok(()) => break,
					Err(err2) => return Err(err.merge(err2)),
				},
			}
		}

		Ok(ast::Expr::new_record_creation_boxed(expr, Box::new(elems)))
	}


	fn prop_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut lhs = self.term_expr(opts)?;
		loop
		{
			let save = &self.save();
			match self.next_token()
			{
				Some(&scanner::Token::Operator(ref op)) => {
					match op.kind()
					{
						scanner::OperatorKind::Dot => {
							match self.term_expr(opts)
							{
								Ok(rhs) => {
									lhs = ast::Expr::new_dotted_boxed(lhs, rhs);
									continue
								},
								Err(_) => {
									self.restore(save);
									break;
								}
							}
						},
						_ => {
							self.restore(save);
							break;
						},
					}
				},

				_ => {
					self.restore(save);
					break;
				}
			}
		}

		return Ok(lhs);
	}


	fn term_expr(&mut self, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let save = self.save();

		let err = match self.number() {
			result@Ok(_) => return result,
			Err(err) => {
				if self.pos != save.pos
				{
					return Err(err)
				}
				err
			},
		};

		self.restore(&save);
		let err = match self.name() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.string() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.paren() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.event_set() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.channel_set() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.target_set() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.list() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.set() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.case_expr() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.let_expr() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.if_expr() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.record_mutation() {
			result@Ok(_) => return result,
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = match self.fn_expr() {
			Ok((expr, _err)) => return Ok(expr),
			Err(err2) => {
				if self.pos != save.pos
				{
					return Err(err2)
				}
				err.merge(err2)
			},
		};

		self.restore(&save);
		let err = if self.keyword(scanner::KeywordKind::Any)
		{
			return Ok(ast::Expr::new_any_boxed());
		}else
		{
			err.merge(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Any(self.pos)))))
		};

		self.restore(&save);
		let err = if self.keyword(scanner::KeywordKind::State)
		{
			return Ok(ast::Expr::new_state_boxed());
		}else
		{
			err.merge(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::State(self.pos)))))
		};

		self.restore(&save);
		let err = if self.keyword(scanner::KeywordKind::Exists)
		{
			return self.quantifier_expr(ast::QuantifierKind::Exists, &opts);
		}else
		{
			err.merge(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Exists(self.pos)))))
		};

		self.restore(&save);
		let err = if self.keyword(scanner::KeywordKind::Exists1)
		{
			return self.quantifier_expr(ast::QuantifierKind::Exists1, &opts);
		}else
		{
			err.merge(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Exists1(self.pos)))))
		};

		self.restore(&save);
		let err = if self.keyword(scanner::KeywordKind::ForAll)
		{
			return self.quantifier_expr(ast::QuantifierKind::ForAll, &opts);
		}else
		{
			err.merge(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ForAll(self.pos)))))
		};

		Err(err)
	}


	fn next_token(&mut self) -> Option<&scanner::Token>
	{
		match self.tokens.get(self.pos)
		{
			Some(tok) => {
				self.pos += 1;
				Some(&*tok)
			},
			None => {
				if self.pos == self.tokens.len()
				{
					self.pos += 1;
					None
				}else
				{
					panic!("next_token() exceeds eof")
				}
			},
		}
	}


	fn name(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let save = self.save();
		match self.next_token()
		{
			Some(&scanner::Token::Identifier(ref name)) =>
				Ok(ast::Expr::new_name_boxed(name.text().clone())),

			_ => {
				self.pos -= 1;
				Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Name(save.pos)))))
			},
		}
	}


	fn name_string(&mut self) -> Result<String, Box<Error>>
	{
		match self.next_token()
		{
			Some(&scanner::Token::Identifier(ref name)) =>
				Ok(name.text().clone()),

			_ => {
				self.pos -= 1;
				Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Name(self.pos)))))
			},
		}
	}


	fn number(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		match self.next_token()
		{
			Some(&scanner::Token::Number(ref num)) =>
				Ok(ast::Expr::new_num_boxed(num.value())),

			_ => {
				self.pos -= 1;
				Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Number(self.pos)))))
			},
		}
	}


	fn string(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let save = self.save();
		match self.next_token()
		{
			Some(&scanner::Token::TokString(ref s)) =>
				Ok(ast::Expr::new_str_boxed(s.value().clone(), s.begin().clone(), s.end().clone())),

			_ => {
				self.pos -= 1;
				Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::TokString(save.pos)))))
			},
		}
	}


	fn paren(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		if !self.punct(scanner::PunctuationKind::ParenLeft)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ParenLeft(self.pos)))));
		}

		let expr = self.expr()?;

		let save = self.save();
		if !self.punct(scanner::PunctuationKind::ParenRight)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ParenRight(save.pos)))));
		}

		Ok(ast::Expr::new_paren_boxed(expr))
	}


	fn event_set(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::EventSetPragma)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::EventSetPragma(self.pos)))));
		}

		self.punct_brace_left()?;

		self.event_set_()
	}

	fn event_set_(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		// brace-left is already consumed.

		let mut exprs = Vec::new();
		loop
		{
			if self.punct(scanner::PunctuationKind::BraceRight)
			{
				break;
			}
			let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::BraceRight(self.pos))));

			match self.expr()
			{
				Ok(expr) =>
					exprs.push(expr),
				Err(err2) =>
					return Err(err.merge(err2)),
			}

			if self.punct(scanner::PunctuationKind::Comma)
			{
				// pass.
			}else
			{
				if self.punct(scanner::PunctuationKind::BraceRight)
				{
					break;
				}
				let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Comma(self.pos))));
				let err2 = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::BraceRight(self.pos))));
				return Err(err.merge(err2));
			}
		}

		Ok(ast::Expr::new_event_set_boxed(Box::new(exprs)))
	}


	fn channel_set(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		if !self.punct(scanner::PunctuationKind::ChannelSetLeft)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ParenLeft(self.pos)))));
		}

		let mut exprs = Vec::new();
		loop
		{
			if self.punct(scanner::PunctuationKind::ChannelSetRight)
			{
				break;
			}
			let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ChannelSetRight(self.pos))));

			match self.expr()
			{
				Ok(expr) =>
					exprs.push(expr),
				Err(err2) =>
					return Err(err.merge(err2)),
			}

			if self.punct(scanner::PunctuationKind::Comma)
			{
				// pass.
			}else
			{
				if self.punct(scanner::PunctuationKind::ChannelSetRight)
				{
					break;
				}
				let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Comma(self.pos))));
				let err2 = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ChannelSetRight(self.pos))));
				return Err(err.merge(err2));
			}
		}

		Ok(ast::Expr::new_channel_set_boxed(Box::new(exprs)))
	}


	fn target_set(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		if !self.punct(scanner::PunctuationKind::TargetSetLeft)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::ParenLeft(self.pos)))));
		}

		let mut exprs = Vec::new();
		loop
		{
			if self.punct(scanner::PunctuationKind::TargetSetRight)
			{
				break;
			}
			let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::TargetSetRight(self.pos))));

			match self.target_set_item()
			{
				Ok(expr) =>
					exprs.push(expr),
				Err(err2) =>
					return Err(err.merge(err2)),
			}

			if self.punct(scanner::PunctuationKind::Comma)
			{
				// pass.
			}else
			{
				if self.punct(scanner::PunctuationKind::TargetSetRight)
				{
					break;
				}
				let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Comma(self.pos))));
				let err2 = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::TargetSetRight(self.pos))));
				return Err(err.merge(err2));
			}
		}

		Ok(ast::Expr::new_target_set_boxed(Box::new(exprs)))
	}


	fn target_set_item(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		match self.punct_brace_left()
		{
			Ok(()) => self.event_set_(),
			Err(err) => match self.expr() {
				Ok(expr) => Ok(expr),
				Err(err2) => Err(err.merge(err2)),
			},
		}
	}


	fn list(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		self.punct_bracket_left()?;

		let save = self.save();
		let first = match self.expr() {
			Ok(expr) => expr,
			Err(err) => {
				self.restore(&save);
				match self.punct_bracket_right() {
					Ok(()) => return Ok(ast::Expr::new_nil_boxed()),
					Err(err2) => return Err(err.merge(err2)),
				}
			},
		};

		let err = match self.punct_comma()
		{
			Ok(()) => return self.list_more(first),
			Err(err) => err,
		};
		let err = match self.punct_bracket_right() {
			Ok(()) =>
				return Ok(ast::Expr::new_list_boxed(Box::new(vec![first]))),
			Err(err2) => err.merge(err2),
		};
		match self.punct_vertical_bar()
		{
			Ok(()) => self.list_comprehension(first),
			Err(err2) => Err(err.merge(err2)),
		}
	}


	fn list_more(&mut self, first: Box<ast::Expr>) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut elems = vec![first];
		loop
		{
			let save = self.save();
			let err = match self.punct_bracket_right()
			{
				Ok(()) => break,
				Err(err) => err,
			};

			self.restore(&save);
			let expr = match self.expr()
			{
				Ok(expr) => expr,
				Err(err2) => return Err(err.merge(err2)),
			};
			elems.push(expr);

			match self.punct_comma()
			{
				Ok(()) => (),
				Err(err) => match self.punct_bracket_right() {
						Ok(()) => break,
						Err(err2) => return Err(err.merge(err2)),
					},
			}
		}

		Ok(ast::Expr::new_list_boxed(Box::new(elems)))
	}


	fn list_comprehension(&mut self, out_expr: Box<ast::Expr>) -> Result<Box<ast::Expr>, Box<Error>>
	{
		// sep_end_by1
		let mut elems = vec![self.expr()?];

		loop
		{
			let err = match self.punct_comma_or_ampersand()
			{
				Ok(()) => match self.punct_bracket_right() {
					Ok(()) => break,
					Err(err) => err,
				},
				Err(err) => match self.punct_bracket_right() {
					Ok(()) => break,
					Err(err2) => return Err(err.merge(err2)),
				},
			};

			let expr = match self.expr() {
				Ok(expr) => expr,
				Err(err2) => return Err(err.merge(err2)),
			};
			elems.push(expr);
		}

		Ok(ast::Expr::new_list_comprehension_boxed(out_expr, Box::new(elems)))
	}

	fn punct_comma_or_ampersand(&mut self) -> Result<(), Box<Error>>
	{
		match self.punct_comma()
		{
			Ok(()) => Ok(()),
			Err(err) => match self.punct_ampersand() {
				Ok(()) => Ok(()),
				Err(err2) => Err(err.merge(err2)),
			},
		}
	}


	fn set(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		self.keyword_set_pragma()?;
		self.punct_brace_left()?;

		let save = self.save();
		let first = match self.expr() {
			Ok(expr) => expr,
			Err(err) => {
				self.restore(&save);
				match self.punct_brace_right() {
					Ok(()) => return Ok(ast::Expr::new_set_boxed(Box::new(Vec::new()))),
					Err(err2) => return Err(err.merge(err2)),
				}
			},
		};

		let err = match self.punct_comma()
		{
			Ok(()) => return self.set_more(first),
			Err(err) => err,
		};
		let err = match self.punct_brace_right() {
			Ok(()) =>
				return Ok(ast::Expr::new_set_boxed(Box::new(vec![first]))),
			Err(err2) => err.merge(err2),
		};
		Err(err)
	}


	fn keyword_set_pragma(&mut self) -> Result<(), Box<Error>>
	{
		if self.keyword(scanner::KeywordKind::SetPragma)
		{
			Ok(())
		}else
		{
			Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::SetPragma(self.pos)))))
		}
	}


	fn set_more(&mut self, first: Box<ast::Expr>) -> Result<Box<ast::Expr>, Box<Error>>
	{
		let mut elems = vec![first];
		loop
		{
			let save = self.save();
			let err = match self.punct_brace_right()
			{
				Ok(()) => break,
				Err(err) => err,
			};

			self.restore(&save);
			let expr = match self.expr()
			{
				Ok(expr) => expr,
				Err(err2) => return Err(err.merge(err2)),
			};
			elems.push(expr);

			match self.punct_comma()
			{
				Ok(()) => (),
				Err(err) => match self.punct_brace_right() {
						Ok(()) => break,
						Err(err2) => return Err(err.merge(err2)),
					},
			}
		}

		Ok(ast::Expr::new_set_boxed(Box::new(elems)))
	}


	fn case_expr(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::Case)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Case(self.pos)))));
		}

		let expr = {
			let opts = ExprOpts::followed_by_block();
			self.expr_(&opts)?
		};
		let mut elems = Vec::new();

		self.punct_brace_left()?;
		loop
		{
			let save = self.save();

			let err = match self.punct_brace_right()
			{
				Ok(()) => break,
				Err(err) => err,
			};

			self.restore(&save);
			let mut opts = ExprOpts::standard();
			opts.imply_op = false;
			let matcher = match self.expr_(&opts)
			{
				Ok(expr) => expr,
				Err(err2) => return Err(err.merge(err2)),
			};

			let save = self.save();
			let guard = if self.keyword(scanner::KeywordKind::When) {
				Ok(self.expr()?)
			}else
			{
				self.restore(&save);
				Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::When(save.pos)))))
			};

			match self.punct_double_arrow()
			{
				Ok(()) => (),
				Err(err2) => match guard {
					Ok(_) => return Err(err2),
					Err(err) => return Err(err.merge(err2)),
				},
			}

			let body = self.expr()?;

			self.punct_semi_colon()?;

			elems.push(ast::CaseElem::new_boxed(matcher, guard.ok(), body));
		}

		Ok(ast::Expr::new_case_boxed(expr, Box::new(elems)))
	}


	fn if_expr(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::If)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::If(self.pos)))));
		}

		let cond = {
			let opts = ExprOpts::followed_by_block();
			self.expr_(&opts)?
		};

		self.punct_brace_left()?;
		let then_expr = self.expr()?;
		self.punct_brace_right()?;

		let else_expr = if self.keyword(scanner::KeywordKind::Else)
		{
			self.punct_brace_left()?;
			let else_expr = self.expr()?;
			self.punct_brace_right()?;
			Some(else_expr)
		}else
		{
			None
		};

		Ok(ast::Expr::new_if_boxed(cond, then_expr, else_expr))
	}


	fn let_expr(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::Let)
		{
			return Err(Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Let(self.pos)))));
		}

		let mut opts = ExprOpts::standard();
		opts.in_op = false;

		let mut mutations = vec![self.mutation(&opts)?];

		loop
		{
			match self.punct_comma()
			{
				Ok(()) => (),
				Err(err) =>
					if self.keyword(scanner::KeywordKind::In)
					{
						break;
					}else
					{
						let err2 = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::In(self.pos))));
						return Err(err.merge(err2));
					}
			}

			if self.keyword(scanner::KeywordKind::In)
			{
				break;
			}
			let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::In(self.pos))));

			match self.mutation(&opts)
			{
				Ok(expr) => mutations.push(expr),
				Err(err2) => return Err(err.merge(err2)),
			}
		}

		let expr = self.expr()?;

		Ok(ast::Expr::new_let_boxed(Box::new(mutations), expr))
	}


	fn mutation(&mut self, opts: &ExprOpts) -> Result<Box<ast::Mutation>, Box<Error>>
	{
		let lhs = self.expr()?;

		let ltype = match self.punct_colon() {
			Ok(()) => Ok(self.expr()?),
			Err(err) => Err(err),
		};

		match self.punct_equal()
		{
			Ok(()) => (),
			Err(err2) => match ltype {
				Ok(_) => return Err(err2),
				Err(err) => return Err(err.merge(err2)),
			},
		}

		let rhs = self.expr_(&opts)?;

		Ok(ast::Mutation::new_boxed(lhs, ltype.ok(), Box::new("=".into()), rhs))
	}


	fn fn_expr(&mut self) -> Result<(Box<ast::Expr>, Option<Box<Error>>), Box<Error>>
	{
		if !self.keyword(scanner::KeywordKind::Fn)
		{
			let err = Box::new(Error::NotFound(NotFound::singleton_boxed(NotFound::Fn(self.pos))));
			return Err(err);
		}

		let (args, err) = self.arg_list_opt()?;

		let (typ, err) = match self.punct_colon()
		{
			Ok(()) => (Some(self.expr_(&ExprOpts::no_prefix_op())?), None),
			Err(err2) => (None, Some(err.merge(err2))),
		};

		let (expr, err) = match self.punct_arrow()
		{
			Ok(()) => (Some(self.expr()?), None),
			Err(err2) => match err {
				Some(err) => (None, Some(err.merge(err2))),
				None => (None, Some(err2)),
			},
		};

		Ok((ast::Expr::new_fn_boxed(args, typ, expr), err))
	}


	fn record_mutation(&mut self) -> Result<Box<ast::Expr>, Box<Error>>
	{
		self.punct_brace_left()?;
		let expr = self.expr()?;
		self.punct_vertical_bar()?;

		let mut elems = Vec::new();

		loop
		{
			let save = self.save();
			let err = match self.punct_brace_right()
			{
				Ok(()) => break,
				Err(err) => err,
			};

			self.restore(&save);
			let name = match self.name()
			{
				Ok(name) => name,
				Err(err2) => return Err(err.merge(err2)),
			};

			self.punct_left_arrow()?;
			let expr = self.expr()?;

			elems.push(ast::Mutation::new_boxed(name, None, Box::new("<-".into()), expr));

			match self.punct_comma()
			{
				Ok(()) => (),
				Err(err) => {
				match self.punct_brace_right() {
					Ok(()) => break,
					Err(err2) => return Err(err.merge(err2)),
				}},
			}
		}

		Ok(ast::Expr::new_record_mutation_boxed(ast::RecordStyle::Inside, expr, Box::new(elems)))
	}


	fn quantifier_expr(&mut self, kind: ast::QuantifierKind, opts: &ExprOpts) -> Result<Box<ast::Expr>, Box<Error>>
	{
		// sep_end_by1
		let mut exprs = vec![self.expr()?];

		loop
		{
			let err = match self.punct_comma()
			{
				Ok(()) => match self.punct_quantifier_sep() {
					Ok(()) => break,
					Err(err) => err,
				},
				Err(err) => match self.punct_quantifier_sep() {
					Ok(()) => break,
					Err(err2) => return Err(err.merge(err2)),
				},
			};

			let expr = match self.expr() {
				Ok(expr) => expr,
				Err(err2) => return Err(err.merge(err2)),
			};
			exprs.push(expr);
		}

		let cond = self.expr_(opts)?;
		Ok(ast::Expr::new_quantifier_boxed(kind, Box::new(exprs), cond))
	}

	fn punct_quantifier_sep(&mut self) -> Result<(), Box<Error>>
	{
		let err = match self.punct_vertical_bar()
		{
			Ok(()) => return Ok(()),
			Err(err) => err,
		};
		let err = match self.punct_ampersand() {
			Ok(()) => return Ok(()),
			Err(err2) => err.merge(err2),
		};
		Err(err)
	}


	fn at_summary_opt(&mut self) -> Box<Option<String>>
	{
		let save = self.save();
		match self.next_token()
		{
			Some(&scanner::Token::AtSummary(ref value)) =>
				Box::new(Some(value.value().clone())),

			_ => {
				self.restore(&save);
				Box::new(None)
			},
		}
	}


	fn at_short_description_opt(&mut self) -> Box<Option<String>>
	{
		let save = self.save();
		match self.next_token()
		{
			Some(&scanner::Token::AtShortDescription(ref value)) =>
				Box::new(Some(value.value().clone())),

			_ => {
				self.restore(&save);
				Box::new(None)
			},
		}
	}


	fn at_long_description_opt(&mut self) -> Box<Option<String>>
	{
		let save = self.save();
		match self.next_token()
		{
			Some(&scanner::Token::AtLongDescription(ref value)) =>
				Box::new(Some(value.value().clone())),

			_ => {
				self.restore(&save);
				Box::new(None)
			},
		}
	}


	fn at_line_description_opt(&mut self) -> Box<Option<String>>
	{
		let save = self.save();
		match self.next_token()
		{
			Some(&scanner::Token::AtLineDescription(ref value)) =>
				Box::new(Some(value.value().clone())),

			_ => {
				self.restore(&save);
				Box::new(None)
			},
		}
	}
}
