//! # Tokenizer for Rusk
use std::convert::TryFrom; // try_from.


type Offset = usize;


#[derive(Debug)]
pub struct Tokenizer
{
	input: Vec<char>,
	ix: Offset,
	tokens: Vec<Box<Token>>,
}


trait HasOffset
{
	fn offset(&self) -> Offset;
}


#[derive(Debug)]
pub enum Error
{
	BadChar(char, Offset),
	Overflow(Offset),
	BadNumTrailer(char, Offset),
	Unterminated(Offset),
	EndOfFile(Offset),
}


impl HasOffset for Error
{
	fn offset(&self) -> Offset
	{
		match self
		{
			Error::BadChar(_, offset) => *offset,
			Error::Overflow(offset) => *offset,
			Error::BadNumTrailer(_, offset) => *offset,
			Error::Unterminated(offset) => *offset,
			Error::EndOfFile(offset) => *offset,
		}
	}
}


#[derive(Debug)]
pub struct Position
{
	line_num: usize,
	col_num: usize,
}


impl Position
{
	pub fn line_num(&self) -> usize
	{
		self.line_num
	}


	pub fn col_num(&self) -> usize
	{
		self.col_num
	}
}


impl std::fmt::Display for Position
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "line {} column {}", self.line_num(), self.col_num())
	}
}


#[derive(Debug)]
pub struct SourceMap
{
	input: Vec<char>,
}


impl SourceMap
{
	pub fn error_position(&self, err: &Error) -> Position
	{
		Self::position_(&self.input, err.offset())
	}


	pub fn position(&self, item: &Token) -> Position
	{
		Self::position_(&self.input, item.offset())
	}


	fn position_(input: &Vec<char>, offset: Offset) -> Position
	{
		let mut line_num = 1;
		let mut col_num = 1;
		for i in 0..offset
		{
			if input[i] == '\n'
			{
				line_num += 1;
				col_num = 1;
			}else
			{
				col_num += 1;
			}
		}
		Position { line_num, col_num, }
	}
}


#[derive(Debug)]
pub enum Token
{
	Identifier(Box<Identifier>),
	Keyword(Box<Keyword>),
	Operator(Box<Operator>),
	Punctuation(Box<Punctuation>),
	PunctOper(Box<PunctOper>),
	Number(Box<Number>),
	TokString(Box<TokString>),
	AtSummary(Box<TokString>),
	AtShortDescription(Box<TokString>),
	AtLongDescription(Box<TokString>),
	AtLineDescription(Box<TokString>),
}


impl HasOffset for Token
{
	fn offset(&self) -> Offset
	{
		match self
		{
			Token::Identifier(identifier) => identifier.offset,
			Token::Keyword(kw) => kw.offset,
			Token::Operator(op) => op.offset,
			Token::Punctuation(punct) => punct.offset,
			Token::PunctOper(punct) => punct.offset,
			Token::Number(number) => number.offset,
			Token::TokString(string) => string.offset,
			Token::AtSummary(string) => string.offset,
			Token::AtShortDescription(string) => string.offset,
			Token::AtLongDescription(string) => string.offset,
			Token::AtLineDescription(string) => string.offset,
		}
	}
}


#[derive(Debug)]
pub struct Identifier
{
	text: String,
	offset: Offset,
}


impl Identifier
{
	pub fn text(&self) -> &String
	{
		&self.text
	}
}


#[derive(Debug)]
pub struct Keyword
{
	kind: KeywordKind,
	offset: Offset,
}


impl Keyword
{
	pub fn kind(&self) -> KeywordKind
	{
		self.kind
	}


	pub fn to_code(&self) -> &'static str
	{
		self.kind.to_code()
	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeywordKind
{
	Any,
	Case,
	Const,
	Else,
	Event,
	Exists,
	Exists1,
	Fn,
	ForAll,
	If,
	In,
	Invariant,
	Let,
	Post,
	State,
	Target,
	Transition,
	Type,
	Var,
	When,

	EventSetPragma,
	MapPragma,
	SetPragma,
}


impl KeywordKind
{
	fn to_code(&self) -> &'static str
	{
		match self
		{
			KeywordKind::Any        => "any",
			KeywordKind::Case       => "case",
			KeywordKind::Const      => "const",
			KeywordKind::Else       => "else",
			KeywordKind::Event      => "event",
			KeywordKind::Exists     => "exists",
			KeywordKind::Exists1    => "exists1",
			KeywordKind::Fn         => "fn",
			KeywordKind::ForAll     => "forall",
			KeywordKind::If         => "if",
			KeywordKind::In         => "in",
			KeywordKind::Invariant  => "invariant",
			KeywordKind::Let        => "let",
			KeywordKind::Post       => "post",
			KeywordKind::State      => "state",
			KeywordKind::Target     => "target",
			KeywordKind::Transition => "transition",
			KeywordKind::Type       => "type",
			KeywordKind::Var        => "var",
			KeywordKind::When       => "when",

			KeywordKind::EventSetPragma => "__event_set",
			KeywordKind::MapPragma => "__map",
			KeywordKind::SetPragma => "__set",
		}
	}
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operator
{
	kind: OperatorKind,
	offset: Offset,
}


impl Operator
{
	pub fn kind(&self) -> &OperatorKind
	{
		&self.kind
	}


	pub fn to_code(&self) -> &str
	{
		self.kind.to_code()
	}
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OperatorKind
{
	LogiOr,
	LogiAnd,
	CmpEq,
	CmpNe,
	CmpLt,
	CmpLe,
	CmpGt,
	CmpGe,
	UserDefined(Box<UserDefined>),
	Concat,
	Add,
	Sub,
	Mul,
	Div,
	Dot,
}


impl OperatorKind
{
	pub fn to_code(&self) -> &str
	{
		match self
		{
		OperatorKind::LogiOr => "||",
		OperatorKind::LogiAnd => "&&",
		OperatorKind::CmpEq => "==",
		OperatorKind::CmpNe => "/=",
		OperatorKind::CmpLt => "<",
		OperatorKind::CmpLe => "<=",
		OperatorKind::CmpGt => ">",
		OperatorKind::CmpGe => ">=",
		OperatorKind::UserDefined(x) => x.text.as_ref(),
		OperatorKind::Concat => "++",
		OperatorKind::Add => "+",
		OperatorKind::Sub => "-",
		OperatorKind::Mul => "*",
		OperatorKind::Div => "/",
		OperatorKind::Dot => ".",
		}
	}
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserDefined
{
	text: String,
}


impl UserDefined
{
	pub fn text(&self) -> &String
	{
		&self.text
	}
}


#[derive(Debug)]
pub struct Punctuation
{
	kind: PunctuationKind,
	offset: Offset,
}


impl Punctuation
{
	pub fn kind(&self) -> PunctuationKind
	{
		self.kind
	}


	pub fn to_code(&self) -> &'static str
	{
		self.kind.to_code()
	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PunctuationKind
{
	Backslash,
	BraceLeft,
	BraceRight,
	BracketLeft,
	BracketRight,
	ChannelSetLeft,
	ChannelSetRight,
	Comma,
	Equal,
	LeftArrow,
	Ampersand,
	ParenLeft,
	ParenRight,
	SemiColon,
	TargetSetLeft,
	TargetSetRight,
	TransitionArrow,
	VerticalBar,
	MapsTo,
}


impl PunctuationKind
{
	fn to_code(&self) -> &'static str
	{
		match self
		{
			PunctuationKind::Backslash        => "/",
			PunctuationKind::BraceLeft        => "{",
			PunctuationKind::BraceRight       => "}",
			PunctuationKind::BracketLeft      => "[",
			PunctuationKind::BracketRight     => "]",
			PunctuationKind::ChannelSetLeft   => "{|",
			PunctuationKind::ChannelSetRight  => "|}",
			PunctuationKind::Comma            => ",",
			PunctuationKind::Equal            => "=",
			PunctuationKind::LeftArrow        => "<-",
			PunctuationKind::Ampersand        => "&",
			PunctuationKind::ParenLeft        => "(",
			PunctuationKind::ParenRight       => ")",
			PunctuationKind::SemiColon        => ";",
			PunctuationKind::TargetSetLeft    => "|[",
			PunctuationKind::TargetSetRight   => "]|",
			PunctuationKind::TransitionArrow  => "-->",
			PunctuationKind::VerticalBar      => "|",
			PunctuationKind::MapsTo      => "|->",
		}
	}
}


#[derive(Debug)]
pub struct PunctOper
{
	kind: PunctOperKind,
	offset: Offset,
}


impl PunctOper
{
	pub fn kind(&self) -> PunctOperKind
	{
		self.kind
	}


	pub fn to_code(&self) -> &'static str
	{
		self.kind.to_code()
	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PunctOperKind
{
	Arrow,
	Colon,
	DoubleArrow,
}


impl PunctOperKind
{
	fn to_code(&self) -> &'static str
	{
		match self
		{
			PunctOperKind::Arrow       => "->",
			PunctOperKind::Colon       => ":",
			PunctOperKind::DoubleArrow => "=>",
		}
	}
}


#[derive(Debug)]
pub struct Number
{
	value: usize,
	offset: Offset,
}


impl Number
{
	pub fn value(&self) -> usize
	{
		self.value
	}
}


#[derive(Debug)]
pub struct TokString
{
	begin: String,
	value: String,
	end: String,
	offset: Offset,
}


impl TokString
{
	pub fn value(&self) -> &String
	{
		&self.value
	}

	pub fn begin(&self) -> &String
	{
		&self.begin
	}

	pub fn end(&self) -> &String
	{
		&self.end
	}
}


pub fn scan(input: String) -> Result<Box<Tokenizer>, (Box<SourceMap>, Box<Error>)>
{
	Tokenizer::scan(input)
}


impl Tokenizer
{
	fn scan(input: String) -> Result<Box<Tokenizer>, (Box<SourceMap>, Box<Error>)>
	{
		let mut me = Box::new(Tokenizer {
			input: input.chars().collect::<Vec<char>>(),
			ix: 0,
			tokens: Vec::new(),
		});

		match me.enter()
		{
			Ok(()) => Ok(me),
			Err(err) => Err((Box::new(SourceMap { input: me.input }), err)),
		}
	}


	pub fn tokens(&self) -> &Vec<Box<Token>>
	{
		&self.tokens
	}


	pub fn results(self: Box<Tokenizer>) -> (Box<SourceMap>, Vec<Box<Token>>)
	{
		(Box::new(SourceMap { input: self.input}), self.tokens)
	}


	fn next(&mut self) -> Option<char>
	{
		match self.input.get(self.ix)
		{
			Some(ch) => {
				self.ix += 1;
				Some(*ch)
			},
			None =>
				None
		}
	}


	fn unget(&mut self)
	{
		self.ix -= 1;
	}


	fn unget_ix(&mut self) -> Offset
	{
		self.ix -= 1;
		self.ix
	}


	fn is_identifier_start(ch: char) -> bool
	{
		ch.is_alphabetic() || ch == '_' || ch == '＿'
	}


	fn is_identifier_trailer(ch: char) -> bool
	{
		ch.is_alphanumeric() || ch == '_' || ch == '＿'
	}


	fn is_operator_char(ch: char) -> bool
	{
		match ch
		{
			'!' => true,
			'$' => true,
			'%' => true,
			'&' => true,
			'*' => true,
			'+' => true,
			'-' => true,
			'.' => true,
			'/' => true,
			':' => true,
			'<' => true,
			'=' => true,
			'>' => true,
			'?' => true,
			'^' => true,
			'|' => true,
			'~' => true,
			_   => false,
		}
	}


	fn enter(&mut self) -> Result<(), Box<Error>>
	{
		while let Some(ch) = self.next()
		{
			match ch
			{
				'\t' => continue,
				'\n' => continue,
				'\r' => continue,
				' '  => continue,

				'!' => self.operator(ch)?,
				'"' => self.string(ch)?,
				'#' => Err(Error::BadChar(ch, self.unget_ix()))?,
				'$' => self.operator(ch)?,
				'%' => self.operator(ch)?,
				'&' => self.operator(ch)?,
				'\'' => self.string(ch)?,
				'(' => self.punctuation(PunctuationKind::ParenLeft, self.ix - 1),
				')' => self.punctuation(PunctuationKind::ParenRight, self.ix - 1),
				'*' => self.operator(ch)?,
				'+' => self.operator(ch)?,
				',' => self.punctuation(PunctuationKind::Comma, self.ix - 1),
				'-' => self.operator(ch)?,

				'.' => self.operator(ch)?,
				'/' => self.slash()?,
				'0' => self.number(0)?,
				'1' => self.number(1)?,
				'2' => self.number(2)?,
				'3' => self.number(3)?,
				'4' => self.number(4)?,
				'5' => self.number(5)?,
				'6' => self.number(6)?,
				'7' => self.number(7)?,
				'8' => self.number(8)?,
				'9' => self.number(9)?,
				':' => self.operator(ch)?,
				';' => self.punctuation(PunctuationKind::SemiColon, self.ix - 1),
				'<' => self.operator(ch)?,
				'=' => self.operator(ch)?,
				'>' => self.operator(ch)?,
				'?' => self.operator(ch)?,

				'@' => self.at()?,
				'A' ..= 'Z' => self.identifier(ch)?,
				'[' => self.punctuation(PunctuationKind::BracketLeft, self.ix - 1),
				'\\' => self.punctuation(PunctuationKind::Backslash, self.ix - 1),
				']' => match self.next() {
					Some('|') => {
						self.punctuation(PunctuationKind::TargetSetRight, self.ix - 2)
					},
					Some(_) => {
						self.ix -= 1;
						self.punctuation(PunctuationKind::BracketRight, self.ix - 1)
					},
					None =>
						self.punctuation(PunctuationKind::BracketRight, self.ix - 1),
				},
				'^' => self.operator(ch)?,
				'_' => self.identifier(ch)?,
				'`' => self.string(ch)?,
				'a' ..= 'z' => self.identifier(ch)?,
				'{' => match self.next() {
					Some('|') => {
						self.punctuation(PunctuationKind::ChannelSetLeft, self.ix - 2)
					},
					Some(_) => {
						self.ix -= 1;
						self.punctuation(PunctuationKind::BraceLeft, self.ix - 1)
					},
					None =>
						self.punctuation(PunctuationKind::BraceLeft, self.ix - 1),
				},
				'|' => match self.next() {
					Some('[') =>
						self.punctuation(PunctuationKind::TargetSetLeft, self.ix - 2),
					Some('}') =>
						self.punctuation(PunctuationKind::ChannelSetRight, self.ix - 2),
					Some(_) => {
						self.ix -= 1;
						self.operator(ch)?
					},
					None =>
						self.operator(ch)?,
				},
				'}' => self.punctuation(PunctuationKind::BraceRight, self.ix - 1),
				'~' => self.operator(ch)?,

				ch if ch.is_whitespace() => continue,
				ch if Self::is_identifier_start(ch) =>
					self.identifier(ch)?,

				ch => Err(Error::BadChar(ch, self.unget_ix()))?,
			}
		}
		Ok(())
	}


	fn identifier(&mut self, ch0: char) -> Result<(), Error>
	{
		let offset = self.ix - 1;
		let mut text = String::new();
		text.push(ch0);

		while let Some(ch) = self.next()
		{
			if Self::is_identifier_trailer(ch)
			{
				text.push(ch);
				continue;
			}
			if ch != '\''
			{
				self.unget();
				break;
			}

			text.push('\'');
			if let Some(ch) = self.next()
			{
				if Self::is_identifier_trailer(ch)
				{
					self.unget();
					return Err(Error::BadChar(ch, self.unget_ix()));
				}
			}
			self.unget();
			break;
		}

		let kind = match text.as_ref() {
			"any" =>         KeywordKind::Any,
			"case" =>        KeywordKind::Case,
			"const" =>       KeywordKind::Const,         
			"else" =>        KeywordKind::Else,
			"event" =>       KeywordKind::Event,
			"exists" =>      KeywordKind::Exists,
			"exists1" =>     KeywordKind::Exists1,
			"fn" =>          KeywordKind::Fn,
			"forall" =>      KeywordKind::ForAll,
			"if" =>          KeywordKind::If,
			"in" =>          KeywordKind::In,
			"invariant" =>   KeywordKind::Invariant,
			"let" =>         KeywordKind::Let,
			"post" =>        KeywordKind::Post,
			"state" =>       KeywordKind::State,
			"target" =>      KeywordKind::Target,
			"transition" =>  KeywordKind::Transition,
			"type" =>        KeywordKind::Type,
			"var" =>         KeywordKind::Var,
			"when" =>        KeywordKind::When,
			"__event_set" => KeywordKind::EventSetPragma,
			"__map" =>       KeywordKind::MapPragma,
			"__set" =>       KeywordKind::SetPragma,
			_ => {
				self.tokens.push(
					Box::new(Token::Identifier(Box::new(Identifier { text, offset })))
				);
				return Ok(());
			},
		};

		self.tokens.push(
			Box::new(Token::Keyword(Box::new(Keyword { kind, offset })))
		);
		Ok(())
	}


	fn slash(&mut self) -> Result<(), Error>
	{
		match self.next()
		{
			Some('/') => {
				while let Some(ch) = self.next()
				{
					if ch == '\n'
					{
						break;
					}
				}
				Ok(())
			},
			Some('*') => {
				let offset = self.ix;
				loop
				{
					match self.next()
					{
						Some('*') => (),
						Some(_ch) => continue,
						None =>
							return Err(Error::Unterminated(offset)),
					}
					match self.next()
					{
						Some('/') => break,
						Some(_ch) => continue,
						None =>
							return Err(Error::Unterminated(offset)),
					}
				}
				Ok(())
			},
			_ => {
				self.unget();
				self.operator('/')
			},
		}
	}

	fn operator(&mut self, ch0: char) -> Result<(), Error>
	{
		let offset = self.ix - 1;
		let mut text = String::new();
		text.push(ch0);

		while let Some(ch) = self.next()
		{
			if Self::is_operator_char(ch)
			{
				text.push(ch);
				continue;
			}
			self.unget();
			break;
		}

		match text.as_ref()
		{
			"&" => {
				self.tokens.push(
					Box::new(Token::Punctuation(Box::new(Punctuation { kind: PunctuationKind::Ampersand, offset }))),
				);
			},
			":" => {
				self.tokens.push(
					Box::new(Token::PunctOper(Box::new(PunctOper { kind: PunctOperKind::Colon, offset }))),
				);
			},
			"-->" => {
				self.tokens.push(
					Box::new(Token::Punctuation(Box::new(Punctuation { kind: PunctuationKind::TransitionArrow, offset }))),
				);
			},
			"->" => {
				self.tokens.push(
					Box::new(Token::PunctOper(Box::new(PunctOper { kind: PunctOperKind::Arrow, offset }))),
				);
			},
			"<-" => {
				self.tokens.push(
					Box::new(Token::Punctuation(Box::new(Punctuation { kind: PunctuationKind::LeftArrow, offset }))),
				);
			},
			"=" => {
				self.tokens.push(
					Box::new(Token::Punctuation(Box::new(Punctuation { kind: PunctuationKind::Equal, offset }))),
				);
			},
			"=>" => {
				self.tokens.push(
					Box::new(Token::PunctOper(Box::new(PunctOper { kind: PunctOperKind::DoubleArrow, offset }))),
				);
			},
			"|" => {
				self.tokens.push(
					Box::new(Token::Punctuation(Box::new(Punctuation { kind: PunctuationKind::VerticalBar, offset }))),
				);
			},
			"|->" => {
				self.tokens.push(
					Box::new(Token::Punctuation(Box::new(Punctuation { kind: PunctuationKind::MapsTo, offset }))),
				);
			},

			"||" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::LogiOr, offset }))),
				);
			},
			"&&" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::LogiAnd, offset }))),
				);
			},
			"==" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::CmpEq, offset }))),
				);
			},
			"/=" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::CmpNe, offset }))),
				);
			},
			"<" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::CmpLt, offset }))),
				);
			},
			"<=" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::CmpLe, offset }))),
				);
			},
			">" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::CmpGt, offset }))),
				);
			},
			">=" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::CmpGe, offset }))),
				);
			},

			"++" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::Concat, offset }))),
				);
			},
			"+" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::Add, offset }))),
				);
			},
			"-" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::Sub, offset }))),
				);
			},
			"*" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::Mul, offset }))),
				);
			},
			"/" => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::Div, offset }))),
				);
			},
			"." => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::Dot, offset }))),
				);
			},

			_ => {
				self.tokens.push(
					Box::new(Token::Operator(Box::new(Operator { kind: OperatorKind::UserDefined(
						Box::new(UserDefined { text })
					), offset }))),
				);
			},
		}
		Ok(())
	}


	fn punctuation(&mut self, kind: PunctuationKind, offset: Offset) -> ()
	{
		self.tokens.push(
			Box::new(Token::Punctuation(Box::new(Punctuation { kind, offset }))),
		);
	}


	fn number(&mut self, mut value: usize) -> Result<(), Error>
	{
		let offset = self.ix - 1;

		while let Some(ch) = self.next()
		{
			if let Some(n) = ch.to_digit(10)
			{
				value = match value.checked_mul(10) {
					Some(x) => x,
					None => return Err(Error::Overflow(self.unget_ix())),
				};
				value = match value.checked_add(usize::try_from(n).unwrap()) {
					Some(x) => x,
					None => return Err(Error::Overflow(self.unget_ix())),
				};
				continue;
			}

			if Self::is_identifier_start(ch)
			{
				return Err(Error::BadNumTrailer(ch, self.unget_ix()));
			}
			self.unget();
			break;
		}

		self.tokens.push(Box::new(Token::Number(Box::new(Number { value, offset }))));
		Ok(())
	}


	fn string(&mut self, quote: char) -> Result<(), Error>
	{
		let offset = self.ix - 1;
		let mut s = String::new();
		loop
		{
			match self.next()
			{
				Some('\\') => {
					match self.next()
					{
						Some(ch) =>
							s.push(ch),
						None =>
							return Err(Error::BadChar('\\', self.unget_ix())),
					}
				},
				Some(ch) if ch == quote => {
					let mut q = String::new();
					q.push(quote);
					self.tokens.push(Box::new(Token::TokString(Box::new(TokString {
						value: s,
						begin: q.clone(),
						end: q,
						offset,
					}))));
					return Ok(());
				},
				Some(ch) =>
					s.push(ch),
				None =>
					break,
			}
		}
		Err(Error::EndOfFile(self.ix))
	}


	fn at(&mut self) -> Result<(), Error>
	{
		while let Some(ch) = self.input.get(self.ix)
		{
			if !ch.is_whitespace()
			{
				break;
			}
			self.ix += 1;
		}

		let ch1 = self.next();
		if ch1 == Some('(')
		{
			if self.input.get(self.ix) == Some(&'-')
			{
				self.ix += 1;
				match self.paren_text_2('(', ')')
				{
					Ok(s) => {
						self.tokens.push(Box::new(Token::AtSummary(s)));
						return Ok(());
					},
					Err(err) =>
						return Err(err),
				}
			}else
			{
				match self.paren_text_1('(', ')')
				{
					Ok(s) => {
						self.tokens.push(Box::new(Token::AtSummary(s)));
						return Ok(());
					},
					Err(err) =>
						return Err(err),
				}
			}
		}

		if ch1 == Some('[') && self.input.get(self.ix) == Some(&'-')
		{
			self.ix += 1;
			match self.paren_text_2('[', ']')
			{
				Ok(s) => {
					self.tokens.push(Box::new(Token::AtShortDescription(s)));
					return Ok(());
				},
				Err(err) =>
					return Err(err),
			}
		}

		if ch1 == Some('{') && self.input.get(self.ix) == Some(&'-')
		{
			self.ix += 1;
			match self.paren_text_2('{', '}')
			{
				Ok(s) => {
					self.tokens.push(Box::new(Token::AtLongDescription(s)));
					return Ok(());
				},
				Err(err) => {
					return Err(err);
				},
			}
		}

		if ch1 == Some('/') && self.input.get(self.ix) == Some(&'/')
		{
			self.ix += 1;
			let offset = self.ix;
			let mut s = String::new();
			while let Some(ch) = self.next()
			{
				if ch == '\n'
				{
					break;
				}
				s.push(ch);
			}
			let stok = Box::new(TokString {
				value: s,
				begin: "@//".into(),
				end: String::new(),
				offset,
			});
			self.tokens.push(Box::new(Token::AtLineDescription(stok)));
			return Ok(());
		}

		match ch1
		{
			Some(ch) => 
				Err(Error::BadChar(ch, self.unget_ix())),
			None =>
				Err(Error::EndOfFile(self.ix)),
		}
	}


	fn paren_text_1(&mut self, begin: char, end: char) -> Result<Box<TokString>, Error>
	{
		let offset = self.ix - 1;
		let mut s = String::new();
		loop
		{
			match self.next()
			{
				Some('\\') => {
					if let Some(ch) = self.next()
					{
						s.push(ch);
						continue;
					}
					break;
				},

				Some(ch1) if ch1 == begin =>
					match self.next()
					{
						Some(ch2) if ch2 == begin => {
							s.push(begin);
							continue;
						},
						Some(_) => {
							self.unget();
							self.unget();
							return Err(Error::BadChar(ch1, self.ix));
						},
						None =>
							break,
					},

				Some(ch1) if ch1 == end =>
					match self.next()
					{
						Some(ch2) if ch2 == end => {
							s.push(end);
							continue;
						},
						Some(_) => {
							self.unget();
							let mut q1 = String::new();
							q1.push('@');
							q1.push(begin);
							let mut q2 = String::new();
							q2.push(end);
							return Ok(Box::new(TokString {
								value: s,
								begin: q1,
								end: q2,
								offset,
							}));
						},
						None =>
							break,
					},

				Some(ch) =>
					s.push(ch),
				None =>
					break,
			}
		}
		Err(Error::EndOfFile(self.ix))
	}


	fn paren_text_2(&mut self, begin: char, end: char) -> Result<Box<TokString>, Error>
	{
		let offset = self.ix + 2;
		let mut s = String::new();
		loop
		{
			match self.next()
			{
				Some('\\') => {
					if let Some(ch) = self.next()
					{
						s.push(ch);
						continue;
					}
					break;
				},
				Some('-') =>
					match self.next()
					{
						Some(ch1) if ch1 == end =>
							match self.next()
							{
								Some(ch2) if ch2 == end => {
									s.push('-');
									s.push(end);
									continue;
								},
								Some(_) => {
									self.unget();
									let mut q1 = String::new();
									q1.push('@');
									q1.push(begin);
									q1.push('-');
									let mut q2 = String::new();
									q2.push('-');
									q2.push(end);
									return Ok(Box::new(TokString {
										value: s,
										begin: q1,
										end: q2,
										offset,
									}));
								},
								None => {
									let mut q1 = String::new();
									q1.push('@');
									q1.push(begin);
									q1.push('-');
									let mut q2 = String::new();
									q2.push('-');
									q2.push(end);
									return Ok(Box::new(TokString {
										value: s,
										begin: q1,
										end: q2,
										offset,
									}));
								},
							},
						Some(ch) => {
							s.push('-');
							s.push(ch);
							continue;
						},
						None =>
							break,
					},

				Some(ch1) if ch1 == begin =>
					match self.next()
					{
						Some(ch2) if ch2 == begin => {
							s.push(begin);
							continue;
						},
						Some(_) => {
							self.unget();
							self.unget();
							return Err(Error::BadChar(ch1, self.ix));
						},
						None =>
							break,
					},

				Some(ch1) if ch1 == end =>
					match self.next()
					{
						Some(ch2) if ch2 == end => {
							s.push(end);
							continue;
						},
						Some(_) => {
							self.unget();
							self.unget();
							return Err(Error::BadChar(ch1, self.ix));
						},
						None =>
							break,
					},

				Some(ch) =>
					s.push(ch),
				None =>
					break,
			}
		}
		Err(Error::EndOfFile(self.ix))
	}
}
