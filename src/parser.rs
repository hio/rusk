use combine::{any, eof, Parser};
use combine::parser::char::{alpha_num, char, digit, letter, newline, space, string};
use combine::parser::choice::{choice, optional};
use combine::parser::combinator::{attempt, not_followed_by};
use combine::parser::repeat::{chainl1, many, many1, sep_end_by, sep_end_by1, skip_many, take_until};
use combine::parser::token::{one_of, value};


pub fn parser<Input>() -> impl Parser<Input, Output = Box<super::node::Module>>
	where
		Input: combine::Stream<Token = char>,
		// Necessary due to rust-lang/rust#24159
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip_many(xspace())
		.with(module())
		.skip(skip_many(xspace()))
		.skip(eof())
		.expected("xspace")
}


pub fn skip<Input, P>(p: P) -> impl Parser<Input, Output = ()>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
		P: Parser<Input>,
{
	value(()).skip(p)
}


pub fn xspace<Input>() -> impl Parser<Input, Output = ()>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	choice((
		space().map(|_| ()),
		line_comment(),
	))
}


pub fn line_comment<Input>() -> impl Parser<Input, Output = ()>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	attempt(string("//"))
		.map(|_| ())
		.skip(skip_many(not_followed_by(newline()).skip(any())))
}


pub fn word<Input>(word: &'static str) -> impl Parser<Input, Output = ()>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	attempt(
		value(())
			.skip(string(word))
			.skip(not_followed_by(alpha_num()))
	).expected(word)
}


pub fn op<Input>(op: &'static str) -> impl Parser<Input, Output = &str>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	attempt(
		string(op)
			.skip(not_followed_by(one_of("+-*/%=<>.".chars())))
	).expected(op)
}


pub fn text<Input>(begin: char, end: char) -> impl Parser<Input, Output = String>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip(char(begin))
		.with(take_until(attempt(char(end))))
			.map(|cs:Vec<char>| cs.iter().collect())
		.skip(char(end))
}


pub fn text2<Input>(begin1: char, begin2: char, end1: char, end2: char) -> impl Parser<Input, Output = String>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	attempt(
		char(begin1).with(char(begin2))
	)
		.with(take_until(attempt( char(end1).with(char(end2)) )))
			.map(|cs:Vec<char>| cs.iter().collect())
		.skip(char(end1))
		.skip(char(end2))
}


pub fn identifier<Input>() -> impl Parser<Input, Output = String>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	attempt(
		letter()
			.and(many(alpha_num().or(char('_'))))
			.map(|(c, mut cs):(char,Vec<char>)| {
				cs.insert(0, c);
				cs.into_iter().collect::<String>()
			})
			.and(optional(char('\'')))
			.map(|(cs, c)| match c { None => cs, Some(_) => cs + "\'", })
	).expected("identifier")
}


pub fn dotted_name<Input>() -> impl Parser<Input, Output = Box<super::node::DottedName>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	sep_end_by1(
		identifier(),
		attempt(wrap_xspaces(char('.'))),
	)
		.map(|names| super::node::DottedName::new_boxed(names))
}


fn summary<Input>() -> impl Parser<Input, Output = String>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip(char('@'))
		.skip(skip_many(xspace()))
		.with(text('(', ')'))
}


pub fn module<Input>() -> impl Parser<Input, Output = Box<super::node::Module>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	event()
		.skip(skip_many(xspace()))
		.and(
			sep_end_by(
				var_stmt(),
				skip_many(xspace())
			)
			.map(|vars:Vec<_>| Box::new(vars)) // XXX: Box<Vec>
		)
		.skip(skip_many(xspace()))
		.and(
			sep_end_by1(state(), skip_many(xspace()))
				.map(|states:Vec<_>| Box::new(states)) // XXX: Box<Vec>
		)
			.map(|((event,vars),states)| super::node::Module::new_boxed(event, vars, states))
}


pub fn event<Input>() -> impl Parser<Input, Output = Box<super::node::Event>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip(word("event"))
		.skip(skip_many(xspace()))
		.with(sep_end_by1(
			event_item(),
			skip_many(xspace())
				.skip(char(','))
				.skip(skip_many(xspace())),
		))
		.skip(skip_many(xspace()))
		.skip(char(';'))
		.map(|items:Vec<_>| super::node::Event::new(items))
}


pub fn event_item<Input>() -> impl Parser<Input, Output = Box<super::node::EventItem>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	dotted_name()
		.map(|name| EventItemBuilder::new_boxed(name))
		.skip(skip_many(xspace()))
		.and(summary())
			.map(|(builder, summary)| builder.set_summary(summary))
		.skip(skip_many(xspace()))
		.and(arg_list_or_empty())
			.map(|(builder, args)| builder.set_args(args))
		.map(|builder| builder.build())
}


struct EventItemBuilder
{
	name: Box<super::node::DottedName>,
	summary: Option<String>,
	args: Option<Box<super::node::ArgList>>,
}


impl EventItemBuilder
{
	fn new_boxed(name: Box<super::node::DottedName>) -> Box<EventItemBuilder>
	{
		Box::new(EventItemBuilder {
			name: name,
			summary: None,
			args: None,
		})
	}

	fn set_summary(mut self: Box<Self>, summary: String) -> Box<Self>
	{
		self.summary = Some(summary);
		self
	}

	fn set_args(mut self: Box<Self>, args: Box<super::node::ArgList>) -> Box<Self>
	{
		self.args = Some(args);
		self
	}

	fn build(self: Box<Self>) -> Box<super::node::EventItem>
	{
		super::node::EventItem::new_boxed(
			self.name,
			self.summary.unwrap(),
			self.args.unwrap(),
		)
	}
}


pub fn state<Input>() -> impl Parser<Input, Output = Box<super::node::State>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip(word("state"))
		.skip(skip_many(xspace()))
		.with(identifier())
			.map(|name| StateBuilder::new_boxed(name))
		.skip(skip_many(xspace()))
		.and(summary())
			.map(|(builder, summary)| builder.set_summary(summary))
		.skip(skip_many(xspace()))
		.and(arg_list_or_empty())
			.map(|(builder, args)| builder.set_args(args))
		.skip(skip_many(xspace()))
		.skip(char('{'))
		.skip(skip_many(xspace()))
		.and(sep_end_by(field(), skip_many(xspace())))
			.map(|(builder, fields)| builder.set_fields(fields))
		.skip(char('}'))
		.map(|builder| builder.build())
}


struct StateBuilder
{
	name: String,
	summary: Option<String>,
	args: Option<Box<super::node::ArgList>>,
	fields: Option<Vec<Box<super::node::Field>>>,
}


impl StateBuilder
{
	fn new_boxed(name: String) -> Box<StateBuilder>
	{
		Box::new(StateBuilder {
			name: name,
			summary: None,
			args: None,
			fields: None,
		})
	}

	fn set_summary(mut self: Box<Self>, summary: String) -> Box<Self>
	{
		self.summary = Some(summary);
		self
	}

	fn set_args(mut self: Box<Self>, args: Box<super::node::ArgList>) -> Box<Self>
	{
		self.args = Some(args);
		self
	}

	fn set_fields(mut self: Box<Self>, fields: Vec<Box<super::node::Field>>) -> Box<Self>
	{
		self.fields = Some(fields);
		self
	}

	fn build(self: Box<Self>) -> Box<super::node::State>
	{
		super::node::State::new_boxed(
			self.name,
			self.summary.unwrap(),
			self.args.unwrap(),
			self.fields.unwrap(),
		)
	}
}


pub fn field<Input>() -> impl Parser<Input, Output = Box<super::node::Field>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	choice((
		var_field(),
		invariant_field(),
		transition_field(),
	))
}


pub fn var_field<Input>() -> impl Parser<Input, Output = Box<super::node::Field>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip(string("var"))
		.skip(skip_many(xspace()))
		.with(identifier())
			.map(|name| VarFieldBuilder::new_boxed(name))
		.skip(skip_many(xspace()))
		.skip(char(':'))
		.skip(skip_many(xspace()))
		.and(expr())
			.map(|(builder, typ)| builder.set_type(typ))
		.skip(skip_many(xspace()))
		.skip(char('='))
		.skip(skip_many(xspace()))
		.and(expr())
			.map(|(builder, init)| builder.set_init(init))
		.skip(skip_many(xspace()))
		.skip(char(';'))
		.map(|builder| builder.build())
}


struct VarFieldBuilder
{
	name: String,
	typ: Option<Box<super::node::Expr>>,
	init: Option<Box<super::node::Expr>>,
}


impl VarFieldBuilder
{
	fn new_boxed(name: String) -> Box<VarFieldBuilder>
	{
		Box::new(VarFieldBuilder {
			name: name,
			typ: None,
			init: None,
		})
	}

	fn set_type(mut self: Box<Self>, typ: Box<super::node::Expr>) -> Box<Self>
	{
		self.typ = Some(typ);
		self
	}

	fn set_init(mut self: Box<Self>, init: Box<super::node::Expr>) -> Box<Self>
	{
		self.init = Some(init);
		self
	}

	fn build(self: Box<Self>) -> Box<super::node::Field>
	{
		super::node::Field::new_var_boxed(
			self.name,
			self.typ.unwrap(),
			self.init.unwrap(),
		)
	}
}


pub fn invariant_field<Input>() -> impl Parser<Input, Output = Box<super::node::Field>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip(string("invariant"))
		.skip(skip_many(xspace()))
		.skip(char('{'))
		.skip(skip_many(xspace()))
		.with(sep_end_by(
			guarded_arm()
				.skip(skip_many(xspace()))
				.skip(char(';')),
			skip_many(xspace())
		))
			.map(|exprs| InvariantFieldBuilder::new_boxed(exprs))
		.skip(skip_many(xspace()))
		.skip(char('}'))
		.skip(skip_many(xspace()))
		.skip(char('@'))
		.skip(skip_many(xspace()))
		.and(text('{', '}'))
			.map(|(builder, desc)| builder.set_description(desc))
		.map(|builder| builder.build())
}


pub fn guarded_arm<Input>() -> impl Parser<Input, Output = Box<super::node::GuardedArm>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	expr()
		.skip(skip_many(xspace()))
		.and(optional(
			op("=>")
				.skip(skip_many(xspace()))
				.with(expr())
		))
		.map(|pair|
			match pair
			{
				(guard, Some(body)) =>
					super::node::GuardedArm::new_boxed(Some(guard), body),
				(body, None) =>
					super::node::GuardedArm::new_boxed(None, body),
			}
		)
}


struct InvariantFieldBuilder
{
	exprs: Vec<Box<super::node::GuardedArm>>,
	desc: Option<String>,
}


impl InvariantFieldBuilder
{
	fn new_boxed(exprs: Vec<Box<super::node::GuardedArm>>) -> Box<InvariantFieldBuilder>
	{
		Box::new(InvariantFieldBuilder {
			exprs: exprs,
			desc: None,
		})
	}

	fn set_description(mut self: Box<Self>, desc: String) -> Box<Self>
	{
		self.desc = Some(desc);
		self
	}

	fn build(self: Box<Self>) -> Box<super::node::Field>
	{
		super::node::Field::new_invariant_boxed(
			self.exprs,
			self.desc.unwrap(),
		)
	}
}


pub fn transition_field<Input>() -> impl Parser<Input, Output = Box<super::node::Field>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip(string("transition"))
		.skip(skip_many(xspace()))
		.with(dotted_name())
			.map(|names| TransitionFieldBuilder::new_boxed(names))
		.skip(skip_many(xspace()))
		.and(arg_list_or_empty())
			.map(|(builder, args)| builder.set_args(args))
		.skip(skip_many(xspace()))
		.and(transition_guard())
			.map(|(builder, guard)| builder.set_guard(guard))
		.skip(skip_many(xspace()))
		.skip(string("-->"))
		.skip(skip_many(xspace()))
		.skip(char('{'))
		.skip(skip_many(xspace()))
		.and(post_cond())
			.map(|(builder, post_cond)| builder.set_post_cond(post_cond))
		.skip(skip_many(xspace()))
		.skip(char('}'))
		.map(|builder| builder.build())
}


struct TransitionFieldBuilder
{
	name: Box<super::node::DottedName>,
	args: Option<Box<super::node::ArgList>>,
	guard: Option<Option<(Box<super::node::Expr>, String)>>,
	post_cond: Option<Box<super::node::PostCond>>,
}


impl TransitionFieldBuilder
{
	fn new_boxed(name: Box<super::node::DottedName>) -> Box<TransitionFieldBuilder>
	{
		Box::new(TransitionFieldBuilder {
			name: name,
			args: None,
			guard: None,
			post_cond: None,
		})
	}

	fn set_args(mut self: Box<Self>, args: Box<super::node::ArgList>) -> Box<Self>
	{
		self.args = Some(args);
		self
	}

	fn set_guard(mut self: Box<Self>, guard: Option<(Box<super::node::Expr>, String)>) -> Box<Self>
	{
		self.guard = Some(guard);
		self
	}

	fn set_post_cond(mut self: Box<Self>, post_cond: Box<super::node::PostCond>) -> Box<Self>
	{
		self.post_cond = Some(post_cond);
		self
	}

	fn build(self: Box<Self>) -> Box<super::node::Field>
	{
		super::node::Field::new_transition_boxed(
			self.name,
			self.args.unwrap(),
			self.guard.unwrap(),
			self.post_cond.unwrap(),
		)
	}
}


pub fn arg_list_or_empty<Input>() -> impl Parser<Input, Output = Box<super::node::ArgList>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	optional(
		skip(char('('))
			.skip(skip_many(xspace()))
			.skip(optional(char(',').skip(skip_many(xspace()))))
			.with(
				sep_end_by1(
					arg_item().skip(skip_many(xspace())), // XXX: trailing spaces.
					skip(char(',')).skip(skip_many(xspace())),
				)
				.map(|args| Box::new(args)) // XXX: Box<Vec>.
			)
			.map(|args| super::node::ArgList::new_boxed(args))
			.skip(skip_many(xspace()))
			.skip(char(')'))
	).map(|opt| opt.unwrap_or_else(|| super::node::ArgList::new_boxed(Box::new(vec![]))))
}


pub fn arg_item<Input>() -> impl Parser<Input, Output = Box<super::node::Arg>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	identifier()
		.skip(skip_many(xspace()))
		.skip(char(':'))
		.skip(skip_many(xspace()))
		.and(expr())
			.map(|(name, typ)| super::node::Arg::new_boxed(name, typ))
}


pub fn transition_guard<Input>() -> impl Parser<Input, Output = Option<(Box<super::node::Expr>, String)>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	optional(
		skip(word("when"))
			.skip(skip_many(xspace()))
			.with(expr())
			.skip(skip_many(xspace()))
			.skip(char('@'))
			.skip(skip_many(xspace()))
			.and(text2('[', '-', '-', ']'))
	)
}


pub fn post_cond<Input>() -> impl Parser<Input, Output = Box<super::node::PostCond>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	skip(word("post"))
	.skip(skip_many(xspace()))
	.skip(char('{'))
	.skip(skip_many(xspace()))
	.with(
		optional(
			skip(word("target"))
			.skip(skip_many(xspace()))
			.with(sep_end_by1(
				identifier(), 
				attempt(
					skip_many(xspace()).skip(char(',')).skip(skip_many(xspace()))
				)
			))
			.map(|targets| Box::new(targets)) // XXX: Box<Vec>.
			.skip(skip_many(xspace()))
			.skip(char(';'))
			.skip(skip_many(xspace()))
		)
		.map(|targets| targets.unwrap_or_else(|| Box::new(vec![])))
		.map(|targets| PostCondBuilder::new_boxed(targets))
	)
	.and(
		sep_end_by1(
			mutation().skip(skip_many(xspace()).skip(char(';'))),
			skip_many(xspace()),
		)
		.map(|exprs| Box::new(exprs)) // XXX: Box<Vec>.
	)
		.map(|(builder, exprs)| builder.set_exprs(exprs))
	.skip(char('}'))
	.skip(skip_many(xspace()))
	.skip(char('@'))
	.skip(skip_many(xspace()))
	.and(text2('{', '-', '-', '}'))
		.map(|(builder, desc)| builder.set_description(desc))
	.map(|builder| builder.build())
}


struct PostCondBuilder
{
	targets: Box<Vec<String>>,
	exprs: Option<Box<Vec<Box<super::node::Mutation>>>>,
	desc: Option<String>,
}


impl PostCondBuilder
{
	fn new_boxed(targets: Box<Vec<String>>) -> Box<PostCondBuilder>
	{
		Box::new(PostCondBuilder {
			targets: targets,
			exprs: None,
			desc: None,
		})
	}

	fn set_exprs(mut self: Box<Self>, exprs: Box<Vec<Box<super::node::Mutation>>>) -> Box<Self>
	{
		self.exprs = Some(exprs);
		self
	}

	fn set_description(mut self: Box<Self>, desc: String) -> Box<Self>
	{
		self.desc = Some(desc);
		self
	}

	fn build(self: Box<Self>) -> Box<super::node::PostCond>
	{
		super::node::PostCond::new_boxed(
			self.targets,
			self.exprs.unwrap(),
			self.desc.unwrap(),
		)
	}
}


pub fn var_stmt<Input>() -> impl Parser<Input, Output = Box<super::node::VarStmt>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	choice((
		word("var").map(|_| super::node::VarType::Var),
		word("const").map(|_| super::node::VarType::Const),
	))
		.map(|typ| VarStmtBuilder::new_boxed(typ))
		.skip(skip_many(xspace()))
		.and(expr())
			.map(|(builder, matcher)| builder.set_matcher(matcher))
		.skip(skip_many(xspace()))
		.skip(op(":"))
		.skip(skip_many(xspace()))
		.and(expr())
			.map(|(builder, typ)| builder.set_type(typ))
		.skip(skip_many(xspace()))
		.skip(op("="))
		.skip(skip_many(xspace()))
		.and(expr())
			.map(|(builder, body)| builder.set_body(body))
		.skip(skip_many(xspace()))
		.skip(char(';'))
		.map(|builder| builder.build())
}


struct VarStmtBuilder
{
	var_type: super::node::VarType,
	matcher: Option<Box<super::node::Expr>>,
	typ: Option<Box<super::node::Expr>>,
	body: Option<Box<super::node::Expr>>,
	summary: Option<String>,
	desc: Option<String>,
}


impl VarStmtBuilder
{
	fn new_boxed(var_type: super::node::VarType) -> Box<VarStmtBuilder>
	{
		Box::new(VarStmtBuilder {
			var_type: var_type,
			matcher: None,
			typ: None,
			body: None,
			summary: None,
			desc: None,
		})
		.set_summary("".into()) // XXX: FIXME
		.set_description("".into()) // XXX: FIXME
	}

	fn set_matcher(mut self: Box<Self>, matcher: Box<super::node::Expr>) -> Box<Self>
	{
		self.matcher = Some(matcher);
		self
	}

	fn set_type(mut self: Box<Self>, typ: Box<super::node::Expr>) -> Box<Self>
	{
		self.typ = Some(typ);
		self
	}

	fn set_body(mut self: Box<Self>, body: Box<super::node::Expr>) -> Box<Self>
	{
		self.body = Some(body);
		self
	}

	fn set_summary(mut self: Box<Self>, summary: String) -> Box<Self>
	{
		self.summary = Some(summary);
		self
	}

	fn set_description(mut self: Box<Self>, desc: String) -> Box<Self>
	{
		self.desc = Some(desc);
		self
	}

	fn build(self: Box<Self>) -> Box<super::node::VarStmt>
	{
		super::node::VarStmt::new_boxed(
			self.var_type,
			self.matcher.unwrap(),
			self.typ.unwrap(),
			self.body.unwrap(),
			self.summary.unwrap(),
			self.desc.unwrap(),
		)
	}
}


pub fn mutation<Input>() -> impl Parser<Input, Output = Box<super::node::Mutation>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	expr()
		.map(|matcher|
			MutationBuilder::new_boxed(matcher))
		.skip(skip_many(xspace()))
		.skip(char('='))
		.skip(skip_many(xspace()))
		.and(expr())
		.map(|(builder, body)| builder.set_body(body))
		.map(|builder| builder.build())
}


struct MutationBuilder
{
	matcher: Box<super::node::Expr>,
	body: Option<Box<super::node::Expr>>,
}


impl MutationBuilder
{
	fn new_boxed(matcher: Box<super::node::Expr>) -> Box<MutationBuilder>
	{
		Box::new(MutationBuilder {
			matcher: matcher,
			body: None,
		})
	}

	fn set_body(mut self: Box<Self>, body: Box<super::node::Expr>) -> Box<Self>
	{
		self.body = Some(body);
		self
	}

	fn build(self: Box<Self>) -> Box<super::node::Mutation>
	{
		super::node::Mutation::new_boxed(
			self.matcher,
			self.body.unwrap(),
		)
	}
}


pub fn expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	or_expr()
}


pub fn expr_rec<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	combine::parser(|input| {
		expr().parse_stream(input).into_result()
	})
}


pub fn or_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	chainl1(
		and_expr(),
		attempt(
			skip_many(xspace())
				.skip(string("||"))
				.skip(skip_many(xspace()))
				.map(|()|
					|lhs, rhs| super::node::Expr::new_or_boxed(lhs, rhs)
				)
		)
	)
}


pub fn and_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	chainl1(
		cmp_expr(),
		attempt(
			skip_many(xspace())
				.skip(string("&&"))
				.skip(skip_many(xspace()))
				.map(|()|
					|lhs, rhs| super::node::Expr::new_and_boxed(lhs, rhs)
				)
		)
	)
}


fn wrap_xspaces<Input, T, P>(p: P) -> impl Parser<Input, Output = T>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
		P: Parser<Input, Output = T>,
{
	skip_many(xspace())
		.with(p)
		.skip(skip_many(xspace()))
}


pub fn cmp_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	chainl1(
		append_expr(),
		attempt(wrap_xspaces(
			choice((
				attempt(string("/=")),
				attempt(string("==")),
				attempt(string("<=")),
				string("<"),
				attempt(string(">=")),
				string(">"),
			))
			.map(|op|
				move |lhs, rhs| super::node::Expr::new_cmp_boxed(lhs, op.into(), rhs)
			)
		))
	)
}


pub fn append_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	chainl1(
		add_expr(),
		attempt(wrap_xspaces(
			choice((
				op("++"), // XXX: op(":")
				op("++"),
			))
			.map(|op|
				move |lhs, rhs| super::node::Expr::new_append_boxed(lhs, op.into(), rhs)
			)
		))
	)
}


pub fn add_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	chainl1(
		mul_expr(),
		attempt(wrap_xspaces(
			choice((
				op("+"),
				op("-"),
			))
			.map(|op|
				move |lhs, rhs| super::node::Expr::new_add_boxed(lhs, op.into(), rhs)
			)
		))
	)
}


pub fn mul_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	chainl1(
		dotted_expr(),
		attempt(wrap_xspaces(
			choice((
				op("*"),
				op("/"),
			))
			.map(|op|
				move |lhs, rhs| super::node::Expr::new_mul_boxed(lhs, op.into(), rhs)
			)
		))
	)
}


pub fn dotted_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	chainl1(
		appli_expr(),
		attempt(wrap_xspaces(
			op(".")
			.map(|_|
				|lhs, rhs| super::node::Expr::new_dotted_boxed(lhs, rhs)
			)
		))
	)
}


pub fn appli_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	chainl1(
		term_expr()
			.skip(skip_many(xspace())), // XXX: trailing spaces.
		attempt(
			skip_many(xspace())
				.map(|()|
					|lhs, rhs| super::node::Expr::new_appli_boxed(lhs, rhs)
				)
		)
	)
}


pub fn term_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	choice((
		case_expr(),
		name_expr(),
		num_expr(),
		str_expr(),
		nil_expr(),
		paren_expr(),
	))
}


pub fn name_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	identifier()
		.map(|name| super::node::Expr::new_name_boxed(name))
}


pub fn num_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	many1(digit())
		.map(|digits: String| super::node::Expr::new_num_boxed(digits.parse::<usize>().unwrap()))
}


pub fn str_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	text('"', '"')
		.map(|s: String| super::node::Expr::new_str_boxed(s))
}


pub fn case_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	word("case")
		.skip(skip_many(xspace()))
		.with(expr_rec())
		.skip(skip_many(xspace()))
		.skip(char('{'))
		.skip(skip_many(xspace()))
		.and(
			sep_end_by1(
				case_elem(),
				skip(skip_many(xspace()))
			)
			.map(|elems:Vec<_>| Box::new(elems)) // XXX: Box<Vec>
		)
		.skip(char('}'))
		.map(|(expr, elems)| super::node::Expr::new_case_boxed(expr, elems))
}


pub fn case_elem<Input>() -> impl Parser<Input, Output = Box<super::node::CaseElem>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	expr_rec()
		.skip(skip_many(xspace()))
		.and(
			optional(
				word("when")
					.skip(skip_many(xspace()))
					.with(expr_rec())
					.skip(skip_many(xspace()))
			)
		)
		.skip(op("=>"))
		.skip(skip_many(xspace()))
		.and(expr_rec())
		.skip(skip_many(xspace()))
		.skip(char(';'))
		.map(|((cond,guard), body) | super::node::CaseElem::new_boxed(cond, guard, body))
}


pub fn nil_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	attempt(string("[]"))
		.map(|_| super::node::Expr::new_nil_boxed())
}


pub fn paren_expr<Input>() -> impl Parser<Input, Output = Box<super::node::Expr>>
	where
		Input: combine::Stream<Token = char>,
		Input::Error: combine::ParseError<Input::Token, Input::Range, Input::Position>,
{
	char('(')
		.with(expr_rec())
		.skip(char(')'))
		.map(|x| super::node::Expr::new_paren_boxed(x))
}
