trait ToMarkdown
{
	fn to_markdown(&self) -> String;
}

pub trait ToMarkdownWithTitle
{
	fn to_markdown(&self, title: &String) -> String;
}

pub trait ToMarkdownWithModule
{
	fn to_markdown(&self, module: &super::node::Module) -> String;
}

trait ToMarkdownWithIndex
{
	fn to_markdown(&self, index: usize) -> String;
}

trait ToMarkdownWithIndexAndModule
{
	fn to_markdown(&self, i: usize, module: &super::node::Module) -> String;
}

trait ToMarkdownIfHasOr
{
	fn to_markdown_if_has_or(&self, default: &str) -> String;
}

trait MarkdownHeader
{
	fn markdown_header() -> String;
}

trait ToText
{
	fn to_text(&self) -> String;
}

trait ToCode
{
	fn to_code(&self) -> String;
}

trait IsTransition
{
	fn is_transition(&self) -> bool;
}


fn if_has_or<T, F>(vec: &Vec<T>, default: &str, f: F) -> String
	where F: FnOnce(&Vec<T>) -> String,
{
	if vec.is_empty()
	{
		default.into()
	}else
	{
		f(vec)
	}
}


fn if_has_field_or<U, Filter, Map, S>(vec1: &Vec<Box<super::node::Field>>, filter: Filter, map: Map, default: S) -> String
	where
		Filter: Fn(&super::node::Field) -> Option<&U>,
		Map: FnOnce(Vec<&U>) -> String,
		S: Into<String>,
{
	let vec2 = vec1.iter().filter_map(|x|filter(x)).collect::<Vec<&U>>();
	if !vec2.is_empty()
	{
		map(vec2)
	}else
	{
		default.into()
	}
}


impl ToMarkdownWithTitle for super::node::Module
{
	fn to_markdown(&self, title: &String) -> String
	{
		vec![
			"---\n".into(),
			format!("title: {}\n", title),
			"---\n".into(),
			"\n".into(),

			self.events().to_markdown(),
			"\n".into(),

			"## Variables\n".into(),
			"\n".into(),
			match self.vars().is_empty() {
				true => "No variables.\n".into(),
				false => vec![
						super::node::VarStmt::markdown_header(),
						self.vars()
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_markdown(i + 1))
							.collect::<Vec<_>>()
							.concat(),
					].concat(),
			},
			"\n".into(),

			self.states()
				.iter()
				.map(|state| state.to_markdown(self))
				.collect::<Vec<_>>()
				.join("\n"),
		].concat()
	}
}


impl ToMarkdown for super::node::Event
{
	fn to_markdown(&self) -> String
	{
		vec![
			"## Events\n".into(),
			"\n".into(),

			if_has_or(
				self.items(),
				"No events.\n",
				|items| vec![
						"| # | Name | Summary |\n".into(),
						"|---|------|---------|\n".into(),
						items
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_markdown(i + 1))
							.collect::<Vec<_>>()
							.concat(),
						].concat(),
			),
		].concat()
	}
}


impl ToMarkdownWithIndex for super::node::EventItem
{
	fn to_markdown(&self, i: usize) -> String
	{
		format!(
			"| {i} | {name}{args} | {summary} |\n",
			i = i,
			name = self.name().to_text(),
			args = self.args().to_markdown_if_has_or(""),
			summary = self.summary(),
		)
	}
}


impl ToText for super::node::DottedName
{
	fn to_text(&self) -> String
	{
		self.names().join(".")
	}
}


impl ToMarkdownWithModule for super::node::State
{
	fn to_markdown(&self, module: &super::node::Module) -> String
	{
		vec![
			format!(
				"## state {name}{args}: {summary}\n",
				name = self.name(),
				args = self.args().to_markdown_if_has_or(""),
				summary = self.summary(),
			),
			"\n".into(),

			"### Variables\n".into(),
			"\n".into(),
			if_has_field_or(
				self.fields(),
				|field| field.get_var(),
				|x| vec![
						super::node::VarField::markdown_header(),
						x
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_markdown(i + 1))
							.collect::<Vec<_>>()
							.concat(),
					].concat(),
				"No variables.\n"
			),
			"\n".into(),

			"### Invariants\n".into(),
			"\n".into(),
			if_has_field_or(
				self.fields(),
				|field| field.get_invariant(),
				|x| vec![
						super::node::InvariantField::markdown_header(),
						x
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_markdown(i + 1))
							.collect::<Vec<_>>()
							.concat(),
					].concat(),
				"No invariants.\n"
			),
			"\n".into(),

			"### Transitions\n".into(),
			"\n".into(),
			if_has_field_or(
				self.fields(),
				|field| field.get_transition(),
				|x| vec![
						super::node::TransitionField::markdown_header(),
						x
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_markdown(i + 1, module))
							.collect::<Vec<_>>()
							.concat(),
					].concat(),
				"No transitions.\n"
			),
		].concat()
	}
}


impl MarkdownHeader for super::node::VarField
{
	fn markdown_header() -> String
	{
		vec![
			"| # | Name | Type | Init | Description | Note |\n",
			"|---|------|------|------|-------------|------|\n",
		].concat()
	}
}


impl ToMarkdownWithIndex for super::node::VarField
{
	fn to_markdown(&self, i: usize) -> String
	{
		format!(
			"| {i} | {name} | `{type}` | `{init}` | {summary} | {desc} |\n",
			i = i,
			name = self.name(),
			type = self.typ().to_code(),
			init = self.init().to_code(),
			summary = "",
			desc = "",
		)
	}
}


impl MarkdownHeader for super::node::InvariantField
{
	fn markdown_header() -> String
	{
		vec![
			"| # | Logical formula | Description |\n",
			"|---|-----------------|-------------|\n",
		].concat()
	}
}


impl ToMarkdownWithIndex for super::node::InvariantField
{
	fn to_markdown(&self, i: usize) -> String
	{
		format!(
			"| {i} | {exprs} | {desc} |\n",
			i = i,
			exprs = self.exprs()
				.iter()
				.map(|x| format!("`{};`", x.to_markdown()))
				.collect::<Vec<_>>()
				.join("<br />"),
			desc = self.desc().trim().replace("\n", "<br />"),
		)
	}
}

impl ToMarkdown for super::node::GuardedArm
{
	fn to_markdown(&self) -> String
	{
		match self.guard()
		{
			Some(guard) => format!("{} => {}", guard.to_code(), self.body().to_code()),
			None => self.body().to_code(),
		}
	}
}

impl MarkdownHeader for super::node::TransitionField
{
	fn markdown_header() -> String
	{
		vec![
			concat!(
				"| # ",
				"| Event<br />(Name) ",
				"| Event<br />(Summary) ",
				"| Guard<br />(Expr) ",
				"| Guard<br />(Summary) ",
				"| Post cond<br />(Target) ",
				"| Post cond<br />(Expr) ",
				"| Transition ",
				"| Post cond<br />(Description) ",
				"|\n"
			),
			concat!(
				"|---", // "| # ",
				"|---", // "| Name ",
				"|---", // "| Event<br />(Summary) ",
				"|---", // "| Guard<br />(Expr) ",
				"|---", // "| Guard<br />(Summary) ",
				"|---", // "| Post cond<br />(Targets) ",
				"|---", // "| Post cond<br />(Expr) ",
				"|---", // "| Transition ",
				"|---", // "| Post cond<br />(Description) ",
				"|\n"
			),
		].concat()
	}
}

impl ToMarkdownWithIndexAndModule for super::node::TransitionField
{
	fn to_markdown(&self, i: usize, module: &super::node::Module) -> String
	{
		format!(
			concat!(
				"| {i} | {name}{args} | {summary} | {guard_expr} | {guard_summary} ",
				"| {post_targets} | {post_expr} ",
				"| {transition} | {post_desc} ",
				"|\n",
			),
			i = i,
			name = self.name().to_text(),
			args = self.args().to_markdown_if_has_or(""),
			summary = module.get_event_summary(self.name()).unwrap_or(&"".into()),
			guard_expr = self.guard().as_ref().map_or(
					"".into(),
					|x| format!("`{}`", x.0.to_code()),
				),
			guard_summary = self.guard().as_ref().map_or(&"".into(), |x| &x.1),
			post_targets = self.post_cond().targets().join(", "),
			post_expr = self.post_cond().exprs()
				.iter()
				.filter(|x| !x.is_transition())
				.map(|x| String::from("`") + &x.to_code() + ";`")
				.collect::<Vec<_>>()
				.join("<br />"),
			transition = self.post_cond().exprs()
				.iter()
				.filter(|x| x.is_transition())
				.map(|x| String::from("`") + &x.to_code() + ";`")
				.collect::<Vec<_>>()
				.join("<br />"),
			post_desc = self.post_cond().desc().trim().replace("\n", "<br />"),
		)
	}
}

impl ToMarkdownIfHasOr for super::node::ArgList
{
	fn to_markdown_if_has_or(&self, default: &str) -> String
	{
		if (*self.args()).is_empty()
		{
			default.into()
		}else
		{
			String::from("(")
				+ &self.args()
					.iter()
					.map(|arg| arg.to_markdown())
					.collect::<Vec<_>>()
					.join(", ")
				+ ")"
		}
	}
}


impl ToMarkdown for super::node::Arg
{
	fn to_markdown(&self) -> String
	{
		vec![
			self.name().clone(),
			": ".into(),
			self.typ().to_code(),
		].concat()
	}
}


impl ToText for super::node::VarType
{
	fn to_text(&self) -> String
	{
		match self
		{
		&super::node::VarType::Var => "var".into(),
		&super::node::VarType::Const => "const".into(),
		}
	}
}

impl MarkdownHeader for super::node::VarStmt
{
	fn markdown_header() -> String
	{
		vec![
			"| # | Name | Type | Init | Summary | Description |\n",
			"|---|------|------|------|---------|-------------|\n",
		].concat()
	}
}

impl ToMarkdownWithIndex for super::node::VarStmt
{
	fn to_markdown(&self, i: usize) -> String
	{
		format!(
			"| {i} | {var_type} {name} | `{type}` | `{init}` | {summary} | {desc} |\n",
			i = i,
			var_type = self.var_type().to_text(),
			name = self.matcher().to_code(),
			type = self.typ().to_code(),
			init = self.init().to_code(),
			summary = self.summary(),
			desc = self.desc(),
		)
	}
}

impl IsTransition for super::node::Mutation
{
	fn is_transition(&self) -> bool
	{
		match self.lhs().get_name()
		{
			Some(name) => name.name() == "state'",
			None => false,
		}
	}
}

impl ToCode for super::node::Mutation
{
	fn to_code(&self) -> String
	{
		self.lhs().to_code() + " = " + &self.rhs().to_code()
	}
}


impl ToCode for super::node::Expr
{
	fn to_code(&self) -> String
	{
		match self
		{
		super::node::Expr::Or(x) => x.to_code(),
		super::node::Expr::And(x) => x.to_code(),
		super::node::Expr::Cmp(x) => x.to_code(),
		super::node::Expr::Dotted(x) => x.to_code(),
		super::node::Expr::Appli(x) => x.to_code(),
		super::node::Expr::Name(x) => x.to_code(),
		super::node::Expr::Num(x) => x.to_code(),
		super::node::Expr::Str(x) => x.to_code(),
		super::node::Expr::Case(x) => x.to_code(),
		super::node::Expr::Nil => "[]".into(),
		super::node::Expr::Paren(x) => String::from("(") + &x.to_code() + ")",
		super::node::Expr::BinOp(x) => x.to_code(),
		}
	}
}

impl ToCode for super::node::OrExpr
{
	fn to_code(&self) -> String
	{
		format!("{} {} {}", self.lhs().to_code(), "||", self.rhs().to_code())
	}
}

impl ToCode for super::node::AndExpr
{
	fn to_code(&self) -> String
	{
		format!("{} {} {}", self.lhs().to_code(), "&&", self.rhs().to_code())
	}
}

impl ToCode for super::node::CmpExpr
{
	fn to_code(&self) -> String
	{
		format!("{} {} {}", self.lhs().to_code(), self.op(), self.rhs().to_code())
	}
}

impl ToCode for super::node::AppliExpr
{
	fn to_code(&self) -> String
	{
		format!("{} {}", self.lhs().to_code(), self.rhs().to_code())
	}
}

impl ToCode for super::node::NameExpr
{
	fn to_code(&self) -> String
	{
		self.name().clone()
	}
}

impl ToCode for super::node::NumExpr
{
	fn to_code(&self) -> String
	{
		format!("{}", self.num())
	}
}

impl ToCode for super::node::StrExpr
{
	fn to_code(&self) -> String
	{
		format!("\"{}\"", self.s())
	}
}

impl ToCode for super::node::BinOpExpr
{
	fn to_code(&self) -> String
	{
		format!("{} {} {}", self.lhs().to_code(), self.op(), self.rhs().to_code())
	}
}

impl ToCode for super::node::DottedExpr
{
	fn to_code(&self) -> String
	{
		format!("{}.{}", self.lhs().to_code(), self.rhs().to_code())
	}
}

impl ToCode for super::node::CaseExpr
{
	fn to_code(&self) -> String
	{
		vec![
			vec![
				"match".into(),
				self.expr().to_code(),
				"{".into(),
			],
			self.elems().iter().flat_map(|x|
					vec![
						vec![ x.cond().to_code() ],
						x.guard().map_or(vec![], |x| vec![
								"when".into(),
								x.to_code(),
							]),
						vec![
							"=>".into(),
							x.body().to_code() + ";",
						],
					].concat()
				).collect::<Vec<_>>(),
			vec![
				"}".into(),
			],
		].concat().join(" ")
	}
}
