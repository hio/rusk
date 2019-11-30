use std::rc::Rc;
use crate::ast;


#[derive(Clone, Debug)]
pub enum Doc
{
	Empty,
	Heading(usize, Rc<Doc>),
	Fragment(Rc<Vec<Doc>>),
	Number(usize),
	String(Rc<String>),
	Static(&'static str),
	Br,
	Code(Rc<Doc>),
	SepBy(&'static str, Rc<Vec<Doc>>),
	SepByDoc(Rc<Doc>, Rc<Vec<Doc>>),
	SepEndBy(&'static str, &'static str, Rc<Vec<Doc>>),
	HeaderRow(Rc<Vec<Doc>>),
	Row(Rc<Vec<Doc>>),
	Cell(Rc<Doc>),
}


trait ToDoc
{
	fn to_doc(&self) -> Doc;
}

pub trait ToDocWithTitle
{
	fn to_doc(&self, title: &String) -> Doc;
}

pub trait ToDocWithModule
{
	fn to_doc(&self, _module: &ast::Module) -> Doc;
}

trait ToDocWithIndex
{
	fn to_doc(&self, _index: usize) -> Doc;
}

trait ToDocWithIndexAndModule
{
	fn to_doc(&self, _index: usize, _module: &ast::Module) -> Doc;
}

trait ToDocIfHasOr
{
	fn to_doc_if_has_or(&self, _default: Doc) -> Doc;
}

trait HeaderDoc
{
	fn header_doc() -> Doc;
}


trait IsTransition
{
	fn is_transition(&self) -> bool;
}


fn if_has_or_<T, F>(vec: &Vec<T>, default: &'static str, f: F) -> Doc
	where F: FnOnce(&Vec<T>) -> Doc,
{
	if vec.is_empty()
	{
		Doc::Static(default)
	}else
	{
		f(vec)
	}
}


fn if_has_field_or_<U, Filter, Map>(vec1: &Vec<Box<ast::Field>>, filter: Filter, map: Map, default: &'static str) -> Doc
	where
		Filter: Fn(&ast::Field) -> Option<&U>,
		Map: FnOnce(Vec<&U>) -> Doc,
{
	let vec2 = vec1.iter().filter_map(|x|filter(x)).collect::<Vec<&U>>();
	if vec2.is_empty()
	{
		Doc::Static(default)
	}else
	{
		map(vec2)
	}
}


impl ToDocWithTitle for ast::Module
{
	fn to_doc(&self, title: &String) -> Doc
	{
		Doc::Fragment(Rc::new(vec![
			Doc::Static("---\n"),
			Doc::Fragment(Rc::new(vec![
				Doc::Static("title: "),
				Doc::String(title.clone().into()),
				Doc::Static("\n"),
			])),
			Doc::Static("---\n"),
			Doc::Static("\n"),

			if_has_or_(
				self.types(),
				"",
				|items| Doc::Fragment(Rc::new(vec![
						Doc::Heading(2, Rc::new(Doc::Static("Types"))),
						Doc::Static("\n"),
						ast::TypeStmt::header_doc(),
						Doc::Fragment(Rc::new(items
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_doc(i + 1))
							.collect::<Vec<_>>()
						)),
						Doc::Static("\n"),
					])),
			),

			if_has_or_(
				self.events(),
				"",
				|items| Doc::Fragment(Rc::new(vec![
						Doc::Heading(2, Rc::new(Doc::Static("Events"))),
						Doc::Static("\n"),
						Doc::Static("| # | Name | Summary | Description |\n"),
						Doc::Static("|---|------|---------|-------------|\n"),
						Doc::Fragment(Rc::new(items
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_doc(i + 1))
							.collect::<Vec<_>>()
						)),
						Doc::Static("\n"),
					])),
			),

			match self.vars().is_empty() {
				true => Doc::Empty,
				false => {
					Doc::Fragment(Rc::new(vec![
						Doc::Heading(2, Rc::new(Doc::Static("Module Variables"))),
						Doc::Static("\n"),
						ast::VarStmt::header_doc(),
						Doc::Fragment(Rc::new(self.vars()
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_doc(i + 1))
							.collect::<Vec<_>>()
						)),
						Doc::Static("\n"),
					]))
				},
			},

			match self.invariants().is_empty() {
				true => Doc::Empty,
				false => Doc::Fragment(Rc::new(vec![
					Doc::Heading(2, Rc::new(Doc::Static("Module Invariants"))),
					Doc::Static("\n"),
					Doc::Fragment(Rc::new(vec![
						ast::InvariantField::header_doc(),
						Doc::Fragment(Rc::new(
							self.invariants()
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc(i + 1))
								.collect::<Vec<_>>()
						)),
					])),
					Doc::Static("\n"),
				])),
			},

			Doc::SepBy("\n", Rc::new(self.states()
				.iter()
				.map(|state| state.to_doc(self))
				.collect::<Vec<_>>()
			)),
		]))
	}
}


impl HeaderDoc for ast::TypeStmt
{
	fn header_doc() -> Doc
	{
		Doc::HeaderRow(Rc::new(vec![
			Doc::Static("#"),
			Doc::Static("Name"),
			Doc::Static("Summary"),
			Doc::Static("Definition"),
			Doc::Static("Description"),
		]))
	}
}


impl ToDocWithIndex for ast::TypeStmt
{
	fn to_doc(&self, i: usize) -> Doc
	{
		Doc::Row(Rc::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name}{args} |
			name_args_to_doc(self.name(), self.args()),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::String(Rc::new(summ.clone()))),
			// | {definition} |
			if self.items().len() == 1
			{
				Doc::Cell(Rc::new(self.items()[0].to_doc()))
			}else
			{
				Doc::Cell(Rc::new(Doc::SepEndBy(" |<br />", " |", Rc::new(self.items().iter().map(|item| item.to_doc()).collect::<Vec<_>>()))))
			},
			// | {desc} |
			Doc::Cell(Rc::new(self.description().as_ref().as_ref().map_or(Doc::Empty, |desc| Doc::String(Rc::new(desc.clone()))))),
		]))
	}
}


impl ToDoc for ast::TypeItem
{
	fn to_doc(&self) -> Doc
	{
		match self
		{
			ast::TypeItem::DataDef(def) =>
				def.to_doc(),
			ast::TypeItem::RecordDef(def) =>
				def.to_doc(),
		}
	}
}


impl ToDoc for ast::DataDef
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			vec![ Doc::String(Rc::new(self.name().clone())) ],
			self.args().args().iter().map(|arg| arg.to_doc()).collect::<Vec<_>>(),
		].concat()))
	}
}


impl ToDoc for ast::RecordDef
{
	fn to_doc(&self) -> Doc
	{
		Doc::Fragment(Rc::new(vec![
			self.name().as_ref().map_or(Doc::Empty, |name| Doc::String(Rc::new(*name.clone() + " "))),
			self.fields().to_doc()
		]))
	}
}


impl ToDoc for Vec<Box<ast::RecordField>>
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepByDoc(Rc::new(Doc::Br), Rc::new(vec![
			vec![ Doc::Static("{"), ],
			self.iter()
				.map(|field| Doc::Fragment(Rc::new(vec![
					field.name().to_doc(),
					field.summary().as_ref().map_or(Doc::Empty, to_summary_doc),
					Doc::Static(": "),
					field.typ().to_doc(),
					field.description().as_ref().map_or(Doc::Empty, to_description_doc),
					Doc::Static(","),
				])))
				.collect::<Vec<_>>(),
			vec![ Doc::Static("}"), ],
		].concat()))
	}
}

fn to_summary_doc(summ: &String) -> Doc
{
	Doc::Fragment(Rc::new(vec![
		Doc::Static(" @( "),
		Doc::String(Rc::new(summ.clone())),
		Doc::Static(" )"),
	]))
}

fn to_description_doc(desc: &String) -> Doc
{
	Doc::Fragment(Rc::new(vec![
		Doc::Static(" @{- "),
		Doc::String(Rc::new(desc.clone())),
		Doc::Static(" -}"),
	]))
}

impl HeaderDoc for ast::Event
{
	fn header_doc() -> Doc
	{
		Doc::Fragment(Rc::new(vec![
			Doc::Heading(2, Rc::new(Doc::Static("Events"))),
			Doc::Static("\n"),
		]))
	}
}


impl ToDoc for ast::Event
{
	fn to_doc(&self) -> Doc
	{
		Doc::Fragment(Rc::new(vec![
			Self::header_doc(),

			if_has_or_(
				self.items(),
				"No events.\n",
				|items| Doc::Fragment(Rc::new(vec![
						vec![ Doc::Static(concat![
							"| # | Name | Summary | Description |\n",
							"|---|------|---------|-------------|\n",
						]) ],
						items
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_doc(i + 1))
							.collect::<Vec<_>>(),
					].concat())),
			),
		]))
	}
}


impl ToDocWithIndex for ast::EventItem
{
	fn to_doc(&self, i: usize) -> Doc
	{
		Doc::Row(Rc::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name}{args} |
			name_args_to_doc(self.name(), self.args()),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::String(Rc::new(summ.clone()))),
			// | {desc} |
			Doc::Cell(Rc::new(self.description().as_ref().as_ref().map_or(Doc::Empty, |desc| Doc::String(Rc::new(desc.clone()))))),
		]))
	}
}


impl ToDoc for ast::DottedName
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(".", Rc::new(
			self.names().iter().map(|name|
				Doc::String(Rc::new(name.clone()))
			).collect::<Vec<_>>()
		))
	}
}


impl ToDocWithModule for ast::State
{
	fn to_doc(&self, module: &ast::Module) -> Doc
	{
		Doc::Fragment(Rc::new(vec![
			// "## state {name_args} @ {summary}\n",
			Doc::Heading(2, Rc::new(Doc::Fragment(Rc::new(vec![
				Doc::Static("state "),
				{
					let name = ast::DottedName::new_boxed(vec![self.name().clone()]);
					name_args_to_doc(name.as_ref(), self.args())
				},
				match self.summary() {
					Some(summ) => Doc::Fragment(Rc::new(vec![
						Doc::Static(" @ "),
						Doc::String((summ as &String).clone().into()),
					])),
					None => Doc::Empty,
				},
			])))),
			Doc::Static("\n"),

			// description.
			match self.description() {
				Some(desc) =>
					Doc::Fragment(Rc::new(vec![
						Doc::String(Rc::new((*desc).as_ref().clone())),
						Doc::Static("\n"),
					])),
				None => Doc::Empty,
			},

			Doc::Heading(3, Rc::new(Doc::Static("Variables"))),
			Doc::Static("\n"),
			if_has_field_or_(
				self.fields(),
				|field| field.get_var(),
				|vars| Doc::Fragment(Rc::new(vec![
						ast::VarField::header_doc(),
						Doc::Fragment(Rc::new(
							vars
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc(i + 1))
								.collect::<Vec<_>>(),
						)),
					])),
				"No variables.\n"
			),
			Doc::Static("\n"),

			Doc::Heading(3, Rc::new(Doc::Static("Invariants"))),
			Doc::Static("\n"),
			if_has_field_or_(
				self.fields(),
				|field| field.get_invariant(),
				|vec| Doc::Fragment(Rc::new(vec![
						ast::InvariantField::header_doc(),
						Doc::Fragment(Rc::new(vec
							.iter()
							.enumerate()
							.map(|(i, x)| x.to_doc(i + 1))
							.collect::<Vec<_>>())),
					])),
				"No invariants.\n"
			),
			Doc::Static("\n"),

			Doc::Heading(3, Rc::new(Doc::Static("Transitions"))),
			Doc::Static("\n"),
			if_has_field_or_(
				self.fields(),
				|field| field.get_transition(),
				|transitions| Doc::Fragment(Rc::new(vec![
						ast::TransitionField::header_doc(),
						Doc::Fragment(Rc::new(
							transitions
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc(i + 1, module))
								.collect::<Vec<_>>(),
						)),
					])),
				"No transitions.\n"
			),
		]))
	}
}


impl HeaderDoc for ast::VarField
{
	fn header_doc() -> Doc
	{
		Doc::Static(concat![
			"| # | Name | Type | Init | Summary | Description |\n",
			"|---|------|------|------|---------|-------------|\n",
		])
	}
}


impl ToDocWithIndex for ast::VarField
{
	fn to_doc(&self, i: usize) -> Doc
	{
		Doc::Row(Rc::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name} |
			self.name().to_doc(),
			// | `{type}` |
			self.typ().as_ref().map_or(Doc::Empty, |typ| Doc::Code(Rc::new(typ.to_doc()))),
			// | `{init}` |
			self.init().as_ref().map_or(Doc::Empty, |init| Doc::Code(Rc::new(init.to_doc()))),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::String(Rc::new(summ.clone()))),
			// | {desc} |
			Doc::Cell(Rc::new(self.description().as_ref().map_or(Doc::Empty, |summ| Doc::String(Rc::new(summ.clone()))))),
		]))
	}
}


impl HeaderDoc for ast::InvariantField
{
	fn header_doc() -> Doc
	{
		Doc::Static(concat![
			"| # | Name | Summary | Logical formula | Description |\n",
			"|---|------|---------|-----------------|-------------|\n",
		])
	}
}


impl ToDocWithIndex for ast::InvariantField
{
	fn to_doc(&self, i: usize) -> Doc
	{
		Doc::Row(Rc::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name} |
			self.name().as_ref().map_or(Doc::Empty, |name| Doc::String(Rc::new(name.clone()))),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::String(Rc::new(summ.clone()))),
			// | {exprs} |
			Doc::SepByDoc(
				Rc::new(Doc::Br),
				Rc::new(self.exprs()
					.iter()
					.map(|x| Doc::Code(Rc::new(
						Doc::Fragment(Rc::new(vec![
							x.to_doc(),
							Doc::Static(";"),
						]))
					)))
					.collect::<Vec<_>>()
				),
			),
			// | {desc} |
			self.description()
				.as_ref()
				.map_or(
					Doc::Empty,
					|desc| Doc::Cell(Rc::new(
						Doc::String(Rc::new(desc.clone()))
					))
				),
		]))
	}
}

impl HeaderDoc for ast::TransitionField
{
	fn header_doc() -> Doc
	{
		Doc::HeaderRow(Rc::new(vec![
			Doc::Static("#"),
			Doc::Static("Event<br />(Name)"),
			Doc::Static("Event<br />(Summary)"),
			Doc::Static("Guard<br />(Expr)"),
			Doc::Static("Guard<br />(Description)"),
			Doc::Static("Post cond<br />(Target)"),
			Doc::Static("Post cond<br />(Expr)"),
			Doc::Static("Transition"),
			Doc::Static("Post cond<br />(Description)"),
			Doc::Static("Description"),
		]))
	}
}

impl ToDocWithIndexAndModule for ast::TransitionField
{
	fn to_doc(&self, i: usize, module: &ast::Module) -> Doc
	{
		Doc::Fragment(Rc::new(
			self.posts()
				.iter()
				.enumerate()
				.map(|(j, post)| transition_row(self, i, module, j, post))
				.collect::<Vec<_>>()
		))
	}
}

fn transition_row(me: &ast::TransitionField, i: usize, module: &ast::Module, j: usize, post: &ast::PostCond) -> Doc
{
	let first = j == 0;
	Doc::Row(Rc::new(vec![
		// | {i} |
		if first { Doc::Number(i) } else { Doc::Empty },
		// | {name}{args} |
		if first { name_args_to_doc(me.name(), me.args()) } else { Doc::Empty },
		// | {summary} |
		if first {
			module.get_event_summary(me.name())
				.map_or(Doc::Empty, |summ|
					Doc::String(Rc::new(summ.clone()))
				)
		}else
		{
			Doc::Empty
		},
		// | {guard_expr} |
		if first {
			me.guard().as_ref().map_or(
					Doc::Empty,
					|x| Doc::Code(Rc::new(x.0.to_doc())),
				)
		}else
		{
			Doc::Empty
		},
		// | {guard_desc} |
		if first {
			Doc::Cell(Rc::new(
				me.guard().as_ref().map_or(
					Doc::Empty,
					|guard| (*guard.1).as_ref().map_or(
						Doc::Empty,
						|desc| Doc::Cell(Rc::new(Doc::String(Rc::new(desc.clone())))),
					),
				),
			))
		}else
		{
			Doc::Empty
		},
		// | {post_targets} |
		Doc::SepBy(
			", ",
			Rc::new(post.targets().iter()
				.map(|target| Doc::String(Rc::new(target.clone())))
				.collect::<Vec<_>>()),
		),
		// | {post_expr} |
		Doc::SepBy(
			"<br />",
			Rc::new(post.exprs()
				.iter()
				.filter(|x| !x.is_transition())
				.map(|x| Doc::Code(Rc::new(Doc::Fragment(Rc::new(vec![
					x.to_doc(),
					Doc::Static(";"),
				])))))
				.collect::<Vec<_>>()),
		),
		// | {transition} |
		Doc::SepBy(
			"<br />",
			Rc::new(post.exprs()
				.iter()
				.filter(|x| x.is_transition())
				.map(|x| Doc::Code(Rc::new(Doc::Fragment(Rc::new(vec![
					x.to_doc(),
					Doc::Static(";"),
				])))))
				.collect::<Vec<_>>()),
		),
		// | {post_desc} |
		post.description().as_ref().map_or(
			Doc::Empty,
			|desc| Doc::Cell(Rc::new(Doc::String(Rc::new(desc.clone())))),
		),
		// | {desc} |
		if first {
			me.description().as_ref().map_or(
				Doc::Empty,
				|desc| Doc::Cell(Rc::new(Doc::String(Rc::new(desc.clone())))),
			)
		}else
		{
			Doc::Empty
		},
	]))
}


fn name_args_to_doc(name: &ast::DottedName, args: &ast::ArgList) -> Doc
{
	if args.args().is_empty()
	{
		name.to_doc()
	}else
	{
		Doc::Fragment(Rc::new(vec![
			name.to_doc(),
			Doc::Static(" "),
			Doc::SepBy(" ", Rc::new(args.args().iter().map(|arg| arg.to_doc()).collect::<Vec<_>>())),
		]))
	}
}


impl ToDocIfHasOr for ast::ArgList
{
	fn to_doc_if_has_or(&self, default: Doc) -> Doc
	{
		if (*self.args()).is_empty()
		{
			default
		}else
		{
			Doc::SepBy(" ", Rc::new(self.args().iter().map(|arg|
				arg.to_doc()
			).collect::<Vec<_>>()))
		}
	}
}


impl ToDoc for ast::Arg
{
	fn to_doc(&self) -> Doc
	{
		match self.typ()
		{
			Some(typ) =>
				Doc::Fragment(Rc::new(vec![
					Doc::Static("("),
					self.name().to_doc(),
					Doc::Static(" : "),
					typ.to_doc(),
					Doc::Static(")"),
				])),
			None =>
				self.name().to_doc(),
		}
	}
}


impl ToDoc for ast::VarType
{
	fn to_doc(&self) -> Doc
	{
		match self
		{
		&ast::VarType::Var => Doc::Static("var"),
		&ast::VarType::Const => Doc::Static("const"),
		}
	}
}

impl HeaderDoc for ast::VarStmt
{
	fn header_doc() -> Doc
	{
		Doc::Static(concat![
			"| # | Name | Type | Init | Summary | Description |\n",
			"|---|------|------|------|---------|-------------|\n",
		])
	}
}

impl ToDocWithIndex for ast::VarStmt
{
	fn to_doc(&self, i: usize) -> Doc
	{
		Doc::Row(Rc::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {var_type} {name} |
			Doc::SepBy(" ", Rc::new(vec![
				self.var_type().to_doc(),
				self.matcher().to_doc(),
			])),
			// | `{type}` |
			self.typ().as_ref().map_or(Doc::Empty, |typ| Doc::Code(Rc::new(typ.to_doc()))),
			// | `{init}` |
			self.init().as_ref().map_or(Doc::Empty, |init| Doc::Code(Rc::new(init.to_doc()))),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::String(Rc::new(summ.clone()))),
			// | {desc} |
			Doc::Cell(Rc::new(self.description().as_ref().map_or(Doc::Empty, |desc| Doc::String(Rc::new(desc.clone()))))),
		]))
	}
}

impl ToDoc for ast::PostCondItem
{
	fn to_doc(&self) -> Doc
	{
		match self
		{
			ast::PostCondItem::Mutation(m) => m.to_doc(),
			ast::PostCondItem::Expr(expr) => expr.to_doc(),
		}
	}
}

impl IsTransition for ast::PostCondItem
{
	fn is_transition(&self) -> bool
	{
		match self
		{
			ast::PostCondItem::Mutation(m) => m.is_transition(),
			ast::PostCondItem::Expr(expr) => {
				match expr.as_ref()
				{
					ast::Expr::Cmp(cmp) => {
						cmp.op() == "==" && match &cmp.lhs() {
							ast::Expr::Name(name) => name.name() == "state'",
							_ => false,
						}
					},
					_ =>
						false,
				}
			},
		}
	}
}

impl IsTransition for ast::Mutation
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

impl ToDoc for ast::Mutation
{
	fn to_doc(&self) -> Doc
	{
		Doc::Fragment(Rc::new(vec![
			self.lhs().to_doc(),
			match self.ltype() {
				Some(ltype) => Doc::Fragment(Rc::new(vec![
						Doc::Static(" : "),
						ltype.to_doc(),
					])),
				None => Doc::Empty,
			},
			Doc::Static(" = "),
			self.rhs().to_doc(),
		]))
	}
}


impl ToDoc for ast::Expr
{
	fn to_doc(&self) -> Doc
	{
		match self
		{
		ast::Expr::Or(x) => x.to_doc(),
		ast::Expr::And(x) => x.to_doc(),
		ast::Expr::Cmp(x) => x.to_doc(),
		ast::Expr::Dotted(x) => x.to_doc(),
		ast::Expr::Appli(x) => x.to_doc(),
		ast::Expr::Name(x) => x.to_doc(),
		ast::Expr::Num(x) => x.to_doc(),
		ast::Expr::Str(x) => x.to_doc(),
		ast::Expr::Case(x) => x.to_doc(),
		ast::Expr::If(x) => x.to_doc(),
		ast::Expr::Nil => Doc::Static("[]"),
		ast::Expr::List(x) => x.to_doc(),
		ast::Expr::ListComprehension(x) => x.to_doc(),
		ast::Expr::Set(x) => x.to_doc(),
		ast::Expr::Map(x) => x.to_doc(),
		ast::Expr::Paren(x) => Doc::Fragment(Rc::new(vec![
				Doc::Static("("),
				x.to_doc(),
				Doc::Static(")"),
			])),
		ast::Expr::EventSet(cset) => Doc::Fragment(Rc::new(vec![
				Doc::Static("__event_set {"),
				Doc::SepBy(", ", Rc::new(cset.iter().map(|chan| chan.to_doc()).collect::<Vec<_>>())),
				Doc::Static("}"),
			])),
		ast::Expr::ChannelSet(cset) => Doc::Fragment(Rc::new(vec![
				Doc::Static("{|"),
				Doc::SepBy(", ", Rc::new(cset.iter().map(|chan| chan.to_doc()).collect::<Vec<_>>())),
				Doc::Static("|}"),
			])),
		ast::Expr::TargetSet(tset) => Doc::Fragment(Rc::new(vec![
				Doc::Static("|["),
				Doc::SepBy(", ", Rc::new(tset.iter().map(|target| target.to_doc()).collect::<Vec<_>>())),
				Doc::Static("]|"),
			])),
		ast::Expr::BinOp(x) => x.to_doc(),
		ast::Expr::Let(x) => x.to_doc(),
		ast::Expr::RecordMutation(x) => x.to_doc(),
		ast::Expr::Quantifier(x) => x.to_doc(),
		ast::Expr::Fn(x) => x.to_doc(),
		ast::Expr::Any => Doc::Static("any"),
		ast::Expr::State => Doc::Static("state"),
		}
	}
}

impl ToDoc for ast::OrExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			self.lhs().to_doc(),
			Doc::Static("||"),
			self.rhs().to_doc(),
		]))
	}
}

impl ToDoc for ast::AndExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			self.lhs().to_doc(),
			Doc::Static("&&"),
			self.rhs().to_doc(),
		]))
	}
}

impl ToDoc for ast::CmpExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			self.lhs().to_doc(),
			Doc::String(Rc::new( self.op().clone() )),
			self.rhs().to_doc(),
		]))
	}
}

impl ToDoc for ast::AppliExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			self.lhs().to_doc(),
			self.rhs().to_doc(),
		]))
	}
}

impl ToDoc for ast::NameExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::String(Rc::new( self.name().clone() ))
	}
}

impl ToDoc for ast::NumExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::String(Rc::new( format!("{}", self.num()) ))
	}
}

impl ToDoc for ast::StrExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::Fragment(Rc::new(vec![
			Doc::String(Rc::new(self.begin().clone())),
			Doc::String(Rc::new(self.value().clone())),
			Doc::String(Rc::new(self.end().clone())),
		]))
	}
}

impl ToDoc for ast::BinOpExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			self.lhs().to_doc(),
			Doc::String(Rc::new( self.op().clone() )),
			self.rhs().to_doc(),
		]))
	}
}

impl ToDoc for ast::DottedExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::Fragment(Rc::new(vec![
			self.lhs().to_doc(),
			Doc::Static("."),
			self.rhs().to_doc(),
		]))
	}
}


impl ToDoc for ast::CaseExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			vec![
				Doc::Static("case"),
				self.expr().to_doc(),
				Doc::Static("{"),
			],
			self.elems().iter().flat_map(|x|
					vec![
						vec![ x.cond().to_doc() ],
						x.guard().map_or(vec![], |x| vec![
								Doc::Static("when"),
								x.to_doc(),
							]),
						vec![
							Doc::Static("=>"),
							x.body().to_doc(),
							Doc::Static(";"),
						],
					].concat()
				).collect::<Vec<_>>(),
			vec![
				Doc::Static("}"),
			],
		].concat()))
	}
}


impl ToDoc for ast::IfExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			vec![
				Doc::Static("if"),
				self.cond().to_doc(),
				Doc::Static("{"),
				self.then_expr().to_doc(),
				Doc::Static("}"),
			],
			self.else_expr().as_ref().map_or(
				Vec::new(),
				|else_expr| vec![
						Doc::Static("else"),
						Doc::Static("{"),
						else_expr.to_doc(),
						Doc::Static("}"),
					],
			),
		].concat()))
	}
}


impl ToDoc for ast::ListExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			Doc::Static("["),
			Doc::SepEndBy(", ", ",", Rc::new(
				self.elems().iter().map(|elem| elem.to_doc()).collect::<Vec<_>>(),
			)),
			Doc::Static("]"),
		]))
	}
}


impl ToDoc for ast::ListComprehensionExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			Doc::Static("["),
			self.expr().to_doc(),
			Doc::Static("|"),
			Doc::SepEndBy(", ", ",", Rc::new(self.elems()
					.iter()
					.map(|elem| elem.to_doc())
					.collect::<Vec<_>>()),
			),
			Doc::Static("]"),
		]))
	}
}


impl ToDoc for ast::SetExpr
{
	fn to_doc(&self) -> Doc
	{
		if self.elems().is_empty()
		{
			Doc::Static("__set {}")
		}else
		{
			Doc::SepBy(" ", Rc::new(vec![
				Doc::Static("__set"),
				Doc::Static("{"),
				Doc::SepEndBy(", ", ",", Rc::new(
					self.elems().iter().map(|elem| elem.to_doc()).collect::<Vec<_>>(),
				)),
				Doc::Static("}"),
			]))
		}
	}
}


impl ToDoc for ast::MapExpr
{
	fn to_doc(&self) -> Doc
	{
		if self.elems().is_empty()
		{
			Doc::Static("__map {}")
		}else
		{
			Doc::SepBy(" ", Rc::new(vec![
				Doc::Static("__map"),
				Doc::Static("{"),
				Doc::SepEndBy(", ", ",", Rc::new(
					self.elems().iter().map(|elem| elem.to_doc()).collect::<Vec<_>>(),
				)),
				Doc::Static("}"),
			]))
		}
	}
}


impl ToDoc for ast::MapItem
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			self.key().to_doc(),
			Doc::Static("|->"),
			self.value().to_doc(),
		]))
	}
}


impl ToDoc for ast::RecordMutation
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			match self.style()
			{
				ast::RecordStyle::Inside =>
					vec![
						Doc::Static("{"),
						self.expr().to_doc(),
						Doc::Static("|"),
					],
				ast::RecordStyle::Outside =>
					vec![
						self.expr().to_doc(),
						Doc::Static("{"),
					],
			},
			vec![
				Doc::SepEndBy(", ", ",", Rc::new(self.mutations().iter().map(|m|
					Doc::SepBy(" ", Rc::new(vec![
						m.lhs().to_doc(),
						Doc::String(Rc::new(m.op().clone())),
						m.rhs().to_doc(),
					]))
				).collect::<Vec<_>>())),
				Doc::Static("}"),
			],
		].concat()))
	}
}


impl ToDoc for ast::LetExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			Doc::Static("let"),
			Doc::SepBy(", ", Rc::new(self.mutations().iter().map(|mutation|
				mutation.to_doc()
			).collect::<Vec<_>>())),
			Doc::Static("in"),
			self.expr().to_doc(),
		]))
	}
}


impl ToDoc for ast::QuantifierExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			match self.kind()
			{
				ast::QuantifierKind::Exists => Doc::Static("exists"),
				ast::QuantifierKind::Exists1 => Doc::Static("exists1"),
				ast::QuantifierKind::ForAll => Doc::Static("forall"),
			},
			Doc::SepBy(", ", Rc::new(
				self.exprs().iter().map(|expr| expr.to_doc()).collect::<Vec<_>>(),
			)),
			Doc::Static("|"),
			self.cond().to_doc(),
		]))
	}
}


impl ToDoc for ast::FnExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Rc::new(vec![
			vec![ Doc::Static("fn") ],
			self.args().args().iter().map(|x| x.to_doc()).collect::<Vec<_>>(),
			match self.typ()
			{
				Some(typ) => vec![
						Doc::Static(":"),
						typ.to_doc(),
					],
				None => Vec::new(),
			},
			match self.body()
			{
				Some(body) => vec![
					Doc::Static("->"),
					body.to_doc(),
				],
				None => Vec::new(),
			},
		].concat()))
	}
}
