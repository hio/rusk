use crate::ast;
use crate::message::Message;
use serde::{Serialize, Deserialize};


// TODO: write reason for using rc rather than box.
// serialize/deserialize rc with "--features rc" makes shared
// objects unshared.
// https://serde.rs/feature-flags.html#-features-rc
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Doc
{
	Empty,
	Title(Box<String>),
	Heading(usize, Box<Doc>),
	Fragment(Box<Vec<Doc>>),
	Number(usize),
	Plain(Box<String>),
	Marked(Box<String>),
	Static(&'static str),
	Br,
	Hr,
	Code(Box<Doc>),
	SepBy(&'static str, Box<Vec<Doc>>),
	SepByDoc(Box<Doc>, Box<Vec<Doc>>),
	SepEndBy(&'static str, &'static str, Box<Vec<Doc>>),
	Table(Box<Vec<Doc>>, Box<Vec<Box<Vec<Doc>>>>),
	Cell(Box<Doc>),
	BlockQuote(Box<Doc>),
}


trait ToDoc
{
	fn to_doc(&self) -> Doc;
}

trait ToDocRow
{
	fn to_doc_row(&self, text: &Message, i: usize) -> Box<Vec<Doc>>;
}

pub trait ToDocWithTitle
{
	fn to_doc(&self, title: &String, text: &Message) -> Doc;
}

pub trait ToDocWithModule
{
	fn to_doc(&self, text: &Message, module: &ast::Module) -> Doc;
}

trait ToDocRowsWithModule
{
	fn to_doc_rows(&self, index: usize, module: &ast::Module) -> Vec<Box<Vec<Doc>>>;
}

trait ToDocIfHasOr
{
	fn to_doc_if_has_or(&self, _default: Doc) -> Doc;
}

trait HeaderRow
{
	fn header_row(text: &Message) -> Box<Vec<Doc>>;
}


trait IsTransition
{
	fn is_transition(&self) -> bool;
}


impl crate::formatter::json::ToJsonText for Doc
{
	fn to_json_text(&self) -> serde_json::Result<String>
	{
		serde_json::to_string(self)
	}

	fn to_json_text_pretty(&self) -> serde_json::Result<String>
	{
		serde_json::to_string_pretty(self)
	}
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
	fn to_doc(&self, title: &String, text: &Message) -> Doc
	{
		Doc::Fragment(Box::new(vec![
			Doc::Title(Box::new(title.clone())),
			Doc::Static("\n"),

			if_has_or_(
				self.types(),
				"",
				|items| Doc::Fragment(Box::new(vec![
						Doc::Heading(2, Box::new(Doc::Plain(text.h_types()))),
						Doc::Static("\n"),
						Doc::Table(
							ast::TypeStmt::header_row(text),
							Box::new(items
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc_row(text, i + 1))
								.collect::<Vec<_>>()),
						),
						Doc::Static("\n"),
					])),
			),

			if_has_or_(
				self.events(),
				"",
				|items| Doc::Fragment(Box::new(vec![
						Doc::Heading(2, Box::new(Doc::Plain(text.h_events()))),
						Doc::Static("\n"),
						Doc::Table(
							ast::Event::header_row(text),
							Box::new(items
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc_row(text, i + 1))
								.collect::<Vec<_>>()),
						),
						Doc::Static("\n"),
					])),
			),

			match self.vars().is_empty() {
				true => Doc::Empty,
				false => {
					Doc::Fragment(Box::new(vec![
						Doc::Heading(2, Box::new(Doc::Plain(text.h_module_variables()))),
						Doc::Static("\n"),
						Doc::Table(
							ast::VarStmt::header_row(text),
							Box::new(self.vars()
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc_row(text, i + 1))
								.collect::<Vec<_>>()),
						),
						Doc::Static("\n"),
					]))
				},
			},

			match self.functions().is_empty() {
				true => Doc::Empty,
				false => Doc::Fragment(Box::new(vec![
					Doc::Heading(2, Box::new(Doc::Plain(text.h_module_functions()))),
					Doc::Static("\n"),
					Doc::Fragment(Box::new(vec![
						Doc::Table(
							ast::FnStmt::header_row(text),
							Box::new(self.functions()
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc_row(text, i + 1))
								.collect::<Vec<_>>()),
						),
					])),
					Doc::Static("\n"),
				])),
			},

			match self.invariants().is_empty() {
				true => Doc::Empty,
				false => Doc::Fragment(Box::new(vec![
					Doc::Heading(2, Box::new(Doc::Plain(text.h_module_invariants()))),
					Doc::Static("\n"),
					Doc::Fragment(Box::new(vec![
						Doc::Table(
							ast::InvariantField::header_row(text),
							Box::new(self.invariants()
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc_row(text, i + 1))
								.collect::<Vec<_>>()),
						),
					])),
					Doc::Static("\n"),
				])),
			},

			Doc::SepBy("\n", Box::new(self.states()
				.iter()
				.map(|state| state.to_doc(text, self))
				.collect::<Vec<_>>()
			)),
		]))
	}
}


impl HeaderRow for ast::TypeStmt
{
	fn header_row(text: &Message) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			Doc::Static("#"),
			Doc::Plain(text.th_name()),
			Doc::Plain(text.th_alias()),
			Doc::Plain(text.th_definision()),
			Doc::Plain(text.th_description()),
		])
	}
}


impl ToDocRow for ast::TypeStmt
{
	fn to_doc_row(&self, _text: &Message, i: usize) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name}{args} |
			name_args_to_doc(Some(self.name()), self.args()),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::Marked(Box::new(summ.as_ref().clone()))),
			// | {definition} |
			if self.items().len() == 1
			{
				Doc::Cell(Box::new(self.items()[0].to_doc()))
			}else
			{
				Doc::Cell(Box::new(Doc::SepByDoc(
					Box::new(Doc::Fragment(Box::new(vec![
						Doc::Static(" |"),
						Doc::Br,
					]))),
					Box::new(
						self.items().iter().map(|item| item.to_doc()).collect::<Vec<_>>()
					)
				)))
			},
			// | {desc} |
			Doc::Cell(Box::new(self.description().as_ref().map_or(Doc::Empty, |desc| to_marked_block(desc)))),
		])
	}
}


fn to_marked_block(text: &String) -> Doc
{
	let mut lines = text
		.replace("\t", "    ")
		.trim_end()
		.split('\n')
		.map(|s| s.into())
		.collect::<Vec<String>>();

	// drop leading empty lines.
	while !lines.is_empty()
	{
		if lines[0].chars().any(|c| !c.is_whitespace())
		{
			break;
		}
		lines.remove(0);
	}

	// drop longest common indent.
	while !lines[0].is_empty()
	{
		// break if first char at first line is not a whitespace.
		let c0 = match lines[0].chars().next() {
			Some(c0) => c0,
			None => break,
		};
		if !c0.is_whitespace()
		{
			break;
		}

		// break if not common.
		if lines.iter().any(|line| line.chars().next() != Some(c0))
		{
			break;
		}

		for line in lines.iter_mut()
		{
			line.remove(0);
		}
	}

	for line in lines.iter_mut()
	{
		*line += "\n";
	}

	Doc::Marked(Box::new(lines.concat()))
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
		Doc::SepBy(" ", Box::new(vec![
			vec![ Doc::Plain(Box::new(self.name().clone())) ],
			self.args().args().iter().map(|arg| arg.to_doc()).collect::<Vec<_>>(),
		].concat()))
	}
}


impl ToDoc for ast::RecordDef
{
	fn to_doc(&self) -> Doc
	{
		Doc::Fragment(Box::new(vec![
			self.name().as_ref().map_or(Doc::Empty, |name| Doc::Plain(Box::new(*name.clone() + " "))),
			self.fields().to_doc()
		]))
	}
}


impl ToDoc for Vec<Box<ast::RecordField>>
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepByDoc(Box::new(Doc::Br), Box::new(vec![
			vec![ Doc::Static("{"), ],
			self.iter()
				.map(|field| Doc::Fragment(Box::new(vec![
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

fn to_summary_doc(summ: &Box<String>) -> Doc
{
	Doc::Fragment(Box::new(vec![
		Doc::Static(" @( "),
		Doc::Marked(Box::new(summ.as_ref().clone())),
		Doc::Static(" )"),
	]))
}

fn to_description_doc(desc: &Box<String>) -> Doc
{
	Doc::Fragment(Box::new(vec![
		Doc::Static(" @{- "),
		Doc::Marked(Box::new(desc.as_ref().clone())),
		Doc::Static(" -}"),
	]))
}

impl HeaderRow for ast::Event
{
	fn header_row(_text: &Message) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			Doc::Static("#"),
			Doc::Static("Name"),
			Doc::Static("Summary"),
			Doc::Static("Description"),
		])
	}
}


impl ToDocRow for ast::EventItem
{
	fn to_doc_row(&self, _text: &Message, i: usize) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name}{args} |
			name_args_to_doc(Some(self.name()), self.args()),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::Marked(Box::new(summ.as_ref().clone()))),
			// | {desc} |
			Doc::Cell(Box::new(self.description().as_ref().as_ref().map_or(Doc::Empty, |desc| to_marked_block(desc)))),
		])
	}
}


impl ToDoc for ast::DottedName
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(".", Box::new(
			self.names().iter().map(|name|
				Doc::Plain(Box::new(name.clone()))
			).collect::<Vec<_>>()
		))
	}
}


impl HeaderRow for ast::FnStmt
{
	fn header_row(_text: &Message) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			Doc::Static("#"),
			Doc::Static("Name"),
			Doc::Static("Summary"),
			Doc::Static("Type"),
			Doc::Static("Body"),
			Doc::Static("Description"),
		])
	}
}


impl ToDocRow for ast::FnStmt
{
	fn to_doc_row(&self, _text: &Message, i: usize) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name} |
			name_args_to_doc(Some(&ast::DottedName::new_boxed(vec![self.name().clone()])), self.args()),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::Marked(Box::new(summ.as_ref().clone()))),
			// | {type} |
			self.typ().as_ref().map_or(Doc::Empty, |typ| typ.to_doc()),
			// | {body} |
			self.body().as_ref().map_or(Doc::Empty, |body| body.to_doc()),
			// | {desc} |
			self.description()
				.as_ref()
				.map_or(
					Doc::Empty,
					|desc| Doc::Cell(Box::new(to_marked_block(desc)))
				),
		])
	}
}


impl ToDocWithModule for ast::State
{
	fn to_doc(&self, text: &Message, module: &ast::Module) -> Doc
	{
		Doc::Fragment(Box::new(vec![
			// "## state {name_args} @ {summary}\n",
			Doc::Heading(2, Box::new(Doc::Fragment(Box::new(vec![
				Doc::Static("state "),
				{
					let name = ast::DottedName::new_boxed(vec![self.name().clone()]);
					name_args_to_doc(Some(name.as_ref()), self.args())
				},
				match self.summary() {
					Some(summ) => Doc::Fragment(Box::new(vec![
						Doc::Static(" @ "),
						Doc::Marked((summ as &String).clone().into()),
					])),
					None => Doc::Empty,
				},
			])))),

			// description.
			match self.description() {
				Some(desc) =>
					Doc::Fragment(Box::new(vec![
						Doc::Static("\n"),
						to_marked_block(desc),
					])),
				None => Doc::Empty,
			},

			if_has_field_or_(
				self.fields(),
				|field| field.get_var(),
				|vars| Doc::Fragment(Box::new(vec![
						Doc::Static("\n"),
						Doc::Heading(3, Box::new(Doc::Static("Variables"))),
						Doc::Static("\n"),
						Doc::Table(
							ast::VarField::header_row(text),
							Box::new(
								vars
									.iter()
									.enumerate()
									.map(|(i, x)| x.to_doc_row(text, i + 1))
									.collect::<Vec<_>>(),
							),
						),
					])),
				"",
			),

			if_has_field_or_(
				self.fields(),
				|field| field.get_invariant(),
				|vec| Doc::Fragment(Box::new(vec![
						Doc::Static("\n"),
						Doc::Heading(3, Box::new(Doc::Static("Invariants"))),
						Doc::Static("\n"),
						Doc::Table(
							ast::InvariantField::header_row(text),
							Box::new(vec
								.iter()
								.enumerate()
								.map(|(i, x)| x.to_doc_row(text, i + 1))
								.collect::<Vec<_>>()),
						),
					])),
				"",
			),

			if_has_field_or_(
				self.fields(),
				|field| field.get_transition(),
				|transitions| Doc::Fragment(Box::new(vec![
						Doc::Static("\n"),
						Doc::Heading(3, Box::new(Doc::Static("Transitions"))),
						Doc::Static("\n"),
						Doc::Table(
							ast::TransitionField::header_row(text),
							Box::new(
								transitions
									.iter()
									.enumerate()
									.map(|(i, x)| x.to_doc_rows(i + 1, module))
									.collect::<Vec<_>>()
									.concat(),
							),
						),
					])),
				"",
			),

			if_has_field_or_(
				self.fields(),
				|field| field.get_tau(),
				|taus| Doc::Fragment(Box::new(vec![
						Doc::Static("\n"),
						Doc::Heading(3, Box::new(Doc::Static("Taus"))),
						Doc::Static("\n"),
						Doc::Table(
							ast::Tau::header_row(text),
							Box::new(
								taus
									.iter()
									.enumerate()
									.map(|(i, x)| x.to_doc_rows(i + 1, module))
									.collect::<Vec<_>>()
									.concat(),
							),
						),
					])),
				"",
			),
		]))
	}
}


impl HeaderRow for ast::VarField
{
	fn header_row(_text: &Message) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			Doc::Static("#"),
			Doc::Static("Name"),
			Doc::Static("Type"),
			Doc::Static("Init"),
			Doc::Static("Summary"),
			Doc::Static("Description"),
		])
	}
}


impl ToDocRow for ast::VarField
{
	fn to_doc_row(&self, _text: &Message, i: usize) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name} |
			self.name().to_doc(),
			// | `{type}` |
			self.typ().as_ref().map_or(Doc::Empty, |typ| Doc::Code(Box::new(typ.to_doc()))),
			// | `{init}` |
			self.init().as_ref().map_or(Doc::Empty, |init| Doc::Code(Box::new(init.to_doc()))),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::Marked(Box::new(summ.as_ref().clone()))),
			// | {desc} |
			Doc::Cell(Box::new(self.description().as_ref().map_or(Doc::Empty, |desc| to_marked_block(desc)))),
		])
	}
}


impl HeaderRow for ast::InvariantField
{
	fn header_row(_text: &Message) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			Doc::Static("#"),
			Doc::Static("Name"),
			Doc::Static("Summary"),
			Doc::Static("Logical Formula"),
			Doc::Static("Description"),
		])
	}
}


impl ToDocRow for ast::InvariantField
{
	fn to_doc_row(&self, _text: &Message, i: usize) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {name} |
			self.name().as_ref().map_or(Doc::Empty, |name| Doc::Plain(Box::new(name.clone()))),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::Marked(Box::new(summ.as_ref().clone()))),
			// | {exprs} |
			Doc::SepByDoc(
				Box::new(Doc::Br),
				Box::new(self.exprs()
					.iter()
					.map(|x| Doc::Code(Box::new(
						Doc::Fragment(Box::new(vec![
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
					|desc| Doc::Cell(Box::new(to_marked_block(desc))),
				),
		])
	}
}

impl HeaderRow for ast::TransitionField
{
	fn header_row(_text: &Message) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			Doc::Static("#"),
			Doc::Static("Event\n(Name)"),
			Doc::Static("Event\n(Summary)"),
			Doc::Static("Guard"),
			Doc::Static("Post cond"),
			Doc::Static("Description"),
		])
	}
}

impl ToDocRowsWithModule for ast::TransitionField
{
	fn to_doc_rows(&self, i: usize, module: &ast::Module) -> Vec<Box<Vec<Doc>>>
	{
		let len = self.posts().len().max(1);

		(0..len).map(|j| {
			let post = self.posts().get(j);
			transition_row(self, i, module, j, post)
		}).collect::<Vec<_>>()
	}
}

fn transition_row(me: &ast::TransitionField, i: usize, module: &ast::Module, j: usize, post: Option<&Box<ast::PostCond>>) -> Box<Vec<Doc>>
{
	let first = j == 0;
	Box::new(vec![
		// | {i} |
		if first { Doc::Number(i) } else { Doc::Empty },
		// | {name}{args} |
		if first { name_args_to_doc(me.name().as_ref().map(|name|name.as_ref()), me.args()) } else { Doc::Empty },
		// | {summary} |
		if first {
			match me.name() {
				Some(name) => match module.get_event_summary(name) {
					Some(summ) => Doc::Marked(Box::new(summ.clone())),
					None => Doc::Empty,
				},
				None => Doc::Empty,
			}
		}else
		{
			Doc::Empty
		},
		// | {guard_expr}/{guard_desc} |
		if first {
			guards_to_cell(me.guards())
		}else
		{
			Doc::Empty
		},
		// | {post_targets} |
		// | {post_expr} |
		// | {transition} |
		// | {post_desc} |
		match post {
			Some(post) =>
				Doc::SepByDoc(Box::new(Doc::Hr), Box::new(vec![
					// targets/exprs/transitions.
					if post.items().is_empty()
					{
						vec![]
					}else
					{
						vec![
							if post.targets().is_empty()
							{
								vec![]
							}else
							{
								vec![
									Doc::Fragment(Box::new(vec![
										Doc::Static("target "),
										Doc::SepBy(
											", ",
											Box::new(post.targets().iter()
												.map(|target| Doc::Plain(Box::new(target.clone())))
												.collect::<Vec<_>>()),
										),
										Doc::Static(";\n"),
									])),
								]
							},

							post.items()
								.iter()
								.map(|item| {
									let code = Doc::Fragment(Box::new(vec![
										Doc::Code(Box::new(item.expr().to_doc())),
										Doc::Static("\n"),
									]));
									match item.description() {
										Some(desc) =>
											Doc::Fragment(Box::new(vec![
												code,
												Doc::Static("\n"),
												Doc::BlockQuote(Box::new(to_marked_block(desc))),
											])),
										None =>
											code,
									}
								})
								.collect::<Vec<_>>(),
						].concat()
					},
					// description.
					vec![
						post.description().as_ref().map_or(
							Doc::Empty,
							|desc| Doc::Cell(Box::new(to_marked_block(desc))),
						),
					],
				].concat())),
			None =>
				Doc::Empty,
		},
		// | {desc} |
		if first {
			me.description().as_ref().map_or(
				Doc::Empty,
				|desc| Doc::Cell(Box::new(to_marked_block(desc))),
			)
		}else
		{
			Doc::Empty
		},
	])
}


fn guards_to_cell(guards: &Vec<Box<ast::GuardExpr>>) -> Doc
{
	match guards.len()
	{
		0 => Doc::Empty,
		1 => Doc::Cell(Box::new(guard_to_cell(guards[0].as_ref()))),
		_ => Doc::Cell(Box::new(Doc::Fragment(Box::new(vec![
			Doc::Static("Satisfy all the following conditions:\n"),
			Doc::Hr,
			Doc::SepByDoc(
				Box::new(Doc::Fragment(Box::new(vec![
					Doc::Hr,
				]))),
				Box::new(guards.iter().map(|guard| guard_to_cell(guard)).collect::<Vec<_>>()),
			),
		])))),
	}
}


fn guard_to_cell(guard: &ast::GuardExpr) -> Doc
{
	match guard.description() {
		Some(desc) => Doc::Fragment(Box::new(vec![
				Doc::Code(Box::new(guard.expr().to_doc())),
				Doc::Static("\n"),
				Doc::Static("\n"),
				Doc::BlockQuote(Box::new(to_marked_block(desc))),
			])),
		None =>
			Doc::Fragment(Box::new(vec![
				Doc::Code(Box::new(guard.expr().to_doc())),
				Doc::Static("\n"),
			])),
	}
}



fn name_args_to_doc(name: Option<&ast::DottedName>, args: &ast::ArgList) -> Doc
{
	let v = vec![
		match name
		{
			Some(name) => vec![ name.to_doc(), ],
			None => vec![],
		},
		args.args().iter().map(|arg| arg.to_doc()).collect::<Vec<_>>(),
	].concat();
	if v.is_empty()
	{
		Doc::Empty
	}else
	{
		Doc::SepBy(" ", Box::new(v))
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
			Doc::SepBy(" ", Box::new(self.args().iter().map(|arg|
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
				Doc::Fragment(Box::new(vec![
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

impl HeaderRow for ast::VarStmt
{
	fn header_row(text: &Message) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			Doc::Static("#"),
			Doc::Plain(text.th_name()),
			Doc::Plain(text.th_type()),
			Doc::Plain(text.th_init()),
			Doc::Plain(text.th_summary()),
			Doc::Plain(text.th_description()),
		])
	}
}

impl ToDocRow for ast::VarStmt
{
	fn to_doc_row(&self, _text: &Message, i: usize) -> Box<Vec<Doc>>
	{
		Box::new(vec![
			// | {i} |
			Doc::Number(i),
			// | {var_type} {name} |
			Doc::SepBy(" ", Box::new(vec![
				self.var_type().to_doc(),
				self.matcher().to_doc(),
			])),
			// | `{type}` |
			self.typ().as_ref().map_or(Doc::Empty, |typ| Doc::Code(Box::new(typ.to_doc()))),
			// | `{init}` |
			self.init().as_ref().map_or(Doc::Empty, |init| Doc::Code(Box::new(init.to_doc()))),
			// | {summary} |
			self.summary().as_ref().map_or(Doc::Empty, |summ| Doc::Marked(Box::new(summ.as_ref().clone()))),
			// | {desc} |
			Doc::Cell(Box::new(self.description().as_ref().map_or(Doc::Empty, |desc| to_marked_block(desc)))),
		])
	}
}

impl ToDoc for ast::PostCondExpr
{
	fn to_doc(&self) -> Doc
	{
		match self
		{
			ast::PostCondExpr::Mutation(m) => m.to_doc(),
			ast::PostCondExpr::Expr(expr) => expr.to_doc(),
		}
	}
}

impl IsTransition for ast::PostCondExpr
{
	fn is_transition(&self) -> bool
	{
		match self
		{
			ast::PostCondExpr::Mutation(m) => m.is_transition(),
			ast::PostCondExpr::Expr(expr) => {
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
		Doc::Fragment(Box::new(vec![
			self.lhs().to_doc(),
			match self.ltype() {
				Some(ltype) => Doc::Fragment(Box::new(vec![
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


impl HeaderRow for ast::Tau
{
	fn header_row(text: &Message) -> Box<Vec<Doc>>
	{
		ast::TransitionField::header_row(text)
	}
}


impl ToDocRowsWithModule for ast::Tau
{
	fn to_doc_rows(&self, i: usize, module: &ast::Module) -> Vec<Box<Vec<Doc>>>
	{
		self.transition().to_doc_rows(i, module)
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
		ast::Expr::EmptyBrace => Doc::Static("{}"),
		ast::Expr::Set(x) => x.to_doc(),
		ast::Expr::Map(x) => x.to_doc(),
		ast::Expr::Paren(x) => Doc::Fragment(Box::new(vec![
				Doc::Static("("),
				x.to_doc(),
				Doc::Static(")"),
			])),
		ast::Expr::EventSet(cset) => Doc::Fragment(Box::new(vec![
				Doc::Static("__event_set {"),
				Doc::SepBy(", ", Box::new(cset.iter().map(|chan| chan.to_doc()).collect::<Vec<_>>())),
				Doc::Static("}"),
			])),
		ast::Expr::ChannelSet(cset) => Doc::Fragment(Box::new(vec![
				Doc::Static("{|"),
				Doc::SepBy(", ", Box::new(cset.iter().map(|chan| chan.to_doc()).collect::<Vec<_>>())),
				Doc::Static("|}"),
			])),
		ast::Expr::TargetSet(tset) => Doc::Fragment(Box::new(vec![
				Doc::Static("|["),
				Doc::SepBy(", ", Box::new(tset.iter().map(|target| target.to_doc()).collect::<Vec<_>>())),
				Doc::Static("]|"),
			])),
		ast::Expr::BinOp(x) => x.to_doc(),
		ast::Expr::Let(x) => x.to_doc(),
		ast::Expr::RecordMutation(x) => x.to_doc(),
		ast::Expr::RecordDef(x) => x.to_doc(),
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
		Doc::SepBy(" ", Box::new(vec![
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
		Doc::SepBy(" ", Box::new(vec![
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
		Doc::SepBy(" ", Box::new(vec![
			self.lhs().to_doc(),
			Doc::Plain(Box::new( self.op().clone() )),
			self.rhs().to_doc(),
		]))
	}
}

impl ToDoc for ast::AppliExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Box::new(vec![
			self.lhs().to_doc(),
			self.rhs().to_doc(),
		]))
	}
}

impl ToDoc for ast::NameExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::Plain(Box::new( self.name().clone() ))
	}
}

impl ToDoc for ast::NumExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::Plain(Box::new( format!("{}", self.num()) ))
	}
}

impl ToDoc for ast::StrExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::Fragment(Box::new(vec![
			Doc::Plain(Box::new(self.begin().clone())),
			Doc::Plain(Box::new(self.value().clone())),
			Doc::Plain(Box::new(self.end().clone())),
		]))
	}
}

impl ToDoc for ast::BinOpExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::SepBy(" ", Box::new(vec![
			self.lhs().to_doc(),
			Doc::Plain(Box::new( self.op().clone() )),
			self.rhs().to_doc(),
		]))
	}
}

impl ToDoc for ast::DottedExpr
{
	fn to_doc(&self) -> Doc
	{
		Doc::Fragment(Box::new(vec![
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
		Doc::SepBy(" ", Box::new(vec![
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
		Doc::SepBy(" ", Box::new(vec![
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
		Doc::SepBy(" ", Box::new(vec![
			Doc::Static("["),
			Doc::SepEndBy(", ", ",", Box::new(
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
		Doc::SepBy(" ", Box::new(vec![
			Doc::Static("["),
			self.expr().to_doc(),
			Doc::Static("|"),
			Doc::SepEndBy(", ", ",", Box::new(self.elems()
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
			Doc::SepBy(" ", Box::new(vec![
				Doc::Static("__set"),
				Doc::Static("{"),
				Doc::SepEndBy(", ", ",", Box::new(
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
			Doc::SepBy(" ", Box::new(vec![
				Doc::Static("__map"),
				Doc::Static("{"),
				Doc::SepEndBy(", ", ",", Box::new(
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
		Doc::SepBy(" ", Box::new(vec![
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
		Doc::SepBy(" ", Box::new(vec![
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
				Doc::SepEndBy(", ", ",", Box::new(self.mutations().iter().map(|m|
					Doc::SepBy(" ", Box::new(vec![
						m.lhs().to_doc(),
						Doc::Plain(Box::new(m.op().clone())),
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
		Doc::SepBy(" ", Box::new(vec![
			Doc::Static("let"),
			Doc::SepBy(", ", Box::new(self.mutations().iter().map(|mutation|
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
		Doc::SepBy(" ", Box::new(vec![
			match self.kind()
			{
				ast::QuantifierKind::Exists => Doc::Static("exists"),
				ast::QuantifierKind::Exists1 => Doc::Static("exists1"),
				ast::QuantifierKind::ForAll => Doc::Static("forall"),
			},
			Doc::SepBy(", ", Box::new(
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
		Doc::SepBy(" ", Box::new(vec![
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


pub fn text_width(s: &String) -> usize
{
	// lazy calculation.
	// consider UAX #11: East Asian Width.
	s.chars().map(|c| if c < '\u{0100}' { 1 } else { 2 } ).sum()
}
