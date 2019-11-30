//! # Abstract Syntax Tree
use serde::{Serialize, Deserialize};
use std::boxed::Box;


#[derive(Debug, Serialize, Deserialize)]
pub struct Module
{
	types: Box<Vec<Box<TypeStmt>>>,
	events: Box<Vec<Box<EventItem>>>,
	vars: Box<Vec<Box<VarStmt>>>,
	invariants: Box<Vec<Box<InvariantField>>>,
	states: Box<Vec<Box<State>>>,
}

impl Module
{
	pub fn new_boxed(types: Box<Vec<Box<TypeStmt>>>, events: Box<Vec<Box<EventItem>>>, vars: Box<Vec<Box<VarStmt>>>, states: Box<Vec<Box<State>>>, invariants: Box<Vec<Box<InvariantField>>>) -> Box<Module>
	{
		Box::new(Module {
			types,
			events,
			vars,
			states,
			invariants,
		})
	}

	pub fn types(&self) -> &Vec<Box<TypeStmt>>
	{
		&self.types
	}

	pub fn events(&self) -> &Vec<Box<EventItem>>
	{
		&self.events
	}

	pub fn vars(&self) -> &Vec<Box<VarStmt>>
	{
		&self.vars
	}

	pub fn invariants(&self) -> &Vec<Box<InvariantField>>
	{
		&self.invariants
	}

	pub fn states(&self) -> &Vec<Box<State>>
	{
		&self.states
	}

	pub fn get_event_summary(&self, name: &DottedName) -> Option<&String>
	{
		self.events
			.iter()
			.find(|item| item.name() == name)
			.and_then(|item| item.summary().as_ref())
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct TypeStmt
{
	name: Box<DottedName>,
	summary: Box<Option<String>>,
	args: Box<ArgList>,
	items: Box<Vec<Box<TypeItem>>>,
	desc: Box<Option<String>>,
}


impl TypeStmt
{
	pub fn new_boxed(name: Box<DottedName>, summary: Box<Option<String>>, args: Box<ArgList>, items: Box<Vec<Box<TypeItem>>>, desc: Box<Option<String>>) -> Box<TypeStmt>
	{
		Box::new(TypeStmt {
			name,
			summary,
			args,
			items,
			desc,
		})
	}


	pub fn name(&self) -> &DottedName
	{
		&self.name
	}

	pub fn summary(&self) -> &Option<String>
	{
		&self.summary
	}

	pub fn args(&self) -> &ArgList
	{
		&self.args
	}

	pub fn items(&self) -> &Vec<Box<TypeItem>>
	{
		&self.items
	}

	pub fn description(&self) -> &Box<Option<String>>
	{
		&self.desc
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub enum TypeItem
{
	DataDef(Box<DataDef>),
	RecordDef(Box<RecordDef>),
}


impl TypeItem
{
	pub fn new_name_boxed(name: Box<String>, args: Box<ArgList>) -> Box<TypeItem>
	{
		Box::new(TypeItem::DataDef(DataDef::new_boxed(name, args)))
	}

	pub fn new_record_def_boxed(name: Option<Box<String>>, fields: Box<Vec<Box<RecordField>>>) -> Box<TypeItem>
	{
		Box::new(TypeItem::RecordDef(RecordDef::new_boxed(name, fields)))
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct DataDef
{
	name: Box<String>,
	args: Box<ArgList>,
}


impl DataDef
{
	pub fn new_boxed(name: Box<String>, args: Box<ArgList>) -> Box<DataDef>
	{
		Box::new(DataDef {
			name,
			args,
		})
	}

	pub fn name(&self) -> &String
	{
		&self.name
	}

	pub fn args(&self) -> &ArgList
	{
		&self.args
	}
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RecordDef
{
	name: Option<Box<String>>,
	fields: Box<Vec<Box<RecordField>>>,
}


impl RecordDef
{
	pub fn new_boxed(name: Option<Box<String>>, fields: Box<Vec<Box<RecordField>>>) -> Box<RecordDef>
	{
		Box::new(RecordDef{
			name,
			fields,
		})
	}

	pub fn name(&self) -> &Option<Box<String>>
	{
		&self.name
	}

	pub fn fields(&self) -> &Vec<Box<RecordField>>
	{
		&self.fields
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct RecordField
{
	name: Box<Expr>,
	summary: Box<Option<String>>,
	typ: Box<Expr>,
	desc: Box<Option<String>>,
}


impl RecordField
{
	pub fn new_boxed(name: Box<Expr>, summary: Box<Option<String>>, typ: Box<Expr>, desc: Box<Option<String>>) -> Box<RecordField>
	{
		Box::new(RecordField {
			name,
			summary,
			typ,
			desc,
		})
	}

	pub fn name(&self) -> &Expr
	{
		&self.name
	}

	pub fn summary(&self) -> &Option<String>
	{
		&self.summary
	}

	pub fn typ(&self) -> &Expr
	{
		&self.typ
	}

	pub fn description(&self) -> &Option<String>
	{
		&self.desc
	}

}


#[derive(Debug, Serialize, Deserialize)]
pub struct Event
{
	items: Box<Vec<Box<EventItem>>>,
}


impl Event
{
	pub fn items(&self) -> &Vec<Box<EventItem>>
	{
		&self.items
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct EventItem
{
	name: Box<DottedName>,
	summary: Box<Option<String>>,
	args: Box<ArgList>,
	desc: Box<Option<String>>,
}


impl EventItem
{
	pub fn new_boxed(name: Box<DottedName>, summary: Box<Option<String>>, args: Box<ArgList>, desc: Box<Option<String>>) -> Box<EventItem>
	{
		Box::new(EventItem {
			name,
			summary,
			args,
			desc,
		})
	}

	pub fn name(&self) -> &DottedName
	{
		&self.name
	}

	pub fn summary(&self) -> &Option<String>
	{
		&self.summary
	}

	pub fn args(&self) -> &ArgList
	{
		&self.args
	}

	pub fn description(&self) -> &Box<Option<String>>
	{
		&self.desc
	}
}


#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct DottedName
{
	names: Vec<String>,
}


impl DottedName
{
	pub fn new_boxed(names: Vec<String>) -> Box<DottedName>
	{
		Box::new(DottedName {
			names: names,
		})
	}

	pub fn names(&self) -> &Vec<String>
	{
		&self.names
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct State
{
	name: String,
	summary: Option<Box<String>>,
	args: Box<ArgList>,
	fields: Vec<Box<Field>>,
	desc: Option<Box<String>>,
}


impl State
{
	pub fn new_boxed(name: String, summary: Option<Box<String>>, args: Box<ArgList>, fields: Vec<Box<Field>>, desc: Option<Box<String>>) -> Box<State>
	{
		Box::new(State {
			name,
			summary,
			args,
			fields,
			desc,
		})
	}

	pub fn name(&self) -> &String
	{
		&self.name
	}

	pub fn summary(&self) -> &Option<Box<String>>
	{
		&self.summary
	}

	pub fn args(&self) -> &ArgList
	{
		&self.args
	}

	pub fn fields(&self) -> &Vec<Box<Field>>
	{
		&self.fields
	}

	pub fn description(&self) -> &Option<Box<String>>
	{
		&self.desc
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub enum Field
{
	Var(VarField),
	Invariant(InvariantField),
	Transition(TransitionField),
}


impl Field
{
	pub fn new_var_boxed(name: Box<Expr>, typ: Option<Box<Expr>>, init: Option<Box<Expr>>, summary: Box<Option<String>>, desc: Box<Option<String>>) -> Box<Field>
	{
		Box::new(Field::Var(VarField::new(name, typ, init, summary, desc)))
	}

	pub fn get_var(&self) -> Option<&VarField>
	{
		if let Field::Var(x) = self { Some(x) }
		else { None }
	}

	pub fn get_invariant(&self) -> Option<&InvariantField>
	{
		if let Field::Invariant(x) = self { Some(x) }
		else { None }
	}

	pub fn get_transition(&self) -> Option<&TransitionField>
	{
		if let Field::Transition(x) = self { Some(x) }
		else { None }
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct VarField
{
	name: Box<Expr>,
	typ: Option<Box<Expr>>,
	init: Option<Box<Expr>>,
	summary: Box<Option<String>>,
	desc: Box<Option<String>>,
}


impl VarField
{
	pub fn new(name: Box<Expr>, typ: Option<Box<Expr>>, init: Option<Box<Expr>>, summary: Box<Option<String>>, desc: Box<Option<String>>) -> VarField
	{
		VarField {
			name,
			typ,
			init,
			summary,
			desc,
		}
	}

	pub fn name(&self) -> &Expr
	{
		&self.name
	}

	pub fn typ(&self) -> &Option<Box<Expr>>
	{
		&self.typ
	}

	pub fn init(&self) -> &Option<Box<Expr>>
	{
		&self.init
	}

	pub fn summary(&self) -> &Option<String>
	{
		&self.summary
	}

	pub fn description(&self) -> &Option<String>
	{
		&self.desc
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct InvariantField
{
	name: Option<String>,
	summary: Box<Option<String>>,
	exprs: Box<Vec<Box<Expr>>>,
	desc: Box<Option<String>>,
}


impl InvariantField
{
	pub fn new_boxed(name: Option<String>, summary: Box<Option<String>>, exprs: Box<Vec<Box<Expr>>>, desc: Box<Option<String>>) -> Box<InvariantField>
	{
		Box::new(InvariantField {
			name,
			summary,
			exprs,
			desc,
		})
	}

	pub fn name(&self) -> &Option<String>
	{
		&self.name
	}

	pub fn summary(&self) -> &Option<String>
	{
		&self.summary
	}

	pub fn exprs(&self) -> &Vec<Box<Expr>>
	{
		&*self.exprs
	}

	pub fn description(&self) -> &Option<String>
	{
		&self.desc
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct TransitionField
{
	name: Box<DottedName>,
	args: Box<ArgList>,
	guard: Option<(Box<Expr>, Box<Option<String>>)>,
	post_cond: Box<PostCond>,
	desc: Box<Option<String>>,
}


impl TransitionField
{
	pub fn new_boxed(name: Box<DottedName>, args: Box<ArgList>, guard: Option<(Box<Expr>, Box<Option<String>>)>, post_cond: Box<PostCond>, desc: Box<Option<String>>) -> Box<TransitionField>
	{
		Box::new(TransitionField {
			name: name,
			args: args,
			guard: guard,
			post_cond: post_cond,
			desc,
		})
	}

	pub fn name(&self) -> &DottedName
	{
		&self.name
	}

	pub fn args(&self) -> &ArgList
	{
		&self.args
	}

	pub fn guard(&self) -> &Option<(Box<Expr>, Box<Option<String>>)>
	{
		&self.guard
	}

	pub fn post_cond(&self) -> &PostCond
	{
		&self.post_cond
	}

	pub fn description(&self) -> &Option<String>
	{
		&self.desc
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct ArgList
{
	args: Box<Vec<Box<Arg>>>,
}


impl ArgList
{
	pub fn new_boxed(args: Box<Vec<Box<Arg>>>) -> Box<ArgList>
	{
		Box::new(ArgList {
			args: args,
		})
	}

	pub fn args(&self) -> &Vec<Box<Arg>>
	{
		&self.args
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct Arg
{
	name: Box<Expr>,
	typ: Option<Box<Expr>>,
}


impl Arg
{
	pub fn new_boxed(name: Box<Expr>, typ: Option<Box<Expr>>) -> Box<Arg>
	{
		Box::new(Arg {
			name: name,
			typ: typ,
		})
	}

	pub fn name(&self) -> &Expr
	{
		&self.name
	}

	pub fn typ(&self) -> &Option<Box<Expr>>
	{
		&self.typ
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct PostCond
{
	targets: Box<Vec<String>>,
	exprs: Box<Vec<Box<PostCondItem>>>,
	desc: Box<Option<String>>,
}


impl PostCond
{
	pub fn new_boxed(targets: Box<Vec<String>>, exprs: Box<Vec<Box<PostCondItem>>>, desc: Box<Option<String>>) -> Box<PostCond>
	{
		Box::new(PostCond {
			targets,
			exprs,
			desc,
		})
	}

	pub fn targets(&self) -> &Vec<String>
	{
		&self.targets
	}

	pub fn exprs(&self) -> &Vec<Box<PostCondItem>>
	{
		&self.exprs
	}

	pub fn description(&self) -> &Option<String>
	{
		&self.desc
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub enum PostCondItem
{
	Mutation(Box<Mutation>),
	Expr(Box<Expr>),
}


impl PostCondItem
{
	pub fn new_mutation_boxed(mutation: Box<Mutation>) -> Box<PostCondItem>
	{
		Box::new(PostCondItem::Mutation(mutation))
	}


	pub fn new_expr_boxed(expr: Box<Expr>) -> Box<PostCondItem>
	{
		Box::new(PostCondItem::Expr(expr))
	}
}


#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum VarType
{
	Var,
	Const,
}


#[derive(Debug, Serialize, Deserialize)]
pub struct VarStmt
{
	var_type: VarType,
	matcher: Box<Expr>,
	typ: Option<Box<Expr>>,
	init: Option<Box<Expr>>,
	summary: Box<Option<String>>,
	desc: Box<Option<String>>,
}


impl VarStmt
{
	pub fn new_boxed(var_type: VarType, matcher: Box<Expr>, typ: Option<Box<Expr>>, init: Option<Box<Expr>>, summary: Box<Option<String>>, desc: Box<Option<String>>) -> Box<VarStmt>
	{
		Box::new(VarStmt {
			var_type,
			matcher,
			typ,
			init,
			summary,
			desc,
		})
	}

	pub fn into_var_field_boxed(self: Box<Self>) -> Box<Field>
	{
		Field::new_var_boxed(
			self.matcher,
			self.typ,
			self.init,
			self.summary,
			self.desc,
		)
	}

	pub fn var_type(&self) -> VarType
	{
		self.var_type
	}

	pub fn matcher(&self) -> &Expr
	{
		&self.matcher
	}

	pub fn typ(&self) -> &Option<Box<Expr>>
	{
		&self.typ
	}

	pub fn init(&self) -> &Option<Box<Expr>>
	{
		&self.init
	}

	pub fn summary(&self) -> &Option<String>
	{
		&self.summary
	}

	pub fn description(&self) -> &Option<String>
	{
		&self.desc
	}
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Mutation
{
	lhs: Box<Expr>,
	ltype: Option<Box<Expr>>,
	op: Box<String>,
	rhs: Box<Expr>,
}


impl Mutation
{
	pub fn new_boxed(lhs: Box<Expr>, ltype: Option<Box<Expr>>, op: Box<String>, rhs: Box<Expr>) -> Box<Mutation>
	{
		Box::new(Mutation {
			lhs,
			ltype,
			op,
			rhs,
		})
	}

	pub fn lhs(&self) -> &Expr
	{
		&self.lhs
	}

	pub fn ltype(&self) -> &Option<Box<Expr>>
	{
		&self.ltype
	}

	pub fn op(&self) -> &String
	{
		&self.op
	}

	pub fn rhs(&self) -> &Expr
	{
		&self.rhs
	}
}


#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Prec
{
	// https://www.haskell.org/onlinereport/decls.html
	Paren,
	// WeakAppli, // $, $!, `seq`
	// Bind, // >>, >>=
	Or, // ||
	And,  // &&
	Cmp,  // ==, /=, <, <=, >, >=, `elem`, `notElem`
	Append,  // :, ++
	Add,  // +, -
	Mul,  // *, /, `div`, `mod`, `rem`, `quot`
	UserDefined,
	Dotted,
	Appli,
	Term,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Expr
{
	Or(Box<OrExpr>),
	And(Box<AndExpr>),
	Cmp(Box<CmpExpr>),
	Dotted(Box<DottedExpr>),
	Appli(Box<AppliExpr>),
	Name(Box<NameExpr>),
	Num(Box<NumExpr>),
	Str(Box<StrExpr>),
	Case(Box<CaseExpr>),
	Let(Box<LetExpr>),
	If(Box<IfExpr>),
	Nil,
	List(Box<ListExpr>),
	ListComprehension(Box<ListComprehensionExpr>),
	Set(Box<SetExpr>),
	Paren(Box<Expr>),
	EventSet(Box<Vec<Box<Expr>>>),
	ChannelSet(Box<Vec<Box<Expr>>>),
	TargetSet(Box<Vec<Box<Expr>>>),
	BinOp(Box<BinOpExpr>),
	RecordMutation(Box<RecordMutation>),
	Quantifier(Box<QuantifierExpr>),
	Fn(Box<FnExpr>),
	Any,
	State,
}


impl Expr
{
	pub fn new_or_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Or(OrExpr::new_boxed(lhs, rhs)))
	}

	pub fn new_and_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::And(AndExpr::new_boxed(lhs, rhs)))
	}

	pub fn new_cmp_boxed(lhs: Box<Expr>, op: String, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Cmp(CmpExpr::new_boxed(lhs, op, rhs)))
	}

	pub fn new_appli_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Appli(AppliExpr::new_boxed(lhs, rhs)))
	}

	pub fn new_name_boxed(name: String) -> Box<Expr>
	{
		Box::new(Expr::Name(NameExpr::new_boxed(name)))
	}

	pub fn new_num_boxed(num: usize) -> Box<Expr>
	{
		Box::new(Expr::Num(NumExpr::new_boxed(num)))
	}

	pub fn new_str_boxed(value: String, begin: String, end: String) -> Box<Expr>
	{
		Box::new(Expr::Str(StrExpr::new_boxed(value, begin, end)))
	}

	pub fn new_case_boxed(expr: Box<Expr>, elems: Box<Vec<Box<CaseElem>>>) -> Box<Expr>
	{
		Box::new(Expr::Case(CaseExpr::new_boxed(expr, elems)))
	}

	pub fn new_if_boxed(cond: Box<Expr>, then_expr: Box<Expr>, else_expr: Option<Box<Expr>>) -> Box<Expr>
	{
		Box::new(Expr::If(IfExpr::new_boxed(cond, then_expr, else_expr)))
	}

	pub fn new_nil_boxed() -> Box<Expr>
	{
		Box::new(Expr::Nil)
	}

	pub fn new_list_boxed(elems: Box<Vec<Box<Expr>>>) -> Box<Expr>
	{
		Box::new(Expr::List(ListExpr::new_boxed(elems)))
	}

	pub fn new_list_comprehension_boxed(out_expr: Box<Expr>, elems: Box<Vec<Box<Expr>>>) -> Box<Expr>
	{
		Box::new(Expr::ListComprehension(ListComprehensionExpr::new_boxed(out_expr, elems)))
	}

	pub fn new_set_boxed(elems: Box<Vec<Box<Expr>>>) -> Box<Expr>
	{
		Box::new(Expr::Set(SetExpr::new_boxed(elems)))
	}

	pub fn new_paren_boxed(expr: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Paren(expr))
	}

	pub fn new_event_set_boxed(exprs: Box<Vec<Box<Expr>>>) -> Box<Expr>
	{
		Box::new(Expr::EventSet(exprs))
	}

	pub fn new_channel_set_boxed(exprs: Box<Vec<Box<Expr>>>) -> Box<Expr>
	{
		Box::new(Expr::ChannelSet(exprs))
	}

	pub fn new_target_set_boxed(expr: Box<Vec<Box<Expr>>>) -> Box<Expr>
	{
		Box::new(Expr::TargetSet(expr))
	}

	pub fn new_record_creation_boxed(expr: Box<Expr>, mutations: Box<Vec<Box<Mutation>>>) -> Box<Expr>
	{
		Box::new(Expr::RecordMutation(RecordMutation::new_boxed(RecordStyle::Outside, expr, mutations)))
	}

	pub fn new_record_mutation_boxed(style: RecordStyle, expr: Box<Expr>, mutations: Box<Vec<Box<Mutation>>>) -> Box<Expr>
	{
		Box::new(Expr::RecordMutation(RecordMutation::new_boxed(style, expr, mutations)))
	}

	pub fn new_append_boxed(lhs: Box<Expr>, op: String, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::BinOp(BinOpExpr::new_boxed(Prec::Append, lhs, op, rhs)))
	}

	pub fn new_add_boxed(lhs: Box<Expr>, op: String, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::BinOp(BinOpExpr::new_boxed(Prec::Add, lhs, op, rhs)))
	}

	pub fn new_mul_boxed(lhs: Box<Expr>, op: String, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::BinOp(BinOpExpr::new_boxed(Prec::Mul, lhs, op, rhs)))
	}

	pub fn new_binop_boxed(lhs: Box<Expr>, op: String, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::BinOp(BinOpExpr::new_boxed(Prec::UserDefined, lhs, op, rhs)))
	}

	pub fn new_dotted_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Dotted(DottedExpr::new_boxed(lhs, rhs)))
	}

	pub fn new_let_boxed(mutations: Box<Vec<Box<Mutation>>>, expr: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Let(LetExpr::new_boxed(mutations, expr)))
	}

	pub fn new_quantifier_boxed(kind: QuantifierKind, exprs: Box<Vec<Box<Expr>>>, cond: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Quantifier(QuantifierExpr::new_boxed(kind, exprs, cond)))
	}

	pub fn new_fn_boxed(args: Box<ArgList>, typ: Option<Box<Expr>>, body: Option<Box<Expr>>) -> Box<Expr>
	{
		Box::new(Expr::Fn(FnExpr::new_boxed(args, typ, body)))
	}


	pub fn new_any_boxed() -> Box<Expr>
	{
		Box::new(Expr::Any)
	}


	pub fn new_state_boxed() -> Box<Expr>
	{
		Box::new(Expr::State)
	}



	pub fn get_dotted(&self) -> Option<&DottedExpr>
	{
		if let Expr::Dotted(x) = self { Some(x) }
		else { None }
	}


	pub fn get_name(&self) -> Option<&NameExpr>
	{
		if let Expr::Name(x) = self { Some(x) }
		else { None }
	}


	#[allow(dead_code)] // XXX
	pub fn prec(&self) -> Prec
	{
		match self
		{
		Expr::Or(_) => Prec::Or,
		Expr::And(_) => Prec::And,
		Expr::Cmp(_) => Prec::Cmp,
		Expr::Dotted(_) => Prec::Dotted,
		Expr::Appli(_) => Prec::Appli,
		Expr::Name(_) => Prec::Term,
		Expr::Num(_) => Prec::Term,
		Expr::Str(_) => Prec::Term,
		Expr::Case(_) => Prec::Term,
		Expr::If(_) => Prec::Term,
		Expr::Nil => Prec::Term,
		Expr::List(_) => Prec::Term,
		Expr::ListComprehension(_) => Prec::Term,
		Expr::Set(_) => Prec::Term,
		Expr::Paren(_) => Prec::Paren,
		Expr::EventSet(_) => Prec::Paren,
		Expr::ChannelSet(_) => Prec::Paren,
		Expr::TargetSet(_) => Prec::Paren,
		Expr::BinOp(x) => x.prec(),
		Expr::Let(_) => Prec::Term,
		Expr::RecordMutation(_) => Prec::Term,
		Expr::Any => Prec::Term,
		Expr::Quantifier(_) => Prec::Term,
		Expr::Fn(_) => Prec::Term,
		Expr::State => Prec::Term,
		}
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct OrExpr
{
	lhs: Box<Expr>,
	rhs: Box<Expr>,
}


impl OrExpr
{
	pub fn new_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<OrExpr>
	{
		Box::new(OrExpr {
			lhs: lhs,
			rhs: rhs,
		})
	}

	pub fn lhs(&self) -> &Expr
	{
		&self.lhs
	}

	pub fn rhs(&self) -> &Expr
	{
		&self.rhs
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct AndExpr
{
	lhs: Box<Expr>,
	rhs: Box<Expr>,
}


impl AndExpr
{
	pub fn new_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<AndExpr>
	{
		Box::new(AndExpr {
			lhs: lhs,
			rhs: rhs,
		})
	}

	pub fn lhs(&self) -> &Expr
	{
		&self.lhs
	}

	pub fn rhs(&self) -> &Expr
	{
		&self.rhs
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct CmpExpr
{
	lhs: Box<Expr>,
	op: String,
	rhs: Box<Expr>,
}


impl CmpExpr
{
	pub fn new_boxed(lhs: Box<Expr>, op: String, rhs: Box<Expr>) -> Box<CmpExpr>
	{
		Box::new(CmpExpr {
			lhs: lhs,
			op: op,
			rhs: rhs,
		})
	}

	pub fn lhs(&self) -> &Expr
	{
		&self.lhs
	}

	pub fn op(&self) -> &String
	{
		&self.op
	}

	pub fn rhs(&self) -> &Expr
	{
		&self.rhs
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct AppliExpr
{
	lhs: Box<Expr>,
	rhs: Box<Expr>,
}


impl AppliExpr
{
	pub fn new_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<AppliExpr>
	{
		Box::new(AppliExpr {
			lhs: lhs,
			rhs: rhs,
		})
	}

	pub fn lhs(&self) -> &Expr
	{
		&self.lhs
	}

	pub fn rhs(&self) -> &Expr
	{
		&self.rhs
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct NameExpr
{
	name: String,
}


impl NameExpr
{
	pub fn new_boxed(name: String) -> Box<NameExpr>
	{
		Box::new(NameExpr {
			name: name,
		})
	}

	pub fn name(&self) -> &String
	{
		&self.name
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct NumExpr
{
	num: usize,
}


impl NumExpr
{
	pub fn new_boxed(num: usize) -> Box<NumExpr>
	{
		Box::new(NumExpr {
			num: num,
		})
	}

	pub fn num(&self) -> usize
	{
		self.num
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct StrExpr
{
	value: String,
	begin: String,
	end: String,
}


impl StrExpr
{
	pub fn new_boxed(value: String, begin: String, end: String) -> Box<StrExpr>
	{
		Box::new(StrExpr {
			value,
			begin,
			end,
		})
	}

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


#[derive(Debug, Serialize, Deserialize)]
pub struct BinOpExpr
{
	prec: Prec,
	lhs: Box<Expr>,
	op: String,
	rhs: Box<Expr>,
}


impl BinOpExpr
{
	pub fn new_boxed(prec: Prec, lhs: Box<Expr>, op: String, rhs: Box<Expr>) -> Box<BinOpExpr>
	{
		Box::new(BinOpExpr {
			prec: prec,
			lhs: lhs,
			op: op,
			rhs: rhs,
		})
	}

	fn prec(&self) -> Prec
	{
		self.prec
	}

	pub fn destruct(self: Box<Self>) -> (Box<Expr>, String, Box<Expr>)
	{
		(self.lhs, self.op, self.rhs)
	}

	pub fn lhs(&self) -> &Expr
	{
		&self.lhs
	}

	pub fn op(&self) -> &String
	{
		&self.op
	}

	pub fn rhs(&self) -> &Expr
	{
		&self.rhs
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct DottedExpr
{
	lhs: Box<Expr>,
	rhs: Box<Expr>,
}


impl DottedExpr
{
	pub fn new_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<DottedExpr>
	{
		Box::new(DottedExpr {
			lhs: lhs,
			rhs: rhs,
		})
	}

	pub fn lhs(&self) -> &Expr
	{
		&self.lhs
	}

	pub fn rhs(&self) -> &Expr
	{
		&self.rhs
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct CaseExpr
{
	expr: Box<Expr>,
	elems: Box<Vec<Box<CaseElem>>>,
}


impl CaseExpr
{
	pub fn new_boxed(expr: Box<Expr>, elems: Box<Vec<Box<CaseElem>>>) -> Box<CaseExpr>
	{
		Box::new(CaseExpr {
			expr: expr,
			elems: elems,
		})
	}

	pub fn expr(&self) -> &Expr
	{
		&self.expr
	}

	pub fn elems(&self) -> &Vec<Box<CaseElem>>
	{
		&self.elems
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct CaseElem
{
	cond: Box<Expr>,
	guard: Option<Box<Expr>>,
	body: Box<Expr>,
}


impl CaseElem
{
	pub fn new_boxed(cond: Box<Expr>, guard: Option<Box<Expr>>, body: Box<Expr>) -> Box<CaseElem>
	{
		Box::new(CaseElem {
			cond: cond,
			guard: guard,
			body: body,
		})
	}

	pub fn cond(&self) -> &Expr
	{
		&self.cond
	}

	pub fn guard(&self) -> Option<&Box<Expr>>
	{
		self.guard.as_ref()
	}

	pub fn body(&self) -> &Expr
	{
		&self.body
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct IfExpr
{
	cond: Box<Expr>,
	then_expr: Box<Expr>,
	else_expr: Option<Box<Expr>>,
}


impl IfExpr
{
	pub fn new_boxed(cond: Box<Expr>, then_expr: Box<Expr>, else_expr: Option<Box<Expr>>) -> Box<IfExpr>
	{
		Box::new(IfExpr {
			cond,
			then_expr,
			else_expr,
		})
	}

	pub fn cond(&self) -> &Expr
	{
		&self.cond
	}

	pub fn then_expr(&self) -> &Expr
	{
		&self.then_expr
	}

	pub fn else_expr(&self) -> &Option<Box<Expr>>
	{
		&self.else_expr
	}
}




#[derive(Debug, Serialize, Deserialize)]
pub struct ListExpr
{
	elems: Box<Vec<Box<Expr>>>,
}


impl ListExpr
{
	pub fn new_boxed(elems: Box<Vec<Box<Expr>>>) -> Box<ListExpr>
	{
		Box::new(ListExpr {
			elems: elems,
		})
	}

	pub fn elems(&self) -> &Vec<Box<Expr>>
	{
		&self.elems
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct ListComprehensionExpr
{
	expr: Box<Expr>,
	elems: Box<Vec<Box<Expr>>>,
}


impl ListComprehensionExpr
{
	pub fn new_boxed(expr: Box<Expr>, elems: Box<Vec<Box<Expr>>>) -> Box<ListComprehensionExpr>
	{
		Box::new(ListComprehensionExpr {
			expr,
			elems,
		})
	}

	pub fn expr(&self) -> &Expr
	{
		&self.expr
	}

	pub fn elems(&self) -> &Vec<Box<Expr>>
	{
		&self.elems
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct SetExpr
{
	elems: Box<Vec<Box<Expr>>>,
}


impl SetExpr
{
	pub fn new_boxed(elems: Box<Vec<Box<Expr>>>) -> Box<SetExpr>
	{
		Box::new(SetExpr {
			elems: elems,
		})
	}

	pub fn elems(&self) -> &Vec<Box<Expr>>
	{
		&self.elems
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct RecordMutation
{
	style: RecordStyle,
	expr: Box<Expr>,
	mutations: Box<Vec<Box<Mutation>>>,
}


#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone, Copy)]
pub enum RecordStyle
{
	Inside,
	Outside,
}


impl RecordMutation
{
	pub fn new_boxed(style: RecordStyle, expr: Box<Expr>, mutations: Box<Vec<Box<Mutation>>>) -> Box<RecordMutation>
	{
		Box::new(RecordMutation {
			style,
			expr,
			mutations,
		})
	}

	pub fn style(&self) -> RecordStyle
	{
		self.style
	}

	pub fn expr(&self) -> &Expr
	{
		&self.expr
	}

	pub fn mutations(&self) -> &Vec<Box<Mutation>>
	{
		&self.mutations
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct LetExpr
{
	mutations: Box<Vec<Box<Mutation>>>,
	expr: Box<Expr>,
}


impl LetExpr
{
	pub fn new_boxed(mutations: Box<Vec<Box<Mutation>>>, expr: Box<Expr>) -> Box<LetExpr>
	{
		Box::new(LetExpr {
			mutations,
			expr,
		})
	}

	pub fn mutations(&self) -> &Vec<Box<Mutation>>
	{
		&self.mutations
	}

	pub fn expr(&self) -> &Expr
	{
		&self.expr
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct QuantifierExpr
{
	kind: QuantifierKind,
	exprs: Box<Vec<Box<Expr>>>,
	cond: Box<Expr>,
}


impl QuantifierExpr
{
	pub fn new_boxed(kind: QuantifierKind, exprs: Box<Vec<Box<Expr>>>, cond: Box<Expr>) -> Box<QuantifierExpr>
	{
		Box::new(QuantifierExpr {
			kind,
			exprs,
			cond,
		})
	}

	pub fn kind(&self) -> QuantifierKind
	{
		self.kind
	}

	pub fn exprs(&self) -> &Vec<Box<Expr>>
	{
		&self.exprs
	}

	pub fn cond(&self) -> &Expr
	{
		&self.cond
	}
}


#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
pub enum QuantifierKind
{
	Exists,
	Exists1,
	ForAll,
}


#[derive(Debug, Serialize, Deserialize)]
pub struct FnExpr
{
	args: Box<ArgList>,
	typ: Option<Box<Expr>>,
	body: Option<Box<Expr>>,
}


impl FnExpr
{
	pub fn new_boxed(args: Box<ArgList>, typ: Option<Box<Expr>>, body: Option<Box<Expr>>) -> Box<FnExpr>
	{
		Box::new(FnExpr {
			args,
			typ,
			body,
		})
	}

	pub fn args(&self) -> &ArgList
	{
		&self.args
	}

	pub fn typ(&self) -> &Option<Box<Expr>>
	{
		&self.typ
	}

	pub fn body(&self) -> &Option<Box<Expr>>
	{
		&self.body
	}
}
