use serde::{Serialize, Deserialize};
use std::boxed::Box;


#[derive(Debug, Serialize, Deserialize)]
pub struct Module
{
	events: Box<Event>,
	vars: Box<Vec<Box<VarStmt>>>,
	states: Box<Vec<Box<State>>>,
}

impl Module
{
	pub fn new_boxed(events: Box<Event>, vars: Box<Vec<Box<VarStmt>>>, states: Box<Vec<Box<State>>>) -> Box<Module>
	{
		Box::new(Module {
			events: events,
			vars: vars,
			states: states,
		})
	}

	pub fn events(&self) -> &Event
	{
		&self.events
	}

	pub fn vars(&self) -> &Vec<Box<VarStmt>>
	{
		&self.vars
	}

	pub fn states(&self) -> &Vec<Box<State>>
	{
		&self.states
	}

	pub fn get_event_summary(&self, name: &DottedName) -> Option<&String>
	{
		self.events.items
			.iter()
			.find(|item| item.name() == name)
			.map(|item| &item.summary)
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct Event
{
	items: Box<Vec<Box<EventItem>>>,
}


impl Event
{
	pub fn new(items: Vec<Box<EventItem>>) -> Box<Event>
	{
		Box::new(Event {
			items: Box::new(items),
		})
	}

	pub fn items(&self) -> &Vec<Box<EventItem>>
	{
		&self.items
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct EventItem
{
	name: Box<DottedName>,
	summary: String,
	args: Box<ArgList>,
}


impl EventItem
{
	pub fn new_boxed(name: Box<DottedName>, summary: String, args: Box<ArgList>) -> Box<EventItem>
	{
		Box::new(EventItem {
			name: name,
			summary: summary,
			args: args,
		})
	}

	pub fn name(&self) -> &DottedName
	{
		&self.name
	}

	pub fn summary(&self) -> &String
	{
		&self.summary
	}

	pub fn args(&self) -> &ArgList
	{
		&self.args
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
	summary: String,
	args: Box<ArgList>,
	fields: Vec<Box<Field>>,
}


impl State
{
	pub fn new_boxed(name: String, summary: String, args: Box<ArgList>, fields: Vec<Box<Field>>) -> Box<State>
	{
		Box::new(State {
			name: name,
			summary: summary,
			args: args,
			fields: fields,
		})
	}

	pub fn name(&self) -> &String
	{
		&self.name
	}

	pub fn summary(&self) -> &String
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
	pub fn new_var_boxed(name: String, typ: Box<Expr>, init: Box<Expr>) -> Box<Field>
	{
		Box::new(Field::Var(VarField::new(name, typ, init)))
	}

	pub fn new_invariant_boxed(exprs: Vec<Box<GuardedArm>>, desc: String) -> Box<Field>
	{
		Box::new(Field::Invariant(InvariantField::new(exprs, desc)))
	}

	pub fn new_transition_boxed(name: Box<DottedName>, args: Box<ArgList>, guard: Option<(Box<Expr>, String)>, post_cond: Box<PostCond>) -> Box<Field>
	{
		Box::new(Field::Transition(TransitionField::new(name, args, guard, post_cond)))
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
	name: String,
	typ: Box<Expr>,
	init: Box<Expr>,
}


impl VarField
{
	pub fn new(name: String, typ: Box<Expr>, init: Box<Expr>) -> VarField
	{
		VarField {
			name: name,
			typ: typ,
			init: init,
		}
	}

	pub fn name(&self) -> &String
	{
		&self.name
	}

	pub fn typ(&self) -> &Expr
	{
		&self.typ
	}

	pub fn init(&self) -> &Expr
	{
		&self.init
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct InvariantField
{
	exprs: Vec<Box<GuardedArm>>,
	desc: String,
}


impl InvariantField
{
	pub fn new(exprs: Vec<Box<GuardedArm>>, desc: String) -> InvariantField
	{
		InvariantField {
			exprs: exprs,
			desc: desc,
		}
	}

	pub fn exprs(&self) -> &Vec<Box<GuardedArm>>
	{
		&self.exprs
	}

	pub fn desc(&self) -> &String
	{
		&self.desc
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct GuardedArm
{
	guard: Option<Box<Expr>>,
	body: Box<Expr>,
}


impl GuardedArm
{
	pub fn new_boxed(guard: Option<Box<Expr>>, body: Box<Expr>) -> Box<GuardedArm>
	{
		Box::new(GuardedArm {
			guard: guard,
			body: body,
		})
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
pub struct TransitionField
{
	name: Box<DottedName>,
	args: Box<ArgList>,
	guard: Option<(Box<Expr>, String)>,
	post_cond: Box<PostCond>,
}


impl TransitionField
{
	pub fn new(name: Box<DottedName>, args: Box<ArgList>, guard: Option<(Box<Expr>, String)>, post_cond: Box<PostCond>) -> TransitionField
	{
		TransitionField {
			name: name,
			args: args,
			guard: guard,
			post_cond: post_cond,
		}
	}

	pub fn name(&self) -> &DottedName
	{
		&self.name
	}

	pub fn args(&self) -> &ArgList
	{
		&self.args
	}

	pub fn guard(&self) -> Option<&(Box<Expr>, String)>
	{
		self.guard.as_ref()
	}

	pub fn post_cond(&self) -> &PostCond
	{
		&self.post_cond
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
	name: String,
	typ: Box<Expr>,
}


impl Arg
{
	pub fn new_boxed(name: String, typ: Box<Expr>) -> Box<Arg>
	{
		Box::new(Arg {
			name: name,
			typ: typ,
		})
	}

	pub fn name(&self) -> &String
	{
		&self.name
	}

	pub fn typ(&self) -> &Expr
	{
		&self.typ
	}
}


#[derive(Debug, Serialize, Deserialize)]
pub struct PostCond
{
	targets: Box<Vec<String>>,
	exprs: Box<Vec<Box<Mutation>>>,
	desc: String,
}


impl PostCond
{
	pub fn new_boxed(targets: Box<Vec<String>>, exprs: Box<Vec<Box<Mutation>>>, desc: String) -> Box<PostCond>
	{
		Box::new(PostCond {
			targets: targets,
			exprs: exprs,
			desc: desc,
		})
	}

	pub fn targets(&self) -> &Vec<String>
	{
		&self.targets
	}

	pub fn exprs(&self) -> &Vec<Box<Mutation>>
	{
		&self.exprs
	}

	pub fn desc(&self) -> &String
	{
		&self.desc
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
	typ: Box<Expr>,
	init: Box<Expr>,
	summary: String,
	desc: String,
}


impl VarStmt
{
	pub fn new_boxed(var_type: VarType, matcher: Box<Expr>, typ: Box<Expr>, init: Box<Expr>, summary: String, desc: String) -> Box<VarStmt>
	{
		Box::new(VarStmt {
			var_type: var_type,
			matcher: matcher,
			typ: typ,
			init: init,
			summary: summary,
			desc: desc,
		})
	}

	pub fn var_type(&self) -> VarType
	{
		self.var_type
	}

	pub fn matcher(&self) -> &Expr
	{
		&self.matcher
	}

	pub fn typ(&self) -> &Expr
	{
		&self.typ
	}

	pub fn init(&self) -> &Expr
	{
		&self.init
	}

	pub fn summary(&self) -> &String
	{
		&self.summary
	}

	pub fn desc(&self) -> &String
	{
		&self.desc
	}
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Mutation
{
	lhs: Box<Expr>,
	rhs: Box<Expr>,
}


impl Mutation
{
	pub fn new_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Mutation>
	{
		Box::new(Mutation {
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
	Nil,
	Paren(Box<Expr>),
	BinOp(Box<BinOpExpr>),
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

	pub fn new_str_boxed(s: String) -> Box<Expr>
	{
		Box::new(Expr::Str(StrExpr::new_boxed(s)))
	}

	pub fn new_case_boxed(expr: Box<Expr>, elems: Box<Vec<Box<CaseElem>>>) -> Box<Expr>
	{
		Box::new(Expr::Case(CaseExpr::new_boxed(expr, elems)))
	}

	pub fn new_nil_boxed() -> Box<Expr>
	{
		Box::new(Expr::Nil)
	}

	pub fn new_paren_boxed(expr: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Paren(expr))
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

	pub fn new_dotted_boxed(lhs: Box<Expr>, rhs: Box<Expr>) -> Box<Expr>
	{
		Box::new(Expr::Dotted(DottedExpr::new_boxed(lhs, rhs)))
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
		Expr::Nil => Prec::Term,
		Expr::Paren(_) => Prec::Paren,
		Expr::BinOp(x) => x.prec(),
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
	s: String,
}


impl StrExpr
{
	pub fn new_boxed(s: String) -> Box<StrExpr>
	{
		Box::new(StrExpr {
			s: s,
		})
	}

	pub fn s(&self) -> &String
	{
		&self.s
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
