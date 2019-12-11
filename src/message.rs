use serde::{Serialize, Deserialize};
use std::rc::Rc;


#[derive(Debug, Serialize, Deserialize)]
pub struct Message
{
	#[serde(default = "h_formal_specification")]
	h_formal_specification: Box<String>,

	#[serde(default = "h_types")]
	h_types: Box<String>,

	#[serde(default = "h_events")]
	h_events: Box<String>,

	#[serde(default = "h_module_variables")]
	h_module_variables: Box<String>,

	#[serde(default = "h_module_functions")]
	h_module_functions: Box<String>,

	#[serde(default = "h_module_invariants")]
	h_module_invariants: Box<String>,

	#[serde(default = "th_name")]
	th_name: Box<String>,

	#[serde(default = "th_alias")]
	th_alias: Box<String>,

	#[serde(default = "th_type")]
	th_type: Box<String>,

	#[serde(default = "th_init")]
	th_init: Box<String>,

	#[serde(default = "th_summary")]
	th_summary: Box<String>,

	#[serde(default = "th_definision")]
	th_definision: Box<String>,

	#[serde(default = "th_description")]
	th_description: Box<String>,

	#[serde(default = "t_const")]
	t_const: Box<String>,

	#[serde(default = "t_var")]
	t_var: Box<String>,
}

impl Message
{
	pub fn h_formal_specification(&self) -> &String
	{
		&self.h_formal_specification
	}

	pub fn h_types(&self) -> Box<String>
	{
		self.h_types.clone()
	}

	pub fn h_events(&self) -> Box<String>
	{
		self.h_events.clone()
	}

	pub fn h_module_variables(&self) -> Box<String>
	{
		self.h_module_variables.clone()
	}

	pub fn h_module_functions(&self) -> Box<String>
	{
		self.h_module_functions.clone()
	}

	pub fn h_module_invariants(&self) -> Box<String>
	{
		self.h_module_invariants.clone()
	}

	pub fn th_name(&self) -> Box<String>
	{
		self.th_name.clone()
	}

	pub fn th_alias(&self) -> Box<String>
	{
		self.th_alias.clone()
	}

	pub fn th_type(&self) -> Box<String>
	{
		self.th_type.clone()
	}

	pub fn th_init(&self) -> Box<String>
	{
		self.th_init.clone()
	}

	pub fn th_summary(&self) -> Box<String>
	{
		self.th_summary.clone()
	}

	pub fn th_definision(&self) -> Box<String>
	{
		self.th_definision.clone()
	}

	pub fn th_description(&self) -> Box<String>
	{
		self.th_description.clone()
	}

	pub fn t_const(&self) -> Box<String>
	{
		self.t_const.clone()
	}

	pub fn t_var(&self) -> Box<String>
	{
		self.t_var.clone()
	}
}

fn h_formal_specification() -> Box<String>
{
	Box::new("Formal Specification".into())
}

fn h_types() -> Box<String>
{
	Box::new("Types".into())
}

fn h_events() -> Box<String>
{
	Box::new("Events".into())
}

fn h_module_variables() -> Box<String>
{
	Box::new("Module Variables".into())
}

fn h_module_functions() -> Box<String>
{
	Box::new("Module Functions".into())
}

fn h_module_invariants() -> Box<String>
{
	Box::new("Module Invariants".into())
}

fn th_name() -> Box<String>
{
	Box::new("Name".into())
}

fn th_alias() -> Box<String>
{
	Box::new("Alias".into())
}

fn th_type() -> Box<String>
{
	Box::new("Type".into())
}

fn th_init() -> Box<String>
{
	Box::new("Init".into())
}

fn th_summary() -> Box<String>
{
	Box::new("Summary".into())
}

fn th_definision() -> Box<String>
{
	Box::new("Definision".into())
}

fn th_description() -> Box<String>
{
	Box::new("Description".into())
}

fn t_const() -> Box<String>
{
	Box::new("const".into())
}

fn t_var() -> Box<String>
{
	Box::new("var".into())
}


pub fn en() -> Rc<Message>
{
	let json = "{}";

	serde_json::from_str(json).unwrap()
}


pub fn ja() -> Rc<Message>
{
	let json = r#"
{
	"h_formal_specification": "形式仕様",
	"h_types": "型",
	"h_module_variables": "モジュール変数",
	"h_module_functions": "モジュール関数",
	"h_module_invariants": "モジュール不変条件",

	"th_name": "名称",
	"th_alias": "別名",
	"th_type": "型",
	"th_init": "初期値",
	"th_summary": "要約",
	"th_definision": "定義",
	"th_description": "説明",

	"t_const": "定数",
	"t_var": "変数"
}
	"#;

	serde_json::from_str(json).unwrap()
}
