//! Rusk: a Specification Language
pub mod ast;
pub mod formatter
{
	//! # Formatter for Output
	pub mod json;
	pub mod markdown;
	pub mod rest;
	pub mod doc;
}
pub mod message;
pub mod parser;
pub mod scanner;

use crate::formatter::json::ToJsonText; // to_json_text(), to_json_text_pretty().
use crate::formatter::doc::ToDocWithTitle; // to_doc().
use crate::formatter::markdown::ToMarkdownText; // to_markdown_text().
use crate::formatter::rest::ToRestText; // to_rest_text().
use std::env;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;


struct Opts
{
	path: String,
	debug: bool,
	title: Option<String>,
	formatter: Formatter,
	text: Rc<message::Message>,
}


#[derive(PartialEq, Eq)]
enum Formatter
{
	Markdown,
	Rest,
	Json,
	JsonPretty,
	DocJson,
	DocJsonPretty,
	//Html,
	//Dot,
	//Rest,
	//Kml,
	//Yakml,
	//Excel,
}


fn run(opts: &Opts, input: String) -> std::io::Result<()>
{
	if opts.debug
	{
		println!("--[source]----\n{}\n--[result]----", input);
	}

	let tokens = match scanner::scan(input) {
		Ok(tokens) => tokens,
		Err((source_map, err)) => {
			let pos = source_map.error_position(err.as_ref());
			if opts.debug
			{
				println!("ERROR: scanner: {:?} {}", err, pos);
			}
			println!("{}:{}:{}:error:{}", opts.path, pos.line_num(), pos.col_num(), format!("{:?}", err));
			std::process::exit(1);
		},
	};
	if opts.debug
	{
		println!("--[tokens]----");
		println!("{:?}", tokens);
		for x in tokens.tokens().iter().enumerate()
		{
			println!("{:?}", x);
		}
		println!("--[parser]----");
	}

	let result = parser::parse_tokens(tokens);
	if opts.debug
	{
		println!("{:?}", result);
	}

	match result {
		Ok(module) => {
			if opts.debug
			{
				println!("OK: {:?}", module);
				if opts.formatter != Formatter::Json
				{
					println!("--[json]----\n{}", module.to_json_text()?);
				}
			}
			match opts.formatter
			{
				Formatter::Markdown => {
					if opts.debug
					{
						println!("--[markdown]----");
					}
					println!("{}", module.as_ref().to_doc(opts.title.as_ref().unwrap(), &opts.text).to_markdown_text());
				},
				Formatter::Rest => {
					if opts.debug
					{
						println!("--[rest]----");
					}
					println!("{}", module.as_ref().to_doc(opts.title.as_ref().unwrap(), &opts.text).to_rest_text());
				},
				Formatter::Json => {
					if opts.debug
					{
						println!("--[json]----");
					}
					println!("{}", module.to_json_text()?);
				},
				Formatter::JsonPretty => {
					if opts.debug
					{
						println!("--[json-pretty]----");
					}
					println!("{}", module.to_json_text_pretty()?);
				},
				Formatter::DocJson => {
					if opts.debug
					{
						println!("--[doc-json]----");
					}
					println!("{}", module.as_ref().to_doc(opts.title.as_ref().unwrap(), &opts.text).to_json_text()?);
				},
				Formatter::DocJsonPretty => {
					if opts.debug
					{
						println!("--[doc-json-pretty]----");
					}
					println!("{}", module.as_ref().to_doc(opts.title.as_ref().unwrap(), &opts.text).to_json_text_pretty()?);
				},
			}
		}
		Err((source_map, err)) => {
			if opts.debug
			{
				println!("ERROR: {:?}", err);
			}
			err.report(&opts.path, source_map);
			std::process::exit(1);
		}
	}

	Ok(())
}

fn read_file(path: String) -> std::io::Result<String>
{
	let mut file = File::open(path)?;
	let mut buff = String::new();
	file.read_to_string(&mut buff)?;
	Ok(buff)
}

fn usage() -> std::io::Result<()>
{
	println!("Usage:  rusk [Options] {{file | -e code}}");
	println!("");
	println!("Options:");
	println!("  -V, --version");
	println!("  -h, --help");
	println!("  --markdown       generate markdown document. (default)");
	println!("  --json           generate json text.");
	println!("  --json-pretty    generate pretty json text.");
	println!("  --title {{title}} set document title.");
	Ok(())
}

fn main() -> std::io::Result<()>
{
	let mut args = env::args().peekable();
	args.next();

	let mut opts = Opts {
		path: "-".into(),
		debug: false,
		title: None,
		formatter: Formatter::Markdown,
		text: message::en(),
	};

	let mut arg = args.peek();
	if arg == Some(&"-V".into()) || arg == Some(&"--version".into())
	{
		println!("rusk version 0.1.16");
		return Ok(());
	}
	if arg == Some(&"-h".into()) || arg == Some(&"--help".into()) || arg == None
	{
		return usage();
	}

	if arg == Some(&"--debug".into())
	{
		opts.debug = true;
		args.next();
		arg = args.peek();
	}

	if arg == Some(&"--ja".into())
	{
		opts.text = message::ja();
		args.next();
		arg = args.peek();
	}

	match arg.map(|s| s.as_str())
	{
		Some("--markdown") => {
			opts.formatter = Formatter::Markdown;
			args.next();
			arg = args.peek();
		},
		Some("--rest") => {
			opts.formatter = Formatter::Rest;
			args.next();
			arg = args.peek();
		},
		Some("--json") => {
			opts.formatter = Formatter::Json;
			args.next();
			arg = args.peek();
		},
		Some("--json-pretty") => {
			opts.formatter = Formatter::JsonPretty;
			args.next();
			arg = args.peek();
		},
		Some("--doc-json") => {
			opts.formatter = Formatter::DocJson;
			args.next();
			arg = args.peek();
		},
		Some("--doc-json-pretty") => {
			opts.formatter = Formatter::DocJsonPretty;
			args.next();
			arg = args.peek();
		},
		_ => (),
	}

	if arg == Some(&"--title".into())
	{
		args.next();
		opts.title = Some(args.next().expect("title"));
	}
	if opts.title == None
	{
		opts.title = Some(opts.text.h_formal_specification().into());
	}

	match args.next()
	{
		None => usage(),
		Some(arg1) => {
			opts.path = arg1.clone();
			if arg1 == "-e"
			{
				run(&opts, args.next().unwrap())
			}else
			{
				run(&opts, read_file(arg1)?)
			}
		}
	}
}
