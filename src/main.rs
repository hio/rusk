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
pub mod parser;
pub mod scanner;

use crate::formatter::json::ToJsonText; // to_json_text(), to_json_text_pretty().
use crate::formatter::doc::ToDocWithTitle; // to_doc().
use crate::formatter::markdown::ToMarkdownText; // to_markdown_text().
use crate::formatter::rest::ToRestText; // to_rest_text().
use std::env;
use std::fs::File;
use std::io::Read;


struct Opts
{
	path: String,
	debug: bool,
	title: String,
	formatter: Formatter,
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
					println!("{}", module.as_ref().to_doc(&opts.title).to_markdown_text());
				},
				Formatter::Rest => {
					if opts.debug
					{
						println!("--[rest]----");
					}
					println!("{}", module.as_ref().to_doc(&opts.title).to_rest_text());
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
					println!("{}", module.as_ref().to_doc(&opts.title).to_json_text()?);
				},
				Formatter::DocJsonPretty => {
					if opts.debug
					{
						println!("--[doc-json-pretty]----");
					}
					println!("{}", module.as_ref().to_doc(&opts.title).to_json_text_pretty()?);
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
		title: "Formal Specification".into(),
		formatter: Formatter::Markdown,
	};

	let arg = args.peek().map_or("--help".into(), |x| x.clone());
	if arg == "-V" || arg == "--version"
	{
		println!("rusk version 0.1.11");
		return Ok(());
	}
	if arg == "-h" || arg == "--help"
	{
		return usage();
	}

	if arg == "--debug"
	{
		opts.debug = true;
		args.next();
	}

	match arg.as_str()
	{
		"--markdown" => {
			opts.formatter = Formatter::Markdown;
			args.next();
		},
		"--rest" => {
			opts.formatter = Formatter::Rest;
			args.next();
		},
		"--json" => {
			opts.formatter = Formatter::Json;
			args.next();
		},
		"--json-pretty" => {
			opts.formatter = Formatter::JsonPretty;
			args.next();
		},
		"--doc-json" => {
			opts.formatter = Formatter::DocJson;
			args.next();
		},
		"--doc-json-pretty" => {
			opts.formatter = Formatter::DocJsonPretty;
			args.next();
		},
		_ => (),
	}

	if arg == "--title"
	{
		args.next();
		opts.title = args.next().expect("title");
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
