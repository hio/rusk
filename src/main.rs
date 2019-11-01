#![type_length_limit="2097152"] // 1048576*2

//use clap::App;
use combine::parser::EasyParser;
use combine::stream::position;
use crate::formatter::ToMarkdownWithTitle;
use serde_json;
use std::env;
use std::fs::File;
use std::io::Read;

mod formatter;
mod node;
mod parser;

struct Opts
{
	debug: bool,
	title: String,
}

fn run(opts: &Opts, input: String) -> std::io::Result<()>
{
	if opts.debug
	{
		println!("--[source]----\n{}\n--[result]----", input);
	}

	let result = parser::parser()
		.easy_parse(position::Stream::new(input.as_str()));
	match result {
		Ok((module, _remaining_input)) => {
			if opts.debug
			{
				println!("OK: {:?}", module);
				println!("--[json]----\n{}", serde_json::to_string(&module)?);
				print!("--[markdown]----\n");
			}
			print!("{}", module.to_markdown(&opts.title));
		}
		Err(err) => {
			println!("ERROR: {}", err);
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
	println!("  --title {{title}}");
	Ok(())
}

fn main() -> std::io::Result<()>
{
	let mut args = env::args().peekable();
	args.next();

	let mut opts = Opts {
		debug: false,
		title: "Formal Specification".into(),
	};

	let arg = args.peek().map_or("--help".into(), |x| x.clone());
	if arg == "-V" || arg == "--version"
	{
		println!("rusk version 0.1.1");
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

	if arg == "--title"
	{
		args.next();
		opts.title = args.next().expect("title");
	}

	match args.next()
	{
		None => usage(),
		Some(arg1) => {
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
