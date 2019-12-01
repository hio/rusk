//! # Markdown formatter
use crate::formatter::doc::{ Doc };


struct Opts
{
	in_code: bool,
}


impl Opts
{
	fn new() -> Opts
	{
		Opts {
			in_code: false,
		}
	}


	fn in_code() -> Opts
	{
		Opts {
			in_code: true,
		}
	}
}


pub trait ToMarkdownText
{
	fn to_markdown_text(&self) -> String;
}


impl ToMarkdownText for Doc
{
	fn to_markdown_text(&self) -> String
	{
		let mut s = String::new();
		self.encode(&mut s, &Opts::new()).unwrap();
		s
	}
}

trait WriteMarkdownText
{
	fn encode(&self, f: &mut impl std::fmt::Write, opts: &Opts) -> std::fmt::Result;
}


impl WriteMarkdownText for Doc
{
	fn encode(&self, f: &mut impl std::fmt::Write, opts: &Opts) -> std::fmt::Result
	{
		match self
		{
			Doc::Empty => Ok(()),
			Doc::Title(title) => {
				write!(f, "---\n")?;
				write!(f, "title: {}\n", title)?;
				write!(f, "---\n")?;
				Ok(())
			},
			Doc::Heading(n, doc) => {
				write!(f, "{} ", String::from("#").repeat(*n))?;
				doc.encode(f, opts)?;
				write!(f, "\n")?;
				Ok(())
			},
			Doc::Fragment(ref v) => {
				for elem in v.as_ref()
				{
					elem.encode(f, opts)?;
				}
				Ok(())
			},
			Doc::Number(n) => write!(f, "{}", n),
			Doc::String(ref s) => escape(f, s, opts),
			Doc::Static(ref s) => write!(f, "{}", s),
			Doc::Br => write!(f, "<br />"),
			Doc::Code(ref node) => {
				write!(f, "``")?;
				node.encode(f, &Opts::in_code())?;
				write!(f, "``")
			},

			Doc::SepBy(ref sep, ref v) =>
				sep_end_by(f, &Doc::Static(sep), &Doc::Empty, v, opts),
			Doc::SepByDoc(ref sep, ref v) =>
				sep_end_by(f, sep, &Doc::Empty, v, opts),
			Doc::SepEndBy(ref sep, ref end, ref v) =>
				sep_end_by(f, &Doc::Static(sep), &Doc::Static(end), v, opts),

			Doc::Table(ref header, ref rows) => {
				write!(f, "|")?;
				for elem in header.as_ref()
				{
					write!(f, " ")?;
					elem.encode(f, opts)?;
					write!(f, " |")?;
				}
				write!(f, "\n")?;

				write!(f, "|{}\n", String::from("---|").repeat(header.len()))?;

				for row in rows.iter()
				{
					write!(f, "|")?;
					for elem in row.as_ref()
					{
						write!(f, " ")?;
						elem.encode(f, opts)?;
						write!(f, " |")?;
					}
					write!(f, "\n")?;
				}

				Ok(())
			},

			Doc::Cell(ref v) => {
				//XXX: should be processed by inner works.
				let mut s = String::new();
				v.encode(&mut s, opts)?;
				let s = s.trim().replace("\n", "<br />");
				let s = s.replace("|", "\\|");
				write!(f, "{}", s)?;
				Ok(())
			},
		}
	}
}


fn sep_end_by(f: &mut impl std::fmt::Write, sep: &Doc, end: &Doc, v: &Vec<Doc>, opts: &Opts) -> std::fmt::Result
{
	let mut iter = v.iter();
	match iter.next()
	{
		None => (),
		Some(ref first) => {
			first.encode(f, opts)?;
			for item in iter
			{
				sep.encode(f, opts)?;
				item.encode(f, opts)?;
			}
			()
		},
	}
	end.encode(f, opts)
}


fn escape(f: &mut impl std::fmt::Write, s: &String, opts: &Opts) -> std::fmt::Result
{
	if opts.in_code
	{
		write!(f, "{}", s)?;
	}else
	{
		let mut iter = s.chars();
		while let Some(ch) = iter.next()
		{
			match ch
			{
				'\\' => write!(f, "\\\\")?,
				'_' => write!(f, "\\{}", ch)?,
				ch => write!(f, "{}", ch)?,
			}
		}
	}
	Ok(())
}
