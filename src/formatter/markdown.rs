//! # Markdown formatter
use crate::formatter::doc::{ Doc, text_width };
use std::rc::Rc;


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


#[derive(Debug)]
struct RenderingCell
{
	s: String,
	width: usize,
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

			Doc::Table(ref header, ref rows) => write_table(f, opts, header, rows),

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


impl RenderingCell
{
	fn from_doc(doc: &Doc, opts: &Opts) -> RenderingCell
	{
		let mut s = String::new();
		doc.encode(&mut s, opts).unwrap();
		let width = text_width(&s);
		RenderingCell { s, width }
	}
}


fn write_table(f: &mut impl std::fmt::Write, opts: &Opts, header: &Rc<Vec<Doc>>, rows: &Rc<Vec<Rc<Vec<Doc>>>>) -> std::fmt::Result
{
	let header_cells = header.iter().map(
		|doc_cell| RenderingCell::from_doc(doc_cell, opts)
	).collect::<Vec<_>>();
	let body_cells = rows.iter().map(
		|doc_row| doc_row.iter().map(
			|doc_cell| RenderingCell::from_doc(doc_cell, opts)
		).collect::<Vec<_>>()
	).collect::<Vec<_>>();

	let widths = body_cells.iter().fold(
		header_cells.iter().map(|cell| cell.width).collect::<Vec<_>>(),
		|acc, row| {
			acc.iter()
				.zip(row.iter().map(|cell| cell.width))
				.map(|(a, b)| if *a > b { *a } else { b })
				.collect::<Vec<_>>()
		},
	);

	write!(f, "|")?;
	for (i, cell) in header_cells.iter().enumerate()
	{
		write!(f, " {1:0$} |", widths[i], cell.s)?;
	}
	write!(f, "\n")?;

	write!(f, "|")?;
	for width in &widths
	{
		write!(f, "{1:-<0$}|", width + 2, "")?;
	}
	write!(f, "\n")?;

	for row in body_cells.iter()
	{
		write!(f, "|")?;
		for (i, cell) in row.iter().enumerate()
		{
			write!(f, " {1:0$} |", widths[i], cell.s)?;
		}
		write!(f, "\n")?;
	}

	Ok(())
}
