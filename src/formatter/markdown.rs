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
	lines: Vec<String>,
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
			Doc::Plain(ref s) => escape(f, s, opts),
			Doc::Marked(ref s) => write!(f, "{}", s),
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

			Doc::Cell(ref v) => v.encode(f, opts),
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
		let lines = s.split('\n').map(|s| s.into()).collect::<Vec<_>>();
		let width = lines.iter().map(|s| text_width(s)).max().unwrap_or(0);
		RenderingCell { lines, width }
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

	let widths:Vec<usize> = body_cells.iter().fold(
		header_cells.iter().map(|cell| cell.width).collect::<Vec<_>>(),
		|acc, row| {
			acc.iter()
				.zip(row.iter().map(|cell| cell.width))
				.map(|(a, b)| if *a > b { *a } else { b })
				.collect::<Vec<_>>()
		},
	);

	let row_sep = {
		let mut s = String::from("+");
		for width in &widths
		{
			s += &"-".repeat(width + 2);
			s.push('+');
		}
		s.push('\n');
		s
	};

	write!(f, "{}", row_sep)?;

	write_row(f, &header_cells, &widths)?;
	write!(f, "{}", row_sep.replace("-", "="))?;

	for row in body_cells.iter()
	{
		write_row(f, &row, &widths)?;
		write!(f, "{}", row_sep)?;
	}

	Ok(())
}


fn write_row(f: &mut impl std::fmt::Write, row: &Vec<RenderingCell>, widths: &Vec<usize>) -> std::fmt::Result
{
	let height = row.iter().map(|cell| cell.lines.len()).max().unwrap_or(1);

	for li in 0..height
	{
		write!(f, "|")?;
		for (i, cell) in row.iter().enumerate()
		{
			write!(f, " {1:0$} |", widths[i], cell.lines.get(li).unwrap_or(&String::new()))?;
		}
		write!(f, "\n")?;
	}
	Ok(())
}
