//! # ReST formatter
use crate::formatter::doc::{ Doc, text_width };


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


pub trait ToRestText
{
	fn to_rest_text(&self) -> String;
}


impl ToRestText for Doc
{
	fn to_rest_text(&self) -> String
	{
		let mut s = String::new();
		self.encode(&mut s, &Opts::new()).unwrap();
		s
	}
}


trait WriteRestText
{
	fn encode(&self, f: &mut impl std::fmt::Write, opts: &Opts) -> std::fmt::Result;
}


impl WriteRestText for Doc
{
	fn encode(&self, f: &mut impl std::fmt::Write, opts: &Opts) -> std::fmt::Result
	{
		match self
		{
			Doc::Empty => Ok(()),
			Doc::Title(title) => {
				let width = text_width(title);
				write!(f, "{}\n", String::from("=").repeat(width))?;
				write!(f, "{}\n", title)?;
				write!(f, "{}\n", String::from("=").repeat(width))?;
				write!(f, "\n")?;
				write!(f, ".. raw:: never // workaround for pandoc.\n")?;
				Ok(())
			},
			Doc::Heading(n, doc) => {
				let s = doc.to_rest_text();
				let width = text_width(&s);
				match n
				{
					2 => {
						write!(f, "{}\n", s)?;
						write!(f, "{}\n", String::from("=").repeat(width))?;
					},
					3 => {
						write!(f, "{}\n", s)?;
						write!(f, "{}\n", String::from("-").repeat(width))?;
					},
					_ => {
						panic!("invalid heading level");
					},
				}
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
			Doc::String(ref s) => escape(f, s, opts, false),
			Doc::Static(ref s) => escape(f, s, opts, true),
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
				write!(f, ".. list-table::\n")?;
				write!(f, "   :header-rows: 1\n")?;
				write!(f, "\n")?;
				let mut iter = header.iter();
				if let Some(first) = iter.next()
				{
					write!(f, "  * - ")?;
					first.encode(f, opts)?;
					write!(f, "\n")?;

					for elem in iter
					{
						write!(f, "    - ")?;
						elem.encode(f, opts)?;
						write!(f, "\n")?;
					}
				}else
				{
					write!(f, "  * - (no columns)\n")?;
				}
				write!(f, "\n")?;

				for row in rows.iter()
				{
					let mut iter = row.iter();
					if let Some(first) = iter.next()
					{
						write!(f, "  * - ")?;
						first.encode(f, opts)?;
						write!(f, "\n")?;

						for elem in iter
						{
							write!(f, "    - ")?;
							elem.encode(f, opts)?;
							write!(f, "\n")?;
						}
					}else
					{
						write!(f, "  * - (no columns)\n")?;
					}
					write!(f, "\n")?;
				}

				Ok(())
			},

			Doc::Cell(ref v) => {
				v.encode(f, opts)?;
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


fn escape(f: &mut impl std::fmt::Write, s: &str, opts: &Opts, is_static: bool) -> std::fmt::Result
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
				'_' => write!(f, "\\_")?,
				'|' => write!(f, "\\|")?,
				'\n' if !is_static => write!(f, "<br />")?,
				ch => write!(f, "{}", ch)?,
			}
		}
	}
	Ok(())
}
