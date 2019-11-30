//! # ReST formatter
use crate::formatter::doc::{ Doc };

pub trait ToRestText
{
	fn to_rest_text(&self) -> String;
	fn encode(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;
}

impl ToRestText for Doc
{
	fn to_rest_text(&self) -> String
	{
		let mut s = String::new();
		self.encode(&mut s).unwrap();
		s
	}

	fn encode(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result
	{
		match self
		{
			Doc::Empty => Ok(()),
			Doc::Heading(n, doc) => {
				let s = doc.to_rest_text();
				let width = text_width(&s);
				match n
				{
					2 => {
						write!(f, "{}\n", String::from("=").repeat(width))?;
						write!(f, "{}\n", s)?;
						write!(f, "{}\n", String::from("=").repeat(width))?;
					},
					3 => {
						write!(f, "{}\n", s)?;
						write!(f, "{}\n", String::from("=").repeat(width))?;
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
					elem.encode(f)?;
				}
				Ok(())
			},
			Doc::Number(n) => write!(f, "{}", n),
			Doc::String(ref s) => escape(f, s),
			Doc::Static(ref s) => write!(f, "{}", s),
			Doc::Br => write!(f, "<br />"),
			Doc::Code(ref node) => {
				write!(f, "``")?;
				node.encode(f)?;
				write!(f, "``")
			},

			Doc::SepBy(ref sep, ref v) =>
				sep_end_by(f, &Doc::Static(sep), &Doc::Empty, v),
			Doc::SepByDoc(ref sep, ref v) =>
				sep_end_by(f, sep, &Doc::Empty, v),

			Doc::SepEndBy(ref sep, ref end, ref v) => {
				let mut iter = v.iter();
				match iter.next()
				{
					None => Ok(()),
					Some(ref first) => {
						first.encode(f)?;
						for i in iter
						{
							write!(f, "{}", sep)?;
							i.encode(f)?;
						}
						write!(f, "{}", end)?;
						Ok(())
					},
				}
			},

			Doc::HeaderRow(ref v) => {
				write!(f, ".. list-table::\n\n")?;
				write!(f, "  *\n")?;
				for elem in v.as_ref()
				{
					write!(f, "    - ")?;
					elem.encode(f)?;
					write!(f, "\n")?;
				}
				write!(f, "\n")?;

				Ok(())
			},

			Doc::Row(ref v) => {
				write!(f, "  *\n")?;
				for elem in v.as_ref()
				{
					write!(f, "   - ")?;
					elem.encode(f)?;
					write!(f, "\n")?;
				}
				write!(f, "\n")?;
				Ok(())
			},

			Doc::Cell(ref v) => {
				//XXX: should be processed by inner works.
				let mut s = String::new();
				v.encode(&mut s)?;
				let s = s.trim().replace("\n", "<br />");
				let s = s.replace("|", "\\|");
				write!(f, "{}", s)?;
				Ok(())
			},
		}
	}
}

fn text_width(s: &String) -> usize
{
	// lazy calculation.
	// consider UAX #11: East Asian Width.
	s.chars().map(|c| if c < '\u{0100}' { 1 } else { 2 } ).sum()
}


	fn sep_end_by(f: &mut impl std::fmt::Write, sep: &Doc, end: &Doc, v: &Vec<Doc>) -> std::fmt::Result
	{
		let mut iter = v.iter();
		match iter.next()
		{
			None => (),
			Some(ref first) => {
				first.encode(f)?;
				for item in iter
				{
					sep.encode(f)?;
					item.encode(f)?;
				}
				()
			},
		}
		end.encode(f)
	}

	fn escape(f: &mut impl std::fmt::Write, s: &String) -> std::fmt::Result
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
		Ok(())
	}
