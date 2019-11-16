//! # Markdown formatter
use crate::formatter::doc::{ Doc };

pub trait ToMarkdownText
{
	fn to_markdown_text(&self) -> String;
	fn encode(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result;
}

impl ToMarkdownText for Doc
{
	fn to_markdown_text(&self) -> String
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
				write!(f, "{} ", String::from("#").repeat(*n))?;
				doc.encode(f)?;
				write!(f, "\n")?;
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
				let mut s = String::new();
				node.encode(&mut s)?;
				let s = s.replace("\\_", "_"); // XXX: should be processed by inner works.
				write!(f, "{}", s)?;
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
				write!(f, "|")?;
				for elem in v.as_ref()
				{
					write!(f, " ")?;
					elem.encode(f)?;
					write!(f, " |")?;
				}
				write!(f, "\n")?;

				write!(f, "|{}\n", String::from("---|").repeat(v.len()))?;
				Ok(())
			},

			Doc::Row(ref v) => {
				write!(f, "|")?;
				for elem in v.as_ref()
				{
					write!(f, " ")?;
					elem.encode(f)?;
					write!(f, " |")?;
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
