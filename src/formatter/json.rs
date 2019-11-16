//! # Json formatter
//!
//! Just using [`serde_json`].
//!
//! [`serde_json`]: https://github.com/serde-rs/json
use crate::ast;

pub trait ToJsonText
{
	fn to_json_text(&self) -> serde_json::Result<String>;
	fn to_json_text_pretty(&self) -> serde_json::Result<String>;
}

impl ToJsonText for ast::Module
{
	fn to_json_text(&self) -> serde_json::Result<String>
	{
		serde_json::to_string(self)
	}

	fn to_json_text_pretty(&self) -> serde_json::Result<String>
	{
		serde_json::to_string_pretty(self)
	}
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
