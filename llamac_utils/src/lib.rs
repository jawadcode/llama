use std::{
    fmt::{self, Debug, Display},
    ops::{Add, Index, Range},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl Ident {
    pub fn new(string: &str) -> Self {
        Self(string.to_string())
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(ident) = self;
        f.write_str(ident)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// Custom span for storing the position of a token or AST node in the source string
pub struct Span {
    /// The start of the span (inclusive)
    pub start: usize,
    /// The end of the span (exclusive)
    pub end: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}...{}", self.start, self.end)
    }
}

impl Add for Span {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start,
            end: rhs.end,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T: Debug + Display + Clone> {
    pub span: Span,
    pub node: T,
}

impl<T: Debug + Display + Clone> Spanned<T> {
    pub fn map<U: Debug + Display + Clone, F: FnOnce(T) -> U>(self, op: F) -> Spanned<U> {
        Spanned {
            span: self.span,
            node: op(self.node),
        }
    }
    pub fn map_span<F: FnOnce(Span) -> Span>(self, op: F) -> Spanned<T> {
        Spanned {
            span: op(self.span),
            node: self.node,
        }
    }
}

impl<T: Debug + Display + Clone> Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        std::fmt::Display::fmt(&self.node, f)
    }
}

#[macro_export]
macro_rules! spanned {
    ($span:expr, $node:expr) => {
        $crate::Spanned {
            span: ($span).into(),
            node: $node,
        }
    };
}

pub struct FmtItems<'iter, I: Display, S: Display> {
    items: &'iter [I],
    sep: S,
}

impl<'slice, I: Display, S: Display> FmtItems<'slice, I, S> {
    pub fn new(items: &'slice [I], sep: S) -> Self {
        Self { items, sep }
    }
}

impl<I: Display, S: Display> Display for FmtItems<'_, I, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.items.iter();
        if let Some(next) = iter.next() {
            next.fmt(f)?;
        }
        for item in iter {
            self.sep.fmt(f)?;
            item.fmt(f)?;
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! parens_fmt {
    ($fmt:expr, $expr:expr) => {{
        $fmt.write_char('(')?;
        $expr.fmt($fmt)?;
        $fmt.write_char(')')
    }};
}
