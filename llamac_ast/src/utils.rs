use std::{
    fmt::{self, Display},
    ops::{Add, Index, Range},
};

#[derive(Clone, Copy, PartialEq)]
/// Custom span for storing the position of a token or AST node in the source string
pub struct Span {
    /// The start of the span (inclusive)
    pub start: usize,
    /// The end of the span (exclusive)
    pub end: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.start.fmt(f)?;
        f.write_str("...")?;
        self.end.fmt(f)
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

#[derive(Clone)]
pub struct Spanned<T: Display + Clone> {
    pub span: Span,
    pub node: T,
}

impl<T: Display + Clone> Spanned<T> {
    pub fn map<U: Display + Clone, F: FnOnce(T) -> U>(self, op: F) -> Spanned<U> {
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

impl<T: Display + Clone> Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.node.fmt(f)
    }
}

#[macro_export]
macro_rules! spanned {
    ($span:expr, $node:expr) => {
        $crate::utils::Spanned {
            span: ($span).into(),
            node: $node,
        }
    };
}

pub(crate) struct FmtItems<'iter, I: Display, S: Display> {
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
        let mut iter = self.items.into_iter();
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
