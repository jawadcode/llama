use std::{
    cell::RefCell,
    error::Error,
    fmt::{self, Debug, Display},
    hash::Hash,
    ops::{Add, DerefMut, Index, Range},
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

    pub fn map_res<U: Debug + Display + Clone, V: Error, F: FnOnce(T) -> Result<U, V>>(
        self,
        op: F,
    ) -> Result<Spanned<U>, V> {
        Ok(Spanned {
            span: self.span,
            node: op(self.node)?,
        })
    }

    pub fn map_ref<'a, U: Debug + Display + Clone, F: FnOnce(&'a T) -> U>(
        &'a self,
        op: F,
    ) -> Spanned<U> {
        Spanned {
            span: self.span,
            node: op(&self.node),
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

pub struct FmtItems<Things: Iterator<Item = Item>, Item: Display, Sep: Display> {
    items: RefCell<Things>,
    sep: Sep,
}

impl<Things: Iterator<Item = Item>, Item: Display, Sep: Display> FmtItems<Things, Item, Sep> {
    pub fn new(items: Things, sep: Sep) -> Self {
        Self {
            items: RefCell::new(items),
            sep,
        }
    }
}

impl<Things: Iterator<Item = Item>, Item: Display, Sep: Display> Display
    for FmtItems<Things, Item, Sep>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.items.borrow_mut();
        if let Some(next) = iter.next() {
            next.fmt(f)?;
        }
        for item in iter.deref_mut() {
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
