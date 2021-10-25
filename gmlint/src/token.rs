use std::fmt;

/// The span of bytes for a token in a GML source file.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct Span {
    pub begin : usize,
    pub end : usize,
}

impl Span {
    /// Creates a new span from these source positions.
    pub fn new(begin : usize, end : usize) -> Self {
        Self { begin, end }
    }

    /// Renders a substring using this span.
    pub fn render<'a>(&self, src : &'a str) -> &'a str {
        &src[self.begin..self.end]
    }
}

impl fmt::Display for Span {
    fn fmt(&self, out : &mut fmt::Formatter) -> fmt::Result {
        write!(out, "[{}..{}]", self.begin, self.end)
    }
}

/// Different types of token available to GML.
#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBox,
    RightBox,
    Colon,
    SemiColon,
    Comma,
    Dot,
    Operator,
    VarDecl,
    Literal,
    Comment,
    Space,
    Tab,
    EoL,
    EoF,
    Unknown,
}
