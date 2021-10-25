use std::{ str::CharIndices, mem, fmt };
use crate::token::TokenKind;

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

/// Converts a stream of characters into lexemes.
pub struct Lexer<'a> {
    src : &'a str,
    chars : CharIndices<'a>,
    current_char : Option<char>,
    ignore_next_char : bool,
    directive_mode : bool,
    span : Span,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer from this source file.
    pub fn new(src : &'a str) -> Self {
        let mut chars = src.char_indices();
        let current_char = chars.next().map(|x| x.1);
        let ignore_next_char = false;
        let directive_mode = false;
        let span = Span::default();
        Self { src, chars, current_char,
                ignore_next_char, directive_mode, span }
    }

    /// Returns a reference to the current span.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Returns the substring of the current span.
    pub fn substring(&self) -> &'a str {
        self.span.render(self.src)
    }

    /// Clears the current substring.
    pub fn clear_span(&mut self) {
        self.span.begin = self.span.end;
    }

    /// Returns `true` if the current character satisfies the predicate `p`.
    /// The function will always return `false` when at the end of the file.
    pub fn sat(&self, p : fn(&char) -> bool) -> bool {
        if let Some(x) = &self.current_char {
            p(x)
        } else {
            false
        }
    }

    /// Advances the lexer and returns the previous character.
    pub fn advance(&mut self) -> Option<char> {
        let future_char = if let Some((i, c)) = self.chars.next() {
            self.span.end = i;
            Some(c)
        } else {
            self.span.end = self.src.len();
            None
        };
        mem::replace(&mut self.current_char, future_char)
    }

    /// Advances the lexer whilst some preciate holds.
    pub fn advance_while(&mut self, p : fn(&char) -> bool) {
        loop {
            if self.sat(p) {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Advances the lexer and returns the next `TokenKind`.
    pub fn generate_token(&mut self) -> TokenKind {
        if self.ignore_next_char {
            self.advance();
            self.ignore_next_char = false;
        }
        self.clear_span();
        if let Some(c) = self.advance() {
            if is_newline(&c) {
                self.advance_while(is_newline);
                TokenKind::EoL
            } else if is_tab(&c) {
                self.advance_while(is_tab);
                TokenKind::Tab
            } else if is_space(&c) {
                self.advance_while(is_space);
                TokenKind::Space
            } else if self.directive_mode {
                match c {
                    x if is_newline(&x) => {
                        self.directive_mode = false;
                        self.generate_token()
                    },
                    x if is_ascii_graphic_kebab(&x) => {
                        self.advance_while(is_ascii_graphic_kebab);
                        match self.substring() {
                            "ALLOW" => TokenKind::DirectiveAllow,
                            "WARN" => TokenKind::DirectiveWarn,
                            _ => TokenKind::DirectiveOption,
                        }
                    },
                    _ => TokenKind::Other,
                }
            } else {
                match c {
                    '/' => if self.sat(|x| matches!(x, '/')) {
                        self.advance();
                        if self.sat(|x| matches!(x, '#')) {
                            self.directive_mode = true;
                            self.generate_token()
                        } else {
                            self.advance_while(|x| !is_newline(x));
                            TokenKind::Comment
                        }
                    } else if self.sat(|x| matches!(x, '*')) {
                        self.advance();
                        loop {
                            if self.sat(|x| matches!(x, '*')) {
                                self.advance();
                                if self.sat(|x| matches!(x, '/')) {
                                    self.advance();
                                    break TokenKind::Comment;
                                }
                            } else if let None = self.advance() {
                                break TokenKind::Comment;
                            }
                        }
                    } else {
                        TokenKind::Other
                    },
                    '(' => TokenKind::LeftParen,
                    ')' => TokenKind::RightParen,
                    '{' => TokenKind::LeftBrace,
                    '}' => TokenKind::RightBrace,
                    '[' => TokenKind::LeftBox,
                    ']' => TokenKind::RightBox,
                    '.' => TokenKind::Dot,
                    x if is_ascii_letter(&x) => {
                        self.advance_while(is_ascii_graphic);
                        if matches!(self.substring(), "var" | "static") {
                            TokenKind::VarDecl
                        } else {
                            TokenKind::Identifier
                        }
                    },
                    x if is_ascii_digit(&x) => {
                        if matches!(x, '0') && self.sat(|x| matches!(x, 'x')) {
                            self.advance_while(is_hex_digit);
                        } else {
                            self.advance_while(is_ascii_digit);
                        }
                        TokenKind::Other
                    },
                    _ => TokenKind::Other,
                }
            }
        } else {
            TokenKind::EoF
        }
    }
}

impl Into<Vec<TokenKind>> for Lexer<'_> {
    fn into(mut self) -> Vec<TokenKind> {
        let mut tokens = Vec::new();
        loop {
            let token = self.generate_token();
            if matches!(token, TokenKind::EoF) {
                break tokens;
            }
            tokens.push(token);
        }
    }
}

fn is_newline(c : &char) -> bool {
    matches!(c, '\n' | '\r')
}

fn is_tab(c : &char) -> bool {
    matches!(c, '\t')
}

fn is_space(c : &char) -> bool {
    c.is_whitespace() && !is_newline(c) && !is_tab(c)
}

fn is_ascii_digit(c : &char) -> bool {
    c.is_ascii_digit()
}

fn is_ascii_letter(c : &char) -> bool {
    c.is_ascii_alphabetic()
}

fn is_ascii_graphic(c : &char) -> bool {
    is_ascii_letter(c) || is_ascii_digit(c) || matches!(c, '_')
}

fn is_ascii_graphic_kebab(c : &char) -> bool {
    is_ascii_graphic(c) || matches!(c, '-')
}

fn is_hex_digit(c : &char) -> bool {
    is_ascii_digit(c) || matches!(c,
            'a' | 'b' | 'c' | 'd' | 'e' | 'f' |
            'A' | 'B' | 'C' | 'D' | 'E' | 'F')
}
