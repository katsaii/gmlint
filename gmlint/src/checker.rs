use std::{
    collections::{ HashSet },
    cmp, fs, mem
};
use crate::{
    lexer::{ Lexer, Span },
    token::TokenKind
};

/// Performs checks on this file and prints errors to the standard output.
pub fn check_file(filepath : &str, illegal_functions : &[String]) {
    if let Ok(src) = fs::read_to_string(filepath) {
        let checker = Checker::new(filepath, &src, illegal_functions);
        checker.perform_checks();
    }
}

/// The type of indent style.
#[derive(Debug, PartialEq)]
pub enum IndentStyle {
    Space,
    Tab,
    Unknown,
}

/// Performs some simple checks on this code.
pub struct Checker<'a> {
    filepath : String,
    src : &'a str,
    lines : Vec<Span>,
    lexer : Lexer<'a>,
    peeked : TokenKind,
    peeked_span : Span,
    illegal_functions : HashSet<String>,
    directive_allow : bool,
    indent_style : IndentStyle,
}

impl<'a> Checker<'a> {
    /// Creates a new checker for this source file.
    pub fn new(
            filepath : &str,
            src : &'a str,
            illegal_function_list : &'a [String]) -> Self {
        let filepath = filepath.to_string();
        let lines = prospect_newlines(src);
        let lexer = Lexer::new(src);
        let peeked = TokenKind::BoF;
        let peeked_span = Span::default();
        let mut illegal_functions = HashSet::new();
        let directive_allow = false;
        let indent_style = IndentStyle::Unknown;
        for name in illegal_function_list {
            illegal_functions.insert(name.to_string());
        }
        Self { filepath, src, lines, lexer, peeked, peeked_span,
                illegal_functions, directive_allow, indent_style }
    }

    /// Returns the substring of the current span.
    pub fn substring(&self) -> &'a str {
        self.peeked_span.render(self.src)
    }

    /// Displays an error.
    pub fn error(&self, option : &str, reason : &str) {
        display_error(&self.peeked_span, &self.lines,
                self.src, &self.filepath, option, reason);
    }

    /// Skips whitespace and reports any changes in indentation.
    /// Returns `None` if the end-of-file is reached.
    pub fn generate_token(&mut self) -> Option<TokenKind> {
        let mut newline = false;
        let token = loop {
            self.peeked_span = self.lexer.span().clone();
            let token = mem::replace(
                    &mut self.peeked, self.lexer.generate_token());
            match token {
                TokenKind::EoL | TokenKind::BoF => {
                    newline = true;
                },
                TokenKind::Comment | TokenKind::Other => (),
                TokenKind::Tab => {
                    if !newline {
                        self.error("bad-tab-style",
                                "Tab is used here when it shouldn't be");
                    } else if self.indent_style == IndentStyle::Unknown {
                        self.indent_style = IndentStyle::Tab;
                    } else if self.indent_style == IndentStyle::Space {
                        self.error("inconsistent-indent",
                                "Expected a space, but found a tab");
                    }
                },
                TokenKind::Space => {
                    if self.indent_style == IndentStyle::Unknown {
                        self.indent_style = IndentStyle::Space;
                    } else if newline && self.indent_style == IndentStyle::Tab {
                        self.error("inconsistent-indent",
                                "Expected a tab, but found a space");
                    }
                },
                TokenKind::DirectiveAllow => {
                    self.directive_allow = true;
                },
                TokenKind::DirectiveWarn => {
                    self.directive_allow = false;
                },
                TokenKind::DirectiveOption => {
                    let directive = self.lexer.substring();
                    println!("{} {}", if self.directive_allow { "allow" } else { "warn" }, directive);
                },
                _ => break token,
            }
        };
        if matches!(token, TokenKind::EoF) {
            None
        } else {
            Some(token)
        }
    }

    /// Runs the checks and consumes this checker.
    pub fn perform_checks(mut self) -> Option<()> {
        loop {
            let token = self.generate_token()?;
            match token {
                TokenKind::Identifier => {
                    if self.illegal_functions.contains(self.substring()) {
                        self.error("illegal-functions", "Accessing this variable is prohibited");
                    }
                },
                _ => (),
            }
        }
        Some(())
    }
}

fn prospect_newlines(src : &str) -> Vec<Span> {
    let mut begin = 0;
    let mut locations = Vec::new();
    let mut chars = src.char_indices().peekable();
    while let Some((end, next)) = chars.next() {
        if let '\r' | '\n' = next {
            if next == '\r' && matches!(chars.peek(), Some((_, '\n'))) {
                chars.next();
            }
        } else {
            continue;
        }
        locations.push(Span::new(begin, end));
        begin = if let Some((i, _)) = chars.peek() {
            *i
        } else {
            src.len()
        };
    }
    locations.push(Span::new(begin, src.len()));
    locations
}

fn binary_search_newlines(
        lines : &[Span], pos : usize) -> Result<usize, usize> {
    lines.binary_search_by(|x| {
        if x.begin > pos {
            cmp::Ordering::Greater
        } else if x.end < pos {
            cmp::Ordering::Less
        } else {
            cmp::Ordering::Equal
        }
    })
}

fn display_error(
        span : &Span,
        lines : &[Span],
        src : &str,
        filepath : &str,
        option : &str,
        reason : &str) {
    let error_begin = span.begin;
    let error_end = span.end;
    let line_begin = binary_search_newlines(&lines, error_begin).unwrap();
    let line_end = binary_search_newlines(&lines, error_end).unwrap();
    let Span { begin : start, end } = lines.get(line_begin).unwrap();
    let row = line_begin + 1;
    let col = error_begin - start + 1;
    let mut indent_length =
            (((line_begin + 1) as f64).log10() + 1.0).floor() as usize;
    if indent_length == 0 {
        indent_length = 1;
    }
    println!("\nWarning in {}:{}:{}", filepath, row, col);
    let indent = " ".repeat(indent_length);
    // underline error
    let mut underline_length = error_end - error_begin;
    if underline_length < 1 {
        underline_length = 1;
    }
    print!(" {:width$} | {}", row,
            &src[*start..*end].replace("\t", " "), width=indent_length);
    if line_begin != line_end {
        let lines_omitted = line_end - line_begin;
        print!(" ... ({} line{} omitted)", lines_omitted,
                if lines_omitted == 1 { "" } else { "s" });
    }
    println!("\n {} :{}{} {}", indent, " ".repeat(col),
            "^".repeat(underline_length), reason);
    println!(" {} ? If this is intentional, include `//# ALLOW {}` before line {}",
            indent, option, row)
}
