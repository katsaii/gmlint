use std::cmp;
use crate::{
    lexer::{ Lexer, Span },
    token::TokenKind
};

/// Performs some simple checks on this code.
pub struct Checker<'a> {
    src : &'a str,
    lexer : Lexer<'a>,
    lines : Vec<Span>,
}

/// Produces a sorted list of source positions where a new line occurs.
pub fn prospect_newlines(src : &str) -> Vec<Span> {
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

/// Searches an array of newline spans for a specific byte position, `pos`.
pub fn binary_search_newlines(lines : &[Span], pos : usize) -> Result<usize, usize> {
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

/// Displays this error to the standard output.
pub fn display_error(
        span : Span,
        lines : &[Span],
        src : &str,
        filepath : &str,
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
    println!("\nWarning in {}:{}:{} -- {}", filepath, row, col, reason);
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
    println!("\n {} |{}{}", indent, " ".repeat(col),
            "^".repeat(underline_length));
}
