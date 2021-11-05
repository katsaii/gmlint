use std::{
    collections::{ HashMap, HashSet },
    path::Path, ffi::OsStr,
    cmp, fs, mem, env, io, fmt,
};
use crate::{
    lexer::{ Lexer, Span },
    token::TokenKind
};
use yaml_rust::YamlLoader;
use gitignore::File as IgnoreFile;
use glob::Pattern;

macro_rules! rule {
    ($($p:pat)|*) => { |x| matches!(x, $($p)|*) }
}

/// Searches a file with this filename, but one of an array of file
/// extensions.
pub fn read_to_string_with_extension<P : AsRef<Path>>(
        dir : P,
        name : &'static str,
        exts : &[&'static str]) -> Option<String> {
    for ext in exts {
        let filepath = dir.as_ref().join(format!("{}.{}", name, ext));
        if let Ok(content) = fs::read_to_string(filepath) {
            return Some(content);
        }
    }
    None
}

/// Loads the banned function list and global directive options from a
/// YAML file.
pub fn load_config<P : AsRef<Path>>(root : P)
        -> Option<(Vec<(Pattern, Option<String>)>, Vec<(String, bool)>)> {
    let mut banned_functions = Vec::new();
    let mut directives = Vec::new();
    if let Some(yaml_content) = read_to_string_with_extension(
            root.as_ref(), "gmlint", &["yml", "yaml", "json"]) {
        if let Ok(yaml) = YamlLoader::load_from_str(&yaml_content) {
            if yaml.len() >= 1 {
                let doc = &yaml[0];
                if let Some(fields) = doc["banned"].as_vec() {
                    for field in fields {
                        let pattern = if let Ok(pattern) = Pattern::new(
                                field["pattern"].as_str()?) {
                            pattern
                        } else {
                            return None;
                        };
                        let suggestion = field["instead"].as_str()
                                .map(|x| x.to_string());
                        banned_functions.push((pattern, suggestion));
                    }
                }
                if let Some(names) = doc["allow"].as_vec() {
                    for name in names {
                        directives.push(
                                (name.as_str()?.to_string(), false));
                    }
                }
                if let Some(names) = doc["warn"].as_vec() {
                    for name in names {
                        directives.push(
                                (name.as_str()?.to_string(), true));
                    }
                }
            }
        }
    }
    Some((banned_functions, directives))
}

/// Loads the configuration file and the ignore file and uses it to check
/// all the GML files in the project directory.
pub fn check_project<P : AsRef<Path>>(project_dir : P) -> io::Result<()> {
    let mut root = env::current_dir()?;
    root.push(project_dir);
    let (banned_functions, directives) =
            load_config(&root).unwrap_or((vec![], vec![]));
    let ignorepath = root.join(".gmlintignore");
    let create_ignorefile = !ignorepath.exists();
    if create_ignorefile {
        fs::File::create(&ignorepath)?;
    }
    let paths = IgnoreFile::new(&ignorepath).
            and_then(|ok| ok.included_files()).map_err(|err| {
                io::Error::new(io::ErrorKind::Other, err)
            })?;
    let mut visited_directives = HashSet::new();
    for path in paths {
        if let Some("gml") = path.extension().and_then(OsStr::to_str) {
            check_file(path, &banned_functions, &directives,
                    &mut visited_directives)?;
        }
    }
    if create_ignorefile {
        fs::remove_file(&ignorepath)?;
    }
    Ok(())
}

/// Performs checks on this file and prints errors to the standard output.
pub fn check_file<P : AsRef<Path>>(
        filepath : P,
        banned_functions : &[(Pattern, Option<String>)],
        directives : &[(String, bool)],
        visited_directives : &mut HashSet<String>) -> io::Result<()> {
    if let Some(filepath_rel) =
            pathdiff::diff_paths(&filepath, env::current_dir()?) {
        let filename = filepath_rel.to_str().unwrap_or("").to_string();
        let src = fs::read_to_string(filepath)?;
        let checker = Checker::new(
                &filename, &src, banned_functions, directives,
                visited_directives);
        checker.perform_checks();
    }
    Ok(())
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
    banned_functions : Vec<(Pattern, Option<String>)>,
    directive_warn : bool,
    indent_style : IndentStyle,
    directives : HashMap<String, bool>,
    error_count : usize,
    visited_directives : &'a mut HashSet<String>,
}

impl<'a> Checker<'a> {
    /// Creates a new checker for this source file.
    pub fn new(
            filepath : &str,
            src : &'a str,
            banned_function_list : &'a [(Pattern, Option<String>)],
            directive_list : &[(String, bool)],
            visited_directives : &'a mut HashSet<String>) -> Self {
        let filepath = filepath.to_string();
        let lines = prospect_newlines(src);
        let lexer = Lexer::new(src);
        let peeked = TokenKind::BoF;
        let peeked_span = Span::default();
        let directive_warn = true;
        let indent_style = IndentStyle::Unknown;
        let banned_functions = banned_function_list
                .into_iter()
                .map(|x| x.clone())
                .collect();
        let directives = directive_list
                .into_iter()
                .map(|x| x.clone())
                .collect();
        let error_count = 0;
        let mut checker = Self { filepath, src, lines, lexer, peeked, peeked_span,
                banned_functions, directive_warn, indent_style,
                directives, error_count, visited_directives };
        checker.advance(); // skip BoF token
        checker
    }

    /// Returns a reference to the current span.
    pub fn span(&self) -> &Span {
        &self.peeked_span
    }

    /// Returns the substring of the current span.
    pub fn substring(&self) -> &'a str {
        self.span().render(self.src)
    }

    /// Displays an error.
    pub fn error<T : fmt::Display>(
            &mut self,
            option : &str,
            span : &Span,
            reason : T) {
        let enabled = matches!(self.directives.get(option), None | Some(true));
        if enabled == directive_enabled_by_default(option) {
            if self.error_count != 0 {
                println!();
            }
            self.error_count += 1;
            display_error(&span, &self.lines, self.src,
                    &self.filepath, option, reason,
                    &mut self.visited_directives);
        }
    }

    /// Skips whitespace and reports any changes in indentation.
    pub fn generate_token(&mut self) -> TokenKind {
        let mut newline = false;
        loop {
            let span = self.lexer.span().clone();
            let mut token = self.lexer.generate_token();
            match token {
                TokenKind::EoL | TokenKind::BoF => {
                    newline = true;
                },
                TokenKind::Comment { unclosed } => {
                    if unclosed {
                        self.error("unclosed-comment", &span,
                                "multi-line comment is never terminated");
                    }
                },
                TokenKind::Tab => {
                    if newline {
                        self.error("prefer-space-indent", &span,
                                "indents should use spaces, but found a tab");
                        if self.indent_style == IndentStyle::Unknown {
                            self.indent_style = IndentStyle::Tab;
                        } else if self.indent_style == IndentStyle::Space {
                            self.error("inconsistent-indent", &span,
                                    "expected a space, but found a tab");
                        }
                    } else {
                        self.error("bad-tab-style", &span,
                                "tab is used here when it shouldn't be");
                    }
                },
                TokenKind::Space => {
                    if newline {
                        self.error("prefer-tab-indent", &span,
                                "indents should use tabs, but found a space");
                        if self.indent_style == IndentStyle::Unknown {
                            self.indent_style = IndentStyle::Space;
                        } else if self.indent_style == IndentStyle::Tab {
                            self.error("inconsistent-indent", &span,
                                    "expected a tab, but found a space");
                        }
                    }
                },
                TokenKind::DirectiveAllow => {
                    self.directive_warn = false;
                },
                TokenKind::DirectiveWarn => {
                    self.directive_warn = true;
                },
                TokenKind::DirectiveOption => {
                    let directive = self.substring();
                    self.directives.insert(
                            directive.to_string(), self.directive_warn);
                },
                TokenKind::Other => {
                    self.error("invalid-character", &span,
                            "invalid character sequence found");
                }
                _ => {
                    match &token {
                        TokenKind::Number {
                                missing_integral, missing_fractional } => {
                            if *missing_integral {
                                self.error("float-missing-integral", &span,
                                        "an explicit `0` should be included \
                                        before the decimal point of this \
                                        number");
                            }
                            if *missing_fractional {
                                self.error("float-missing-fractional", &span,
                                        "an explicit `0` should be included \
                                        after the decimal point of this \
                                        number");
                            }
                        },
                        TokenKind::NumberHex { delphi_style } => {
                            if *delphi_style {
                                self.error("delphi-syntax", &span,
                                        "instead of `$` for hexadecimal \
                                        literals, you should use `0x`");
                            } else {
                                self.error("c-syntax", &span,
                                        "instead of `0x` for hexadecimal \
                                        literals, you should use `$`");
                            }
                        },
                        TokenKind::Str { unclosed } => {
                            if *unclosed {
                                self.error("unclosed-string", &span,
                                        "missing a closing quote in this \
                                        string");
                            }
                        },
                        TokenKind::Equals => {
                            self.error("c-syntax", &span,
                                    "instead of `=`, you should use `:=`");
                        },
                        TokenKind::LeftBrace => {
                            self.error("c-syntax", &span,
                                    "instead of `{`, you should use `begin`");
                        },
                        TokenKind::RightBrace => {
                            self.error("c-syntax", &span,
                                    "instead of `}`, you should use `begin`");
                        },
                        TokenKind::ColonEquals => {
                            self.error("delphi-syntax", &span,
                                    "instead of `:=`, you should use `=`");
                            token = TokenKind::Equals;
                        },
                        TokenKind::Begin => {
                            self.error("delphi-syntax", &span,
                                    "instead of `begin`, you should use `{`");
                            token = TokenKind::LeftBrace;
                        },
                        TokenKind::End => {
                            self.error("delphi-syntax", &span,
                                    "instead of `end`, you should use `}`");
                            token = TokenKind::RightBrace;
                        },
                        _ => (),
                    }
                    break token;
                },
            }
        }
    }

    /// Advances the parser and returns the previous token.
    pub fn advance(&mut self) -> TokenKind {
        self.peeked_span = self.lexer.span().clone();
        let next = self.generate_token();
        mem::replace(&mut self.peeked, next)
    }

    /// Returns whether the current token satisfies a predicate `p`.
    pub fn sat(&self, p : fn(&TokenKind) -> bool) -> bool {
        p(&self.peeked)
    }

    /// Displays an error at the current span and returns `None`.
    pub fn unexpected<T : fmt::Display, R>(
            &mut self,
            reason : T) -> Option<R> {
        self.advance();
        self.error("syntax-errors", &self.span().clone(), reason);
        None
    }

    /// Returns the current token if it satisfies a predicate `p`.
    /// Otherwise, an error is displayed at the current span.
    pub fn expect<T : fmt::Display>(
            &mut self,
            p : fn(&TokenKind) -> bool,
            reason : T) -> Option<TokenKind> {
        if self.sat(p) {
            Some(self.advance())
        } else {
            self.unexpected(reason)
        }
    }

    /// Advances the lexer if the current token is a semicolon.
    /// Otherwise, an error is displayed at the current span.
    pub fn expect_semicolon<T : fmt::Display>(&mut self, reason : T) {
        if self.sat(rule!(TokenKind::SemiColon)) {
            self.advance();
        } else {
            self.error("implicit-semicolons", &self.span().clone(),
                    format!("expected a semicolon after {}",
                            reason));
        }
    }

    /// Recovers from an error by advancing the lexer to the start of a
    /// statement.
    pub fn recover(&mut self) {
        loop {
            if self.sat(rule!(
                    TokenKind::EoF
                    | TokenKind::Var
                    | TokenKind::Static
                    | TokenKind::Globalvar
                    | TokenKind::If
                    | TokenKind::For
                    | TokenKind::While
                    | TokenKind::Do
                    | TokenKind::Repeat
                    | TokenKind::With
                    | TokenKind::Break
                    | TokenKind::Continue
                    | TokenKind::Function
                    | TokenKind::Return
                    | TokenKind::Identifier)) {
                break;
            } else if self.sat(rule!(
                    TokenKind::SemiColon)) {
                self.advance();
                break;
            }
            self.advance();
        }
    }

    /// Runs the checks and consumes this checker.
    pub fn perform_checks(mut self) {
        self.check_program();
        if self.error_count > 0 {
            let plural = if self.error_count == 1 { "" } else { "s" };
            println!();
            println!("displayed {} error{} for {}",
                    self.error_count, plural, self.filepath);
            println!();
        }
    }

    /// Performs some checks for the program.
    pub fn check_program(&mut self) {
        while !self.sat(rule!(TokenKind::EoF)) {
            if let None = self.check_stmt() {
                self.recover();
            }
        }
    }

    /// Analyses statements.
    pub fn check_stmt(&mut self) -> Option<()> {
        self.check_stmt_expr()
    }

    /// Analyses expression statements.
    pub fn check_stmt_expr(&mut self) -> Option<()> {
        self.check_expr()?;
        if self.sat(rule!(
                TokenKind::Equals
                | TokenKind::PlusEquals
                | TokenKind::MinusEquals
                | TokenKind::AsteriskEquals
                | TokenKind::SolidusEquals
                | TokenKind::PercentEquals
                | TokenKind::LessLessEquals
                | TokenKind::GreaterGreaterEquals
                | TokenKind::AmpersandEquals
                | TokenKind::AmpersandAmpersandEquals
                | TokenKind::BarEquals
                | TokenKind::BarBarEquals
                | TokenKind::CaretEquals
                | TokenKind::CaretCaretEquals
                | TokenKind::HookHookEquals)) {
            self.advance();
            self.check_expr()?;
            self.expect_semicolon("after assignment statements");
        } else {
            self.expect_semicolon("after expression statements");
        }
        Some(())
    }

    /// Analyses expressions.
    pub fn check_expr(&mut self) -> Option<()> {
        self.check_expr_terminal()
    }

    /// Analyses literals and identifiers.
    pub fn check_expr_terminal(&mut self) -> Option<()> {
        if self.sat(rule!(TokenKind::Identifier)) {
            self.advance();
            let substring = self.substring();
            let span = self.span().clone();
            for (pattern, replacement) in &self.banned_functions {
                if pattern.matches(substring) {
                    let message = if let Some(name) = replacement {
                        format!(", instead use `{}`", name)
                    } else {
                        String::new()
                    };
                    self.error("banned-functions", &span,
                            format!("accessing this variable is \
                                    prohibited{}", message));
                    break;
                }
            }
            Some(())
        } else if self.sat(rule!(
                TokenKind::Number { .. }
                | TokenKind::NumberHex { .. }
                | TokenKind::Str { .. }
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Infinity
                | TokenKind::NaN
                | TokenKind::Undefined)) {
            self.advance();
            Some(())
        } else {
            self.check_expr_grouping()
        }
    }

    /// Analyses groupings of expressions.
    pub fn check_expr_grouping(&mut self) -> Option<()> {
        if self.sat(rule!(TokenKind::LeftParen)) {
            self.advance();
            self.check_expr()?;
            self.expect(rule!(TokenKind::RightParen),
                    "expected a closing `)` here")?;
            Some(())
        } else if self.sat(rule!(TokenKind::LeftBox)) {
            self.advance();
            if !self.sat(rule!(TokenKind::RightBox)) {
                loop {
                    self.check_expr()?;
                    let mut has_comma = false;
                    while self.sat(rule!(TokenKind::Comma)) {
                        self.advance();
                        has_comma = true;
                    }
                    if !has_comma {
                        break;
                    }
                }
            }
            self.expect(rule!(TokenKind::RightBox),
                    "expected a closing `]` here")?;
            Some(())
        } else if self.sat(rule!(TokenKind::LeftBrace)) {
            self.unexpected("unimplemented")
        } else {
            self.unexpected("unexpected symbol in expression")
        }
    }

}

fn directive_enabled_by_default(directive : &str) -> bool {
    matches!(directive,
            "banned-functions" | "inconsistent-indent" |
            "bad-tab-style" | "syntax-errors" | "implicit-semicolons" |
            "delphi-syntax")
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

fn display_error<T : fmt::Display>(
        span : &Span,
        lines : &[Span],
        src : &str,
        filepath : &str,
        option : &str,
        reason : T,
        visited_options : &mut HashSet<String>) {
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
    println!("error ({}) in {}:{}:{}", option, filepath, row, col);
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
    println!("\n {} |{}{} {}", indent, " ".repeat(col),
            "^".repeat(underline_length), reason);
    if !visited_options.contains(option) {
        visited_options.insert(option.to_string());
        print!(" {} = note: `//# WARN {}` is enabled", indent, option);
        if directive_enabled_by_default(option) {
            print!(" by default");
        }
        println!();
    }
}
