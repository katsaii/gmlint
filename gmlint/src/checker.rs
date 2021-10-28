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
        Self { filepath, src, lines, lexer, peeked, peeked_span,
                banned_functions, directive_warn, indent_style,
                directives, error_count, visited_directives }
    }

    /// Returns the substring of the current span.
    pub fn substring(&self) -> &'a str {
        self.peeked_span.render(self.src)
    }

    /// Displays an error.
    pub fn error<T : fmt::Display>(&mut self, option : &str, reason : T) {
        let enabled = matches!(self.directives.get(option), None | Some(true));
        if enabled == directive_enabled_by_default(option) {
            if self.error_count != 0 {
                println!();
            }
            self.error_count += 1;
            display_error(&self.peeked_span, &self.lines, self.src,
                    &self.filepath, option, reason,
                    &mut self.visited_directives);
        }
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
                                "tab is used here when it shouldn't be");
                    } else if self.indent_style == IndentStyle::Unknown {
                        self.indent_style = IndentStyle::Tab;
                    } else if self.indent_style == IndentStyle::Space {
                        self.error("inconsistent-indentation",
                                "expected a space, but found a tab");
                    }
                },
                TokenKind::Space => {
                    if !newline {
                        // ignore
                    } else if self.indent_style == IndentStyle::Unknown {
                        self.indent_style = IndentStyle::Space;
                    } else if self.indent_style == IndentStyle::Tab {
                        self.error("inconsistent-indentation",
                                "expected a tab, but found a space");
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
    pub fn check_program(&mut self) -> Option<()> {
        loop {
            let token = self.generate_token()?;
            match token {
                TokenKind::Identifier => {
                    let substring = self.substring();
                    for (pattern, replacement) in &self.banned_functions {
                        if pattern.matches(substring) {
                            let message = if let Some(name) = replacement {
                                format!(", instead use `{}`", name)
                            } else {
                                String::new()
                            };
                            self.error("banned-functions",
                                    format!("accessing this variable is \
                                            prohibited{}", message));
                            break;
                        }
                    }
                },
                _ => (),
            }
        }
    }
}

fn directive_enabled_by_default(directive : &str) -> bool {
    matches!(directive,
            "banned-functions" | "inconsistent-indentation" |
            "bad-tab-style")
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
        println!(" {} = note: `//# WARN {}` is enabled", indent, option);
    }
}
