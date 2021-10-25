pub mod checker;
pub mod lexer;
pub mod token;

use std::env;

fn main() {
    let args : Vec<String> = env::args().collect();
    checker::check_file(&args[1], &[]);
}
