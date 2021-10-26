pub mod checker;
pub mod lexer;
pub mod token;

use std::env;

fn main() {
    let args : Vec<String> = env::args().collect();
    let path;
    if args.len() <= 1 {
        path = "";
    } else {
        path = &args[1];
    }
    if let Err(e) = checker::check_project(path) {
        println!("A problem occurred when trying to run to program:\n{}", e);
    }
}
