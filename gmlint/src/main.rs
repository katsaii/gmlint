pub mod checker;
pub mod lexer;
pub mod token;

fn main() {
    let tokens : Vec<_> = lexer::Lexer::new("a bc /*# hi hi */ 123").into();
    println!("{:?}", tokens);
}
