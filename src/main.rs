use std::env;

mod ast;
fn main() {
    env::set_var("RUST_BACKTRACE", "1");
    let contents = std::fs::read_to_string("examples/1.custom")
        .expect("Something went wrong reading the file");
    let tokens = ast::lexer::Lexer::lex_string(contents.as_str()).unwrap();
    println!("{:#?}", tokens);
    let mut parser = ast::parser::Parser::new(&tokens);
    let parsed = parser.parse().unwrap();
    println!("{:#?}", parsed);
}
