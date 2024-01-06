extern crate lazy_static;

mod tokenizer;
mod codegen;
mod util;
mod parse;
mod types;

use std::env;

// use crate::
//     {tokenizer::Token,
//     codegen::codegen};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("{} invalid number of arguments", args[0]);
        return;
    }
    let p: &mut str = args[1].clone().leak();
    // let tokens = Token::tokenize(p);
    // let mut program = parse::parse(&tokens);
    // codegen(&mut program);
    decaf::run(p);
}