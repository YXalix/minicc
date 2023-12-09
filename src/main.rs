extern crate lazy_static;
mod tokenizer;
mod codegen;
mod util;
mod parse;

use std::env;

use crate::
    {tokenizer::Token,
    codegen::codegen};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("{} invalid number of arguments", args[0]);
        return;
    }
    let p = args[1].clone().leak();
    let tokens = Token::tokenize(p);
    let programs = parse::parse(&tokens);
    codegen(&programs);
}