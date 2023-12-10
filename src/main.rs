extern crate lazy_static;
mod tokenizer;
mod codegen;
mod util;
mod parse;
use lazy_static::lazy_static;

use std::{env, sync::Mutex};

use crate::
    {tokenizer::Token,
    codegen::codegen, parse::Function};

lazy_static! {
    static ref PROGRAM: Mutex<Function> = Mutex::new(Function::new());
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("{} invalid number of arguments", args[0]);
        return;
    }
    let p = args[1].clone().leak();
    let tokens = Token::tokenize(p);
    parse::parse(&tokens);
    codegen();
}