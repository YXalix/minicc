mod ltex;
mod parse;
mod ast;

pub fn ltex(input: &'static str) {
    let mut tokenizer = ltex::Tokenizer::new();
    tokenizer.tokenize(input);
    println!("{:?}", tokenizer.tokens);
}