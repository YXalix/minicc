use parse::Parser;

mod ltex;
mod parse;
mod ast;
mod ir;
mod codegen;

pub fn ltex(input: &'static str) {
    let mut tokenizer = ltex::Tokenizer::new();
    tokenizer.tokenize(input);
    println!("{:?}", tokenizer.tokens);
}

// 我的实现中包含一个显式的IR，编译过程先将AST翻译为IR，再将IR翻译成汇编
// 简单起见，最终也只会有这两个转化过程，在后续step中会引入对程序的合法性的检查，都是在"将AST翻译为IR"这一步进行
// 如果想查看中间结果，可以自行在这个函数中合适的位置加上输出
pub fn run(input: &'static str) {
    let mut tokenizer = ltex::Tokenizer::new();
    tokenizer.tokenize(input);
    let p = Parser::parse(&mut tokenizer);
    let ir = ir::ast2ir(&p);
    codegen::write_asm(&ir);
}