use crate::{ast::Prog, ltex::{Tokenizer, TokenType}};

// GLOBALS = functionDefinition*
// functionDefinition = declspec declarator "{" compoundStmt*
// declspec = "int"
// declarator = "*"* ident typeSuffix
// typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
// funcParams = (param ("," param)*)? ")"
// param = declspec declarator

// compoundStmt = (declaration | stmt)* "}"
// declaration =
//    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// stmt = "return" expr ";"
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | "for" "(" exprStmt expr? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "{" compoundStmt
//        | exprStmt
// exprStmt = expr? ";"
// expr = assign
// assign = equality ("=" assign)?
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-" | "*" | "&") unary | postfix
// postfix = primary ("[" expr "]")*
// primary = "(" expr ")" | "sizeof" unary | ident funcArgs? | num


pub struct Parser {}

impl Parser {
    fn parse(p: &mut Tokenizer) -> Prog {
        let mut prog = Prog {
            funcs: Vec::new(),
            globs: Vec::new()
        };
        prog
    }


}