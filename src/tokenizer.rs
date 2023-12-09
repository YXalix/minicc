
// 词法分析器
use crate::util::{extract_number_and_endpoint, read_punct};



#[derive(PartialEq)]
#[derive(Debug)]
pub enum TokenType {
    TkPunct, // 操作符如： + -
    TkNum,   // 数字
    TkEof,   // 文件终止符，即文件的最后
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenType,
    pub charactors: &'static str,
    pub val: i32,
}

impl Token {
    fn new(kind: TokenType, charactors: &'static str, val: i32) -> Token {
        Token {
            kind,
            charactors,
            val,
        }
    }

    pub fn tokenize(input: &'static str) -> Vec<Token>{
        let mut tokens = Vec::new();
        let mut p = input;
        while p.len() > 0 {
            let c = p.chars().nth(0).unwrap();
            if c.is_whitespace() {
                p = &p[1..];
                continue;
            }
            if c.is_digit(10) {
                if let Some((number, endpoint)) = extract_number_and_endpoint(p) {
                    tokens.push(Token::new(TokenType::TkNum, &p[..endpoint], number));
                    p = &p[endpoint..];
                    continue;
                } else {
                    panic!("invalid number: {}", p);
                }
            }
            if let Some(len) = read_punct(p) {
                tokens.push(Token::new(TokenType::TkPunct, &p[..len], 0));
                p = &p[len..];
                continue;
            }
            panic!("invalid character: {}", c);
        }
        tokens.push(Token::new(TokenType::TkEof, "", 0));
        tokens
    }

    pub fn equal(&self, charactors: &str) -> bool{
        if self.charactors != charactors {
            return false;
        } else {
            return true;
        }
    }
}