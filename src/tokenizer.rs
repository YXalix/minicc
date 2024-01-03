
// 词法分析器
use crate::util::{extract_number_and_endpoint, read_punct};



#[derive(PartialEq)]
#[derive(Debug)]
pub enum TokenType {
    TkIdent, // 标识符
    TkPunct, // 操作符如： + -
    TkNum,   // 数字
    TkKeyword, // 关键字
    TkEof,   // 文件终止符，即文件的最后
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenType,
    pub charactors: &'static str,
    pub val: i32,
}

pub fn is_keyword(token: &Token) -> bool {
    static KEYWORDS: [&'static str; 7] = ["return", "if", "else", "for", "while","int", "sizeof"];
    if KEYWORDS.contains(&token.charactors) {
        return true;
    } else {
        return false;
    }
}

pub fn convert_keyword(token: &mut Vec<Token>) {
    for i in 0..token.len() {
        if is_keyword(&token[i]) {
            token[i].kind = TokenType::TkKeyword;
        }
    }
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

            // 解析标记符
            // [a-zA-Z_][a-zA-Z0-9_]*
            if c.is_ascii_alphabetic() || c == '_' {
                let mut now_index = 1;
                for c in p[1..].chars() {
                    if c.is_ascii_alphanumeric() || c == '_' || c.is_ascii_digit() {
                        now_index += 1;
                    } else {
                        break;
                    }
                }
                tokens.push(Token::new(TokenType::TkIdent, &p[..now_index], 0));
                p = &p[now_index..];
                continue;
            }
            if let Some(len) = read_punct(p) {
                tokens.push(Token::new(TokenType::TkPunct, &p[..len], 0));
                p = &p[len..];
                continue;
            }
            panic!("invalid character: {}", c);
        }
        tokens.push(Token::new(TokenType::TkEof, "", 0));
        convert_keyword(&mut tokens);
        tokens
    }

    pub fn equal(&self, charactors: &str) -> bool {
        if self.charactors != charactors {
            return false;
        } else {
            return true;
        }
    }
}