use std::char;

// keywords
static KEYWORDS: [&str; 7] = ["return", "if", "else", "for", "while","int", "sizeof"]; 

#[derive(PartialEq)]
#[derive(Debug)]
pub enum TokenType {
    TkIdent, // 标识符
    TkPunct, // 操作符如： + -
    TkNum(i32),   // 数字
    TkKeyword, // 关键字
    TkEof,   // 文件终止符，即文件的最后
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenType,
    pub charactors: &'static str,
}

pub struct Tokenizer {
    pub tokens: Vec<Token>,
    pub now_index: usize,
}

impl Token {
    pub fn new(kind: TokenType, charactors: &'static str) -> Token {
        Token {
            kind,
            charactors,
        }
    }

    fn is_keyword(&self) -> bool {
        if KEYWORDS.contains(&self.charactors) {
            return true;
        } else {
            return false;
        }
    }

    pub fn equal(&self, charactors: &'static str) -> bool {
        if self.charactors == charactors {
            true
        } else {
            false
        }
    }
}

impl Tokenizer {
    pub fn new() -> Tokenizer {
        Tokenizer {
            tokens: Vec::new(),
            now_index: 0,
        }
    }

    pub fn cur_token(&mut self) -> &Token {
        &self.tokens[self.now_index]
    }

    pub fn consume(&mut self, charactors: &'static str) -> bool {
        if self.cur_token().charactors == charactors {
            self.now_index += 1;
            return true;
        } else {
            return false;
        }
    }

    pub fn skip(&mut self) {
        self.now_index += 1;
    }

    pub fn tokenize(&mut self, input: &'static str) {
        let mut p = input;
        while p.len() > 0 {
            let c = p.chars().nth(0).unwrap();
            if c.is_whitespace() {
                p = &p[1..];
                continue;
            }
            if c.is_digit(10) {
                if let Some((number, endpoint)) = extract_number_and_endpoint(p) {
                    self.tokens.push(Token::new(TokenType::TkNum(number), &p[..endpoint]));
                    p = &p[endpoint..];
                    continue;
                }
            }
            if c.is_ascii_alphabetic() || c == '_' {
                let mut now_index = 1;
                for c in p[1..].chars() {
                    if c.is_ascii_alphanumeric() || c == '_' || c.is_ascii_digit() {
                        now_index += 1;
                    } else {
                        break;
                    }
                }
                self.tokens.push(Token::new(TokenType::TkIdent, &p[..now_index]));
                p = &p[now_index..];
                continue;
            }
            if let Some(len) = read_punct(p) {
                self.tokens.push(Token::new(TokenType::TkPunct, &p[..len]));
                p = &p[len..];
                continue;
            }
            panic!("无法识别的字符: {}", c);
        }
        self.tokens.push(Token::new(TokenType::TkEof, ""));

        // 将标识符转换为关键字
        self.convert_keyword();
    }

    fn convert_keyword(&mut self) {
        for token in &mut self.tokens {
            if token.is_keyword() {
                token.kind = TokenType::TkKeyword;
            }
        }
    }
}

fn extract_number_and_endpoint(input: &str) -> Option<(i32, usize)> {
    // 查找第一个非数字字符的索引
    let mut now_index = 0;
    for c in input.chars() {
        if !c.is_digit(10) {
            break;
        }
        now_index += 1;
    }
    if now_index == 0 {
        return None;
    } else {
        let number = input[..now_index].parse::<i32>().unwrap();
        return Some((number, now_index));
    }
}

fn read_punct(input: &str) -> Option<usize> {
    if input.len() == 0 {
        return None;
    }
    if input.starts_with("==") || input.starts_with("!=") || input.starts_with("<=") || input.starts_with(">=") {
        return Some(2);
    }
    let c = input.chars().nth(0).unwrap();
    return if c.is_ascii_punctuation() {
        Some(1)
    } else {
        None
    };
}