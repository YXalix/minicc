use crate::tokenizer::{Token, TokenType};


// ND_EQ,  // ==
// ND_NE,  // !=
// ND_LT,  // <
// ND_LE,  // <=
#[derive(PartialEq)]
#[derive(Debug, Clone)]
pub enum NodeType {
    NdAdd,  // +
    NdSub,  // -
    NdMul,  // *
    NdDiv,  // /
    NdNum,  // 整形
    NdNeg,  // 负数
    NdEq,  // ==
    NdNe,  // !=
    NdLt,  // <
    NdLe,  // <=
    NdExprStmt, // 表达式语句
}

#[derive(Clone)]
pub struct Node {
    pub kind: NodeType,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: i32,
}

impl Node {

    pub fn new_binary(kind: NodeType, lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
        Box::new(
            Node {
                kind,
                lhs: Some(lhs),
                rhs: Some(rhs),
                val: 0,
            }
        )
    }

    pub fn new_num(val: i32) -> Box<Node> {
        Box::new(
            Node {
                kind: NodeType::NdNum,
                lhs: None,
                rhs: None,
                val,
            }
        )
    }

    pub fn new_unary(kind: NodeType, lhs: Box<Node>) -> Box<Node> {
        Box::new(
            Node {
                kind,
                lhs: Some(lhs),
                rhs: None,
                val: 0,
            }
        )
    }
}

fn comsume(tokens: &Vec<Token>, charactors: &str) {
    if tokens[unsafe { TOKEN_POS }].equal(charactors) {
        unsafe { TOKEN_POS += 1 };
    } else {
        panic!("expected token: {}, but got: {:?}", charactors, tokens[unsafe { TOKEN_POS }]);
    }

}

// program = stmt*
// stmt = exprStmt
// exprStmt = expr ";"
// expr = equality
// equality = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
// add = mul ("+" mul | "-" mul)*
// mul = unary ("*" unary | "/" unary)*
// unary = ("+" | "-") unary | primary
// primary = "(" expr ")" | num


fn stmt(tokens: &Vec<Token>) -> Box<Node> {
    // stmt = exprStmt
    expr_stmt(tokens)
}

fn expr_stmt(tokens: &Vec<Token>) -> Box<Node> {
    // exprStmt = expr ";"
    let node = Node::new_unary(NodeType::NdExprStmt, expr(tokens));
    comsume(tokens, ";");
    node
}

fn expr(tokens: &Vec<Token>) -> Box<Node> {
    // expr = equality
    equality(tokens)
}

fn equality(tokens: &Vec<Token>) -> Box<Node> {
    // equality = relational ("==" relational | "!=" relational)*
    let mut node = relational(tokens);
    loop {
        if tokens[unsafe { TOKEN_POS }].equal("==") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdEq, node, relational(tokens));
            continue;
        }
        if tokens[unsafe { TOKEN_POS }].equal("!=") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdNe, node, relational(tokens));
            continue;
        }
        return node;
    }
}

fn relational(tokens: &Vec<Token>) -> Box<Node> {
    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    let mut node = add(tokens);
    loop {
        if tokens[unsafe { TOKEN_POS }].equal("<") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdLt, node, add(tokens));
            continue;
        }
        if tokens[unsafe { TOKEN_POS }].equal("<=") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdLe, node, add(tokens));
            continue;
        }
        if tokens[unsafe { TOKEN_POS }].equal(">") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdLt, add(tokens), node);
            continue;
        }
        if tokens[unsafe { TOKEN_POS }].equal(">=") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdLe, add(tokens), node);
            continue;
        }
        return node;
    }
}

fn add(tokens: &Vec<Token>) -> Box<Node> {
    // add = mul ("+" mul | "-" mul)*
    let mut node = mul(tokens);
    loop {
        if tokens[unsafe { TOKEN_POS }].equal("+") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdAdd, node, mul(tokens));
            continue;
        }
        if tokens[unsafe { TOKEN_POS }].equal("-") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdSub, node, mul(tokens));
            continue;
        }
        return node;
    }
}

// 解析乘除
// mul = unary ("*" unary | "/" unary)*
fn mul(tokens: &Vec<Token>) -> Box<Node> {
    let mut node = unary(tokens);
    loop {
        if tokens[unsafe { TOKEN_POS }].equal("*") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdMul, node, unary(tokens));
            continue;
        }
        if tokens[unsafe { TOKEN_POS }].equal("/") {
            unsafe { TOKEN_POS += 1 };
            node = Node::new_binary(NodeType::NdDiv, node, unary(tokens));
            continue;
        }
        return node;
    }
}

fn unary(tokens: &Vec<Token>) -> Box<Node> {
    // ("+" | "-") unary | primary
    if tokens[unsafe { TOKEN_POS }].equal("+") {
        unsafe { TOKEN_POS += 1 };
        return unary(tokens);
    }
    if tokens[unsafe { TOKEN_POS }].equal("-") {
        unsafe { TOKEN_POS += 1 };
        return Node::new_unary(NodeType::NdNeg, unary(tokens));
    }
    primary(tokens)
}

// 解析括号、数字
// primary = "(" expr ")" | num
fn primary(tokens: &Vec<Token>) -> Box<Node> {
    // "(" expr ")"
    if tokens[unsafe { TOKEN_POS }].equal("(") {
        unsafe { TOKEN_POS += 1 };
        let node = expr(tokens);
        comsume(tokens, ")");
        return node;
    }

    if tokens[unsafe { TOKEN_POS }].kind == TokenType::TkNum {
        let node = Node::new_num(tokens[unsafe { TOKEN_POS }].val);
        unsafe { TOKEN_POS += 1 };
        return node;
    }
    panic!("unexpected token: {:?}", tokens[unsafe { TOKEN_POS }]);
}

static mut TOKEN_POS: usize = 0;

pub fn parse(tokens: &Vec<Token>) -> Vec<Box<Node>> {
    let mut program = Vec::new();
    // program = stmt*
    while tokens[unsafe { TOKEN_POS }].kind != TokenType::TkEof {
        program.push(stmt(tokens));
    }
    program
}