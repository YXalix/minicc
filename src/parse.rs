use crate::{tokenizer::{Token, TokenType}, PROGRAM};

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
    NdAssign, // 赋值语句
    NdVar, // 变量
}

#[derive(Clone)]
pub struct Node {
    pub kind: NodeType,
    pub lhs: Option<Box<Node>>,
    pub rhs: Option<Box<Node>>,
    pub val: Option<i32>,
    pub var: Option<usize>,
}

impl Node {
    pub fn new_binary(kind: NodeType, lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
        Box::new(
            Node {
                kind,
                lhs: Some(lhs),
                rhs: Some(rhs),
                val: None,
                var: None,
            }
        )
    }

    pub fn new_num(val: i32) -> Box<Node> {
        Box::new(
            Node {
                kind: NodeType::NdNum,
                lhs: None,
                rhs: None,
                val: Some(val),
                var: None,
            }
        )
    }

    pub fn new_unary(kind: NodeType, lhs: Box<Node>) -> Box<Node> {
        Box::new(
            Node {
                kind,
                lhs: Some(lhs),
                rhs: None,
                val: None,
                var: None,
            }
        )
    }

    pub fn new_varnode(var_index: usize) -> Box<Node> {
        Box::new(
            Node {
                kind: NodeType::NdVar,
                lhs: None,
                rhs: None,
                val: None,
                var: Some(var_index),
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

fn find_var(name: &'static str) -> Option<usize> {
    for (i, var) in PROGRAM.lock().unwrap().variables.iter().enumerate()  {
        if var.name == name {
            return Some(i);
        }
    }
    None
}

// program = stmt*
// stmt = exprStmt
// exprStmt = expr ";"
// expr = assign
// assign = equality ("=" assign)?
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
    // expr = assign
    assign(tokens)
}

fn assign(tokens: &Vec<Token>) -> Box<Node> {
    // assign = equality ("=" assign)?
    let mut node = equality(tokens);
    if tokens[unsafe { TOKEN_POS }].equal("=") {
        unsafe { TOKEN_POS += 1 };
        node = Node::new_binary(NodeType::NdAssign, node, assign(tokens));
    }
    node
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

// 解析括号、数字、变量
// primary = "(" expr ")" | ident｜ num
fn primary(tokens: &Vec<Token>) -> Box<Node> {
    // "(" expr ")"
    if tokens[unsafe { TOKEN_POS }].equal("(") {
        unsafe { TOKEN_POS += 1 };
        let node = expr(tokens);
        comsume(tokens, ")");
        return node;
    }

    // ident
    if tokens[unsafe { TOKEN_POS }].kind == TokenType::TkIdent {
        let name = tokens[unsafe { TOKEN_POS }].charactors;
        unsafe { TOKEN_POS += 1 };
        if let Some(var_index) = find_var(name) {
            return Node::new_varnode(var_index);
        } else {
            PROGRAM.lock().unwrap().variables.push(Variable::new(name, 0));
            let var_index = PROGRAM.lock().unwrap().variables.len() - 1;
            return Node::new_varnode(var_index);
        }
    }

    // num
    if tokens[unsafe { TOKEN_POS }].kind == TokenType::TkNum {
        let node = Node::new_num(tokens[unsafe { TOKEN_POS }].val);
        unsafe { TOKEN_POS += 1 };
        return node;
    }
    panic!("unexpected token: {:?}", tokens[unsafe { TOKEN_POS }]);
}

static mut TOKEN_POS: usize = 0;

#[derive(Clone)]
pub struct Variable {
    pub name: &'static str,
    pub offset: i32,
}

impl Variable {
    fn new(name: &'static str, offset: i32) -> Variable {
        Variable {
            name,
            offset,
        }
    }
}

pub struct Function {
    pub body: Vec<Box<Node>>,
    pub variables: Vec<Variable>,
    pub stack_size: i32,
}

impl Function {
    pub fn new() -> Function {
        Function {
            body: Vec::new(),
            variables: Vec::new(),
            stack_size: 0,
        }
    }

    pub fn push(&mut self, node: Box<Node>) {
        self.body.push(node);
    }
}

pub fn parse(tokens: &Vec<Token>){
    // program = stmt*
    while tokens[unsafe { TOKEN_POS }].kind != TokenType::TkEof {
        let node = stmt(tokens);
        PROGRAM.lock().unwrap().push(node);
    }
}