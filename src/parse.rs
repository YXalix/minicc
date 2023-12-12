use crate::{tokenizer::{Token, TokenType}, PROGRAM, types::{Type, TypeKind, pointer_to}};


#[derive(PartialEq)]
#[derive(Debug, Clone)]

// AST的节点种类
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
    NdReturn, // return
    NdBloc, // {}
    NdIf, // if
    NdFor, // "for" 或 "while"，循环
    NdAddr, // &
    NdDeref, // *
}

#[derive(Clone)]
pub struct Node {
    pub kind: NodeType,

    pub ty: Option<Box<Type>>,

    pub lhs: Option<Box<Node>>, 
    pub rhs: Option<Box<Node>>,
    
    // "if"语句 或者 "for"语句
    pub cond: Option<Box<Node>>,
    pub then: Option<Box<Node>>,
    pub els: Option<Box<Node>>,
    pub init: Option<Box<Node>>,
    pub inc: Option<Box<Node>>, // 递增语句

    pub body: Vec<Box<Node>>,
    pub val: Option<i32>,
    pub var: Option<usize>,
}

impl Node {
    pub fn new_node(kind: NodeType) -> Box<Node> {
        Box::new(
            Node {
                kind,
                ty: None,
                lhs: None,
                rhs: None,
                cond: None,
                then: None,
                els: None,
                init: None,
                inc: None,
                body: Vec::new(),
                val: None,
                var: None,
            }
        )
    }

    pub fn new_binary(kind: NodeType, lhs: Box<Node>, rhs: Box<Node>) -> Box<Node> {
        let mut node = Node::new_node(kind);
        node.lhs = Some(lhs);
        node.rhs = Some(rhs);
        node
    }

    pub fn new_num(val: i32) -> Box<Node> {
        let mut node = Node::new_node(NodeType::NdNum);
        node.val = Some(val);
        node
    }

    pub fn new_unary(kind: NodeType, lhs: Box<Node>) -> Box<Node> {
        let mut node = Node::new_node(kind);
        node.lhs = Some(lhs);
        node
    }

    pub fn new_varnode(var_index: usize) -> Box<Node> {
        let mut node = Node::new_node(NodeType::NdVar);
        node.var = Some(var_index);
        node
    }

    fn get_type(&self) -> Box<Type> {
        if let Some(ty) = self.ty.as_ref() {
            ty.clone()
        } else {
            panic!("node type is None");
        }
    }

    fn is_ptr(&self) -> bool {
        if let Some(ty) = self.ty.as_ref() {
            ty.is_ptr()
        } else {
            false
        }
    }

    fn is_integer(&self) -> bool {
        if let Some(ty) = self.ty.as_ref() {
            ty.is_integer()
        } else {
            false
        }
    }

    /// 为节点添加类型
    fn add_type(&mut self){
        if self.ty.is_some() {
            return;
        }

        // 递归访问所有节点以增加类型
        if let Some(lhs) = self.lhs.as_mut() {
            lhs.add_type();
        }
        if let Some(rhs) = self.rhs.as_mut() {
            rhs.add_type();
        }
        if let Some(cond) = self.cond.as_mut() {
            cond.add_type();
        }
        if let Some(then) = self.then.as_mut() {
            then.add_type();
        }
        if let Some(els) = self.els.as_mut() {
            els.add_type();
        }
        if let Some(init) = self.init.as_mut() {
            init.add_type();
        }
        if let Some(inc) = self.inc.as_mut() {
            inc.add_type();
        }

        // 访问链表内的所有节点以增加类型
        (self.body).iter_mut().for_each(|temp| {
            temp.add_type();
        });

        match self.kind {
            // 将节点类型设为 节点左部的类型
            NodeType::NdAdd | NodeType::NdSub | NodeType::NdMul | NodeType::NdDiv | NodeType::NdNeg | NodeType::NdAssign => {
                self.ty = Some(self.lhs.as_ref().unwrap().get_type());
            },
            // 将节点类型设为 int类型
            NodeType::NdEq | NodeType::NdNe | NodeType::NdLt | NodeType::NdLe | NodeType::NdNum => {
                self.ty = Some(
                    Type::new(TypeKind::TyInt, Box::new(Type {
                        kind: TypeKind::TyInt,
                        ptr_to: None,
                        tok: None,
                    }))
                );
            },
            // 将节点类型设为 变量的类型
            NodeType::NdVar => {
                let var_index = self.var.unwrap();
                self.ty = Some(PROGRAM.read().unwrap().variables[var_index].ty.as_ref().unwrap().clone());
            },
            // 将节点类型设为 指针，并指向左部的类型
            NodeType::NdAddr => {
                self.ty = Some(pointer_to(self.lhs.as_ref().unwrap().get_type()));
            },
            // 节点类型：如果解引用指向的是指针，则为指针指向的类型；否则报错
            NodeType::NdDeref => {
                if ! self.lhs.as_ref().unwrap().is_ptr() {
                    panic!("invalid pointer dereference");
                }
                self.ty = Some(self.lhs.as_ref().unwrap().get_type().ptr_to.as_ref().unwrap().clone());
            },
            _ => {},
        }

    }
}


fn comsume(tokens: &Vec<Token>, charactors: &str) {
    if get_cur_token(tokens).equal(charactors) {
        skip();
    } else {
        panic!("expected token: {}, but got: {:?}", charactors, get_cur_token(tokens));
    }
}

fn find_var(name: &'static str) -> Option<usize> {
    for (i, var) in PROGRAM.read().unwrap().variables.iter().enumerate()  {
        if var.name == name {
            return Some(i);
        }
    }
    None
}

// 新增一个变量
fn new_lvar(tokens: &Vec<Token>, tok: Option<usize>, ty: Box<Type>) -> usize {
    let name = tokens[tok.unwrap()].charactors;
    if let Some(var_index) = find_var(name) {
        return var_index;
    }
    PROGRAM.write().unwrap().variables.push(Variable::new(name, Some(ty)));
    PROGRAM.read().unwrap().variables.len() - 1
}

// program = "{" compoundStmt
// compoundStmt = (declaration | stmt)* "}"
// declaration =
//    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// declspec = "int"
// declarator = "*"* ident
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
// unary = ("+" | "-" | "*" | "&") unary | primary
// primary = "(" expr ")" | ident | num


// declspec = "int"
// declarator specifier
fn declspec(tokens: &Vec<Token>) -> Box<Type> {
    comsume(tokens, "int");
    Box::new(Type {
        kind: TypeKind::TyInt,
        ptr_to: None,
        tok: None,
    })
}

// declarator = "*"* ident
fn declarator(tokens: &Vec<Token>, mut ty: Box<Type>) -> Box<Type> {
    // "*"*
    // 构建所有的（多重）指针
    while get_cur_token(tokens).equal("*") {
        skip();
        ty = pointer_to(ty);
    }

    if get_cur_token(tokens).kind != TokenType::TkIdent {
        panic!("expected a variable name");
    }

    // ident
    // 变量名
    ty.tok = Some(unsafe { TOKEN_POS });
    skip();
    ty
}

// declaration =
//    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
fn declaration(tokens: &Vec<Token>) -> Box<Node> {
    let basety = declspec(tokens);
    let mut node = Node::new_node(NodeType::NdBloc);
    // 对变量声明次数计数
    let mut i = 0;

    // (declarator ("=" expr)? ("," declarator ("=" expr)?)*)?
    while get_cur_token(tokens).equal(";") == false {
        // 第1个变量不必匹配 ","
        if i > 0 {
            comsume(tokens, ",");
        }
        i += 1;
        // declarator
        // 声明获取到变量类型，包括变量名
        let ty = declarator(tokens, basety.clone());
        let var = new_lvar(tokens, ty.tok, ty.clone());

        // 如果不存在"="则为变量声明，不需要生成节点，已经存储在Locals中了
        if get_cur_token(tokens).equal("=") == false {
            continue;
        }

        // 解析“=”后面的Token
        let lhs = Node::new_varnode(var);
        // 解析递归赋值语句
        skip();
        let rhs = assign(tokens);
        let tmp = Node::new_binary(NodeType::NdAssign, lhs, rhs);
        let tmp = Node::new_unary(NodeType::NdExprStmt, tmp);

        node.body.push(tmp);
    }
    comsume(tokens, ";");
    node
}


fn stmt(tokens: &Vec<Token>) -> Box<Node> {
    // stmt = "return" expr ";"
    //        | "if" "(" expr ")" stmt ("else" stmt)?
    //        | "for" "(" exprStmt expr? ";" expr? ")" stmt
    //        | "while" "(" expr ")" stmt
    //        | "{" compoundStmt
    //        | exprStmt

    // "return" expr ";"
    if get_cur_token(tokens).equal("return") {
        skip();
        let node = Node::new_unary(NodeType::NdReturn, expr(tokens));
        comsume(tokens, ";");
        return node;
    }

    // "{" compoundStmt
    if get_cur_token(tokens).equal("{") {
        skip();
        return compound_stmt(tokens);
    }

    // "if" "(" expr ")" stmt ("else" stmt)?
    if get_cur_token(tokens).equal("if") {
        skip();
        comsume(tokens, "(");
        let cond = expr(tokens);
        comsume(tokens, ")");
        let then = stmt(tokens);
        let mut node = Node::new_node(NodeType::NdIf);
        node.cond = Some(cond);
        node.then = Some(then);
        if get_cur_token(tokens).equal("else") {
            skip();
            node.els = Some(stmt(tokens));
        }
        return node;
    }

    // "for" "(" exprStmt expr? ";" expr? ")" stmt
    if get_cur_token(tokens).equal("for") {
        skip();
        comsume(tokens, "(");
        let mut node = Node::new_node(NodeType::NdFor);
        node.init = Some(expr_stmt(tokens));
        if get_cur_token(tokens).equal(";") == false {
            node.cond = Some(expr(tokens));
        }
        comsume(tokens, ";");
        if get_cur_token(tokens).equal(")") == false {
            node.inc = Some(expr(tokens));
        }
        comsume(tokens, ")");
        node.then = Some(stmt(tokens));
        return node;
    }

    // "while" "(" expr ")" stmt
    if get_cur_token(tokens).equal("while") {
        skip();
        comsume(tokens, "(");
        let mut node = Node::new_node(NodeType::NdFor);
        node.cond = Some(expr(tokens));
        comsume(tokens, ")");
        node.then = Some(stmt(tokens));
        return node;
    }

    // exprStmt
    expr_stmt(tokens)
}

// 解析复合语句
// compoundStmt = (declaration | stmt)* "}"
fn compound_stmt(tokens: &Vec<Token>) -> Box<Node> {
    let mut node = Node::new_node(NodeType::NdBloc);
    while get_cur_token(tokens).equal("}") == false {
        // declaration
        if get_cur_token(tokens).equal("int") {
            node.body.push(declaration(tokens));
        } else {
            node.body.push(stmt(tokens));
        }
        // 构造完AST后，为节点添加类型信息
        node.add_type();
    }
    comsume(tokens, "}");
    node
}

fn expr_stmt(tokens: &Vec<Token>) -> Box<Node> {
    // exprStmt = expr? ";"
    if get_cur_token(tokens).equal(";") {
        skip();
        return Node::new_node(NodeType::NdBloc);
    }

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
    if get_cur_token(tokens).equal("=") {
        skip();
        node = Node::new_binary(NodeType::NdAssign, node, assign(tokens));
    }
    node
}

fn equality(tokens: &Vec<Token>) -> Box<Node> {
    // equality = relational ("==" relational | "!=" relational)*
    let mut node = relational(tokens);
    loop {
        if get_cur_token(tokens).equal("==") {
            skip();
            node = Node::new_binary(NodeType::NdEq, node, relational(tokens));
            continue;
        }
        if get_cur_token(tokens).equal("!=") {
            skip();
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
        if get_cur_token(tokens).equal("<") {
            skip();
            node = Node::new_binary(NodeType::NdLt, node, add(tokens));
            continue;
        }
        if get_cur_token(tokens).equal("<=") {
            skip();
            node = Node::new_binary(NodeType::NdLe, node, add(tokens));
            continue;
        }
        if get_cur_token(tokens).equal(">") {
            skip();
            node = Node::new_binary(NodeType::NdLt, add(tokens), node);
            continue;
        }
        if get_cur_token(tokens).equal(">=") {
            skip();
            node = Node::new_binary(NodeType::NdLe, add(tokens), node);
            continue;
        }
        return node;
    }
}


// 解析各种加法
fn new_add(mut lhs: Box<Node>, mut rhs: Box<Node>) -> Box<Node> {
    // 为左右部添加类型
    lhs.add_type();
    rhs.add_type();

    // num + num
    if lhs.is_integer() && rhs.is_integer() {
        return Node::new_binary(NodeType::NdAdd, lhs, rhs);
    }

    // 不能解析 ptr + ptr
    if lhs.is_ptr() && rhs.is_ptr() {
        panic!("invalid operands ptr + ptr");
    }

    // 将 num + ptr 转换为 ptr + num
    if lhs.is_integer() && rhs.is_ptr() {
        // num + ptr
        let temp = Node::new_binary(NodeType::NdMul, lhs, Node::new_num(8));
        return Node::new_binary(NodeType::NdAdd, temp, rhs);
    } else {
        // ptr + num
        let temp = Node::new_binary(NodeType::NdMul, rhs, Node::new_num(8));
        return Node::new_binary(NodeType::NdAdd, lhs, temp);
    }
}

// 解析各种减法
fn new_sub(mut lhs: Box<Node>, mut rhs: Box<Node>) -> Box<Node> {
    // 为左右部添加类型
    lhs.add_type();
    rhs.add_type();
    // num - num
    if lhs.is_integer() && rhs.is_integer() {
        return Node::new_binary(NodeType::NdSub, lhs, rhs);
    }

    // ptr - num
    if lhs.is_ptr() && rhs.is_integer() {
        let temp = Node::new_binary(NodeType::NdMul, rhs.clone(), Node::new_num(8));
        let mut nd = Node::new_binary(NodeType::NdSub, lhs.clone(), temp);
        // 节点类型为指针
        nd.ty = Some(lhs.get_type());
        return nd;
    }

    // ptr - ptr
    if lhs.is_ptr() && rhs.is_ptr() {
        let temp = Node::new_binary(NodeType::NdSub, lhs, rhs);
        return Node::new_binary(NodeType::NdDiv, temp, Node::new_num(8));
    }
    panic!("invalid operands for num - ptr");
}


fn add(tokens: &Vec<Token>) -> Box<Node> {
    // add = mul ("+" mul | "-" mul)*
    let mut node = mul(tokens);
    loop {
        if get_cur_token(tokens).equal("+") {
            skip();
            node = new_add(node, mul(tokens));
            continue;
        }
        if get_cur_token(tokens).equal("-") {
            skip();
            node = new_sub(node, mul(tokens));
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
        if get_cur_token(tokens).equal("*") {
            skip();
            node = Node::new_binary(NodeType::NdMul, node, unary(tokens));
            continue;
        }
        if get_cur_token(tokens).equal("/") {
            skip();
            node = Node::new_binary(NodeType::NdDiv, node, unary(tokens));
            continue;
        }
        return node;
    }
}

// 解析一元运算
// unary = ("+" | "-" | "*" | "&") unary | primary
fn unary(tokens: &Vec<Token>) -> Box<Node> {
    // "+" unary
    if get_cur_token(tokens).equal("+") {
        skip();
        return unary(tokens);
    }

    // "-" unary
    if get_cur_token(tokens).equal("-") {
        skip();
        return Node::new_unary(NodeType::NdNeg, unary(tokens));
    }

    // "&" unary
    if get_cur_token(tokens).equal("&") {
        skip();
        return Node::new_unary(NodeType::NdAddr, unary(tokens));
    }

    // "*" unary
    if get_cur_token(tokens).equal("*") {
        skip();
        return Node::new_unary(NodeType::NdDeref, unary(tokens));
    }

    // primary
    primary(tokens)
}

// 解析括号、数字、变量
// primary = "(" expr ")" | ident｜ num
fn primary(tokens: &Vec<Token>) -> Box<Node> {
    // "(" expr ")"
    if get_cur_token(tokens).equal("(") {
        skip();
        let node = expr(tokens);
        comsume(tokens, ")");
        return node;
    }

    // ident
    if get_cur_token(tokens).kind == TokenType::TkIdent {
        let name = get_cur_token(tokens).charactors;
        skip();
        if let Some(var_index) = find_var(name) {
            return Node::new_varnode(var_index);
        } else {
            panic!("undefined variable")
        }
    }

    // num
    if get_cur_token(tokens).kind == TokenType::TkNum {
        let node = Node::new_num(get_cur_token(tokens).val);
        skip();
        return node;
    }
    panic!("unexpected token: {:?}", get_cur_token(tokens));
}

static mut TOKEN_POS: usize = 0;

fn get_cur_token(tokens: &Vec<Token>) -> &Token {
    &tokens[unsafe { TOKEN_POS }]
}

fn skip() {
    unsafe { TOKEN_POS += 1 };
}

#[derive(Clone)]
pub struct Variable {
    pub name: &'static str,
    pub offset: i32,
    pub ty: Option<Box<Type>>,
}

impl Variable {
    fn new(name: &'static str,ty: Option<Box<Type>>) -> Variable {
        Variable {
            name: name,
            offset: 0,
            ty: ty,
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
    // program = "{" compoundStmt

    // "{"
    comsume(tokens, "{");
    let node = compound_stmt(tokens);
    PROGRAM.write().unwrap().push(node);
}