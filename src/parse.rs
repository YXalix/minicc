use crate::{tokenizer::{Token, TokenType}, types::{Type, pointer_to, TypeKind}};


// 语法解析入口函数
// program = (functionDefinition | globalVariable)*
pub fn parse(tokens: &Vec<Token>) -> &mut Vec<Obj> {
    while get_cur_token(tokens).kind != TokenType::TkEof {
        // declspec
        let ty = declspec(tokens);
        // 函数
        if is_function(tokens) {
            function(tokens, ty);
            continue;
        }
        // 全局变量
        global_variable(tokens, ty);
    }
    unsafe { &mut GLOBALS }
}

static mut TOKEN_POS: usize = 0;

fn get_cur_token(tokens: &Vec<Token>) -> &Token {
    &tokens[unsafe { TOKEN_POS }]
}

fn skip() {
    unsafe { TOKEN_POS += 1 };
}

fn get_num(tokens: &Vec<Token>) -> i32 {
    let tok = get_cur_token(tokens);
    if tok.kind != TokenType::TkNum {
        panic!("not a number token");
    }
    tok.val
}


static mut GLOBALS:Vec<Obj> = Vec::new();

#[derive(Clone)]
pub struct Obj{
    pub name: &'static str,         // 变量名
    pub ty: Option<Box<Type>>,      // 变量类型
    pub is_local: bool,             // 是否为局部变量

    // 局部变量
    pub offset: i32,                // 偏移量

    // 函数 或 全局变量
    pub is_function: bool,          // 是否为函数
    // 函数
    pub params: Vec<Obj>,           // 参数

    pub body: Vec<Box<Node>>,       // 函数体
    pub locals: Vec<Obj>,           // 局部变量
    pub stack_size: i32,            // 栈大小
}

impl Obj {
    pub fn new() -> Obj {
        Obj {
            name: "",
            ty: None,
            is_local: false,
            offset: 0,
            is_function: false,
            params: Vec::new(),
            body: Vec::new(),
            locals: Vec::new(),
            stack_size: 0,
        }
    }


}


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
    NdFuncCall, // 函数调用
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
    pub is_gobal: bool,

    // 函数调用
    pub funcname: Option<&'static str>, // 函数名
    pub args: Vec<Box<Node>>, // 参数
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
                funcname: None,
                args: Vec::new(),
                is_gobal: false,
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
        // 访问链表内的所有参数节点以增加类型
        (self.args).iter_mut().for_each(|temp| {
            temp.add_type();
        });

        match self.kind {
            // 将节点类型设为 节点左部的类型
            NodeType::NdAdd | NodeType::NdSub | NodeType::NdMul | NodeType::NdDiv | NodeType::NdNeg => {
                self.ty = Some(self.lhs.as_ref().unwrap().get_type());
            },
            NodeType::NdAssign => {
                if self.lhs.as_ref().unwrap().get_type().kind == TypeKind::TyArray {
                    panic!("not an lvalue");
                }
                self.ty = Some(self.lhs.as_ref().unwrap().get_type());
            },
            // 将节点类型设为 int类型
            NodeType::NdEq | NodeType::NdNe | NodeType::NdLt | NodeType::NdLe | NodeType::NdNum | NodeType::NdFuncCall => {
                self.ty = Some(Type::new_int_type());
            },
            // 将节点类型设为 变量的类型
            NodeType::NdVar => {
                let var_index = self.var.unwrap();
                if self.is_gobal {
                    let ty = unsafe { GLOBALS[var_index].ty.clone()};
                    self.ty = ty;
                }else {
                    let ty = unsafe { GLOBALS.last().unwrap().locals[var_index].ty.clone() };
                    self.ty = ty;
                    // Some(unsafe { GLOBALS.last().unwrap().locals[var_index].ty.as_ref().unwrap().clone()});
                }
            },
            // 将节点类型设为 指针，并指向左部的类型
            NodeType::NdAddr => {
                // 左部如果是数组, 则为指向数组基类的指针
                if self.lhs.as_ref().unwrap().get_type().kind == TypeKind::TyArray {
                    self.ty = Some(pointer_to(self.lhs.as_ref().unwrap().get_type().base.as_ref().unwrap().clone()));
                    return;
                }
                self.ty = Some(pointer_to(self.lhs.as_ref().unwrap().get_type()));
            },
            // 节点类型：如果解引用指向的是指针，则为指针指向的类型；否则报错
            NodeType::NdDeref => {
                // 如果不存在基类, 则无法解引用
                if self.lhs.as_ref().unwrap().get_type().base.is_none() {
                    panic!("invalid pointer dereference");
                }
                self.ty = Some(self.lhs.as_ref().unwrap().get_type().base.as_ref().unwrap().clone());
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

fn find_var(name: &'static str) -> Option<(usize, bool)> {
    // 查找Locals变量中是否存在同名变量
    if let Some(obj) = unsafe { GLOBALS.last() } {
        for (i, var) in obj.locals.iter().enumerate()  {
            if var.name == name {
                return Some((i, false));
            }
        }
    }
    // 查找Globals变量中是否存在同名变量
    for (i, var) in unsafe { GLOBALS.iter().enumerate() } {
        if var.name == name {
            return Some((i, true));
        }
    }
    None
}

// 新增一个变量
fn new_lvar(tokens: &Vec<Token>, tok: Option<usize>, ty: Box<Type>) -> usize {
    let name = tokens[tok.unwrap()].charactors;
    if let Some((var_index,_)) = find_var(name) {
        return var_index;
    }
    let mut obj = Obj::new();
    obj.name = name;
    obj.ty = Some(ty);
    unsafe { GLOBALS.last_mut().unwrap().locals.push(obj) };
    unsafe { GLOBALS.last().unwrap().locals.len() - 1}
}

// 新增一个全局变量
fn new_gvar(tokens: &Vec<Token>, tok: Option<usize>, ty: Box<Type>) -> usize {
    let name = tokens[tok.unwrap()].charactors;
    if let Some((var_index, is_gobal)) = find_var(name) {
        if is_gobal {
            return var_index;
        }
    }
    let mut obj = Obj::new();
    obj.name = name;
    obj.ty = Some(ty);
    obj.is_local = false;
    obj.is_function = false;
    unsafe { GLOBALS.push(obj) };
    unsafe { GLOBALS.len() - 1}
}


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


// declspec = "int"
// declarator specifier
fn declspec(tokens: &Vec<Token>) -> Box<Type> {
    comsume(tokens, "int");
    Type::new_int_type()
}

// funcParams = (param ("," param)*)? ")"
// param = declspec declarator
fn func_params(tokens: &Vec<Token>) -> Vec<Box<Type>> {
    let mut params = Vec::new();
    let mut flag = 0;
    while get_cur_token(tokens).equal(")") == false {
        if flag > 0 {
            comsume(tokens, ",");
        }
        flag += 1;
        let basety = declspec(tokens);
        let declarty = declarator(tokens, basety);
        params.push(declarty);
    }
    params
}


// typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
fn type_suffix(tokens: &Vec<Token>, mut ty: Box<Type>) -> Box<Type> {
    if get_cur_token(tokens).equal("(") {
        skip();
        ty.kind = TypeKind::TyFunc;
        ty.params = func_params(tokens);
        comsume(tokens, ")");
    } else if get_cur_token(tokens).equal("[") {
        skip();
        let array_len = get_num(tokens);
        skip();
        comsume(tokens, "]");
        ty = type_suffix(tokens, ty);
        ty = Type::array_of(ty, array_len as usize);
    }
    ty
}


// declarator = "*"* ident typeSuffix
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

    let tok_index = Some(unsafe { TOKEN_POS });
    skip();

    // typeSuffix
    ty = type_suffix(tokens, ty);

    // ident
    // 变量名 或 函数名
    ty.tok = tok_index;
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
        return new_add(rhs, lhs);
    } else {
        // ptr + num
        // 指针加法，ptr+1，1不是1个字节而是1个元素的空间，所以需要×Size操作
        let temp = Node::new_binary(NodeType::NdMul, rhs, Node::new_num(lhs.get_type().base.unwrap().size as i32));
        return Node::new_binary(NodeType::NdAdd, lhs, temp);
    }
}

// 解析各种减法
fn new_sub(mut lhs: Box<Node>, mut rhs: Box<Node>) -> Box<Node> {
    // 为左右部添加类型
    lhs.add_type();
    rhs.add_type();
    let size = lhs.get_type().size;
    // num - num
    if lhs.is_integer() && rhs.is_integer() {
        return Node::new_binary(NodeType::NdSub, lhs, rhs);
    }

    // ptr - num
    if lhs.is_ptr() && rhs.is_integer() {
        let temp = Node::new_binary(NodeType::NdMul, rhs.clone(), Node::new_num(size as i32));
        let mut nd = Node::new_binary(NodeType::NdSub, lhs.clone(), temp);
        // 节点类型为指针
        nd.ty = Some(lhs.get_type());
        return nd;
    }

    // ptr - ptr
    if lhs.is_ptr() && rhs.is_ptr() {
        let temp = Node::new_binary(NodeType::NdSub, lhs, rhs);
        return Node::new_binary(NodeType::NdDiv, temp, Node::new_num(size as i32));
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
// unary = ("+" | "-" | "*" | "&") unary | postfix
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

    // postfix
    postfix(tokens)
}

// postfix = primary ("[" expr "]")*
fn postfix(tokens: &Vec<Token>) -> Box<Node> {
    let mut node = primary(tokens);
    loop {
        // "[" expr "]"
        if get_cur_token(tokens).equal("[") {
            skip();
            let idx = expr(tokens);
            comsume(tokens, "]");
            node = Node::new_unary(NodeType::NdDeref, new_add(node, idx));
            continue;
        }
        return node;
    }
}



// 解析函数调用
// funcall = ident "(" (assign ("," assign)*)? ")"
fn funcall(tokens: &Vec<Token>, name: &'static str) -> Box<Node> {
    let mut node = Node::new_node(NodeType::NdFuncCall);
    node.funcname = Some(name);
    comsume(tokens, "(");
    if get_cur_token(tokens).equal(")") == false {
        node.args.push(assign(tokens));
        while get_cur_token(tokens).equal(",") {
            skip();
            node.args.push(assign(tokens));
        }
    }
    comsume(tokens, ")");
    node
}



// 解析括号、数字、变量
// primary = "(" expr ")" | "sizeof" unary | ident funcArgs? | num
fn primary(tokens: &Vec<Token>) -> Box<Node> {
    // "(" expr ")"
    if get_cur_token(tokens).equal("(") {
        skip();
        let node = expr(tokens);
        comsume(tokens, ")");
        return node;
    }

    // "sizeof" unary
    if get_cur_token(tokens).equal("sizeof") {
        comsume(tokens, "sizeof");
        let mut node = unary(tokens);
        node.add_type();
        return Node::new_num(node.ty.unwrap().size as i32);
    }

    // ident args?
    if get_cur_token(tokens).kind == TokenType::TkIdent {
        let name = get_cur_token(tokens).charactors;
        skip();

        // 函数调用
        // args = "(" ")"
        if get_cur_token(tokens).equal("(") {
            return funcall(tokens, name);
        }

        if let Some((var_index, is_gobal)) = find_var(name) {
            let mut tmp = Node::new_varnode(var_index);
            tmp.is_gobal = is_gobal;
            return tmp;
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


// 将形参添加到局部变量
fn create_param_lvars(tokens: &Vec<Token>, params: Vec<Box<Type>>) {
    params.iter().rev().for_each(|ty|{
        new_lvar(tokens, ty.tok, ty.clone());
    });
}

// 区分 函数还是全局变量
fn is_function(tokens: &Vec<Token>) -> bool {
    // 记录当前Token位置
    let index = unsafe { TOKEN_POS };
    if get_cur_token(tokens).equal(";") {
        return false;
    }
    // 虚设变量，用于调用declarator
    let ty = Type::new_int_type();
    let ty = declarator(tokens, ty);
    // 恢复Token位置
    unsafe { TOKEN_POS = index };
    return ty.kind == TypeKind::TyFunc;
}  

// 构造全局变量
fn global_variable(tokens: &Vec<Token>, basety: Box<Type>) {
    let mut first = true;
    while get_cur_token(tokens).equal(";") == false{
        if first == false {
            comsume(tokens, ",");
        }
        first = false;
        let ty = declarator(tokens, basety.clone());
        new_gvar(tokens, ty.tok, ty);
    }
    comsume(tokens, ";");

}



// functionDefinition = declspec declarator "{" compoundStmt*
fn function(tokens: &Vec<Token>, basety: Box<Type>) {
    let mut func = Obj::new();
    func.is_function = true;
    unsafe { GLOBALS.push(func) };
    let ty = declarator(tokens, basety);

    // 函数名
    let name = tokens[ty.tok.unwrap()].charactors;
    unsafe { GLOBALS.last_mut().unwrap().name = name };
    // 函数参数
    create_param_lvars(tokens, ty.params);
    unsafe { GLOBALS.last_mut().unwrap().params = GLOBALS.last().unwrap().locals.clone() };


    comsume(tokens, "{");
    // 函数体存储语句的AST，Locals存储变量
    let tmp = compound_stmt(tokens);
    unsafe { GLOBALS.last_mut().unwrap().body.push(tmp) };
}