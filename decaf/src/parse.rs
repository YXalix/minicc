use crate::ltex::{Tokenizer, TokenType};
use crate::ast::{*, UnaryOp::*, BinaryOp::*};
// GLOBALS = functionDefinition*
// Globals = declspec declarator typeSuffix ";"
// functionDefinition = declspec declarator "{" compoundStmt*
// declspec = "char" | "int"
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
    pub fn parse(p: &mut Tokenizer) -> Prog {
        let mut prog = Prog {
            funcs: Vec::new(),
            globs: Vec::new()
        };
        while p.cur_token().kind != TokenType::TkEof {
            // 函数
            if Parser::is_function(p) {
                let func = Parser::prog_func(p);
                prog.funcs.push(func);
                continue;
            }
            // 全局变量
            let glob = Parser::prog_glob(p);
            prog.globs.push(glob);
        }
        prog
    }

    // 区分 函数还是全局变量
    fn is_function(p: &mut Tokenizer) -> bool {
        // 记录当前的token的index
        let index = p.now_index;

        let ty = Parser::declspec(p);

        if p.cur_token().equal(";") {
            return false;
        }
        let (params, _) = Parser::declarator(p, ty);
        // 还原index
        p.now_index = index;
        // 如果是函数
        if let Some(_) = params {
            return true;
        }
        // 如果是全局变量
        return false;
    }

    fn prog_glob(p: &mut Tokenizer) -> Decl {
        let ty = Parser::declspec(p);
        let (_, decl) = Parser::declarator(p, ty);
        if let Some(decl) = decl {
            return decl;
        } else {
            panic!("expected decl, but got {:?}", p.cur_token());
        }
    }


    // functionDefinition = declspec declarator "{" compoundStmt*
    fn prog_func(p: &mut Tokenizer) -> Func {
        let ret = Parser::declspec(p);
        let (func, _) = Parser::declarator(p, ret);
        p.consume("{");

        let stmts = Parser::compound_stmt(p);
        let mut func = func.unwrap();
        func.stmts = Some(stmts);
        return func;
    }

    // declspec = "char" | "int"
    // declarator specifiers
    fn declspec(p: &mut Tokenizer) -> Ty {
        if p.cur_token().charactors == "char" {
            p.skip();
            return Ty { kind: TypeKind::TyChar, count: 0 };
        }
        if p.cur_token().charactors == "int" {
            p.skip();
            return Ty { kind: TypeKind::TyInt, count: 0 };
        }
        panic!("unexpected token: {:?}", p.cur_token());
    }

    // declarator = "*"* ident typeSuffix
    fn declarator(p: &mut Tokenizer, ty: Ty) -> (Option<Func>, Option<Decl>) {
        let mut ty = ty;
        while p.cur_token().charactors == "*" {
            p.skip();
            ty.count += 1;
        }
        let name = p.cur_token().charactors;
        p.skip();
        let mut dims = Vec::new();
        let (params, dims) = Parser::type_suffix(p, &mut dims);

        // 如果是函数
        if let Some(params) = params {
            let func = Func { ret: ty, name, params, stmts: None };
            return (Some(func), None);
        } else {
            let decl = Decl { ty, name, dims, init: None };
            return (None, Some(decl));
        }
    }

    // typeSuffix = "(" funcParams | "[" num "]" typeSuffix | ε
    fn type_suffix(p: &mut Tokenizer, dims: &mut Vec<u32>) -> (Option<Vec<Decl>>, Vec<u32>) {
        if p.cur_token().charactors == "(" {
            let params = Parser::func_params(p);
            return (Some(params), Vec::new());
        }
        if p.cur_token().charactors == "[" {
            p.skip();
            if let TokenType::TkNum(x) = p.cur_token().kind {
                p.skip();
                p.consume("]");
                dims.push(x as u32);
                let (_, dims) = Parser::type_suffix(p, dims);
                return (None, dims);
            } else {
                panic!("expected number, but got {:?}", p.cur_token());
            }
        }
        return (None, Vec::new());
    }

    // funcParams = (param ("," param)*)? ")"
    // param = declspec declarator
    fn func_params(p: &mut Tokenizer) -> Vec<Decl> {
        let mut params = Vec::new();
        p.consume("(");
        let mut flag = true;
        while p.cur_token().charactors != ")" {
            if flag {
                p.consume(",");
            }
            flag = false;
            let ty = Parser::declspec(p);
            let (_, decl) = Parser::declarator(p, ty);
            if let Some(decl) = decl {
                params.push(decl);
            } else {
                panic!("expected decl, but got {:?}", p.cur_token());
            }
        }
        p.consume(")");
        return params;
    }


    // compoundStmt = (declaration | stmt)* "}"
    fn compound_stmt(p: &mut Tokenizer) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while p.cur_token().charactors != "}" {
            if p.cur_token().charactors == "char" || p.cur_token().charactors == "int" {
                let decls = Parser::declaration(p);
                for decl in decls {
                    stmts.push(Stmt::Decl(decl));
                }
            } else {
                let stmt = Parser::stmt(p);
                stmts.push(stmt);
            }
        }
        p.skip();
        return stmts;
    }

    // stmt = "return" expr ";"
    //        | "if" "(" expr ")" stmt ("else" stmt)?
    //        | "for" "(" exprStmt expr? ";" expr? ")" stmt
    //        | "while" "(" expr ")" stmt
    //        | "{" compoundStmt
    //        | exprStmt
    fn stmt(p: &mut Tokenizer) -> Stmt {
        // "return" expr ";"
        if p.cur_token().charactors == "return" {
            p.skip();
            let expr = Parser::expr(p);
            p.consume(";");
            return Stmt::Ret(expr);
        }

        // "if" "(" expr ")" stmt ("else" stmt)?
        if p.cur_token().charactors == "if" {
            p.skip();
            p.consume("(");
            let expr = Parser::expr(p);
            p.consume(")");
            let stmt = Parser::stmt(p);
            let mut else_stmt = None;
            if p.cur_token().charactors == "else" {
                p.skip();
                else_stmt = Some(Box::new(Parser::stmt(p)));
            }
            return Stmt::If(expr, Box::new(stmt), else_stmt);
        }

        // "for" "(" exprStmt expr? ";" expr? ")" stmt
        if p.cur_token().charactors == "for" {
            p.skip();
            p.consume("(");
            let init = Parser::expr_stmt(p);
            let mut cond = None;
            if p.cur_token().charactors != ";" {
                cond = Some(Parser::expr(p));
            }
            p.consume(";");
            let mut update = None;
            if p.cur_token().charactors != ")" {
                update = Some(Parser::expr(p));
            }
            p.consume(")");
            let body = Parser::stmt(p);
            return Stmt::Block( vec![ init, Stmt::For { cond, update, body: Box::new(body) }]);
        }

        // "while" "(" expr ")" stmt
        if p.cur_token().charactors == "while" {
            p.skip();
            p.consume("(");
            let cond = Parser::expr(p);
            p.consume(")");
            let body = Parser::stmt(p);
            return Stmt::For { cond: Some(cond), update: None, body: Box::new(body) };
        }

        // "{" compoundStmt
        if p.cur_token().charactors == "{" {
            p.skip();
            let stmts = Parser::compound_stmt(p);
            return Stmt::Block(stmts);
        }

        // exprStmt
        let e_stmt = Parser::expr_stmt(p);
        return e_stmt;
    }

    // exprStmt = expr? ";"
    fn expr_stmt(p: &mut Tokenizer) -> Stmt {
        if p.cur_token().charactors == ";" {
            p.skip();
            return Stmt::Empty;
        }
        let expr = Parser::expr(p);
        p.consume(";");
        return Stmt::Expr(expr);
    }


    // declaration =
    //    declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
    fn declaration(p: &mut Tokenizer) -> Vec<Decl> {
        let ty = Parser::declspec(p);
        let mut decls = Vec::new();
        loop {
            let (_, decl) = Parser::declarator(p, ty);
            if let Some(decl) = decl {
                if p.cur_token().charactors == "=" {
                    p.skip();
                    let init = Parser::expr(p);
                    decls.push(Decl { init: Some(init), ..decl });
                } else {
                    decls.push(decl);
                }
            } else {
                panic!("expected decl, but got {:?}", p.cur_token());
            }
            if p.cur_token().charactors == ";" {
                break;
            }
            p.consume(",");
        }
        p.consume(";");
        return decls;
    }
    

    // expr = assign
    fn expr(p: &mut Tokenizer) -> Expr {
        return Parser::assign(p);
    }

    // assign = equality ("=" assign)?
    fn assign(p: &mut Tokenizer) -> Expr {
        let mut expr = Parser::equality(p);
        if p.cur_token().charactors == "=" {
            p.skip();
            let rhs = Parser::assign(p);
            expr = Expr::Assign(Box::new(expr), Box::new(rhs));
        }
        return expr;
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(p: &mut Tokenizer) -> Expr {
        let mut expr = Parser::relational(p);
        while p.cur_token().charactors == "==" || p.cur_token().charactors == "!=" {
            let op = p.cur_token().charactors;
            p.skip();
            let rhs = Parser::relational(p);
            if op == "==" {
                expr = Expr::Binary( Eq, Box::new(expr), Box::new(rhs) );
            } else {
                expr = Expr::Binary( Ne, Box::new(expr), Box::new(rhs) );
            }
        }
        return expr;
    }

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(p: &mut Tokenizer) -> Expr {
        let mut expr = Parser::add(p);
        while p.cur_token().charactors == "<" || p.cur_token().charactors == "<="
            || p.cur_token().charactors == ">" || p.cur_token().charactors == ">=" {
            let op = p.cur_token().charactors;
            p.skip();
            let rhs = Parser::add(p);
            if op == "<" {
                expr = Expr::Binary( Lt, Box::new(expr), Box::new(rhs) );
            } else if op == "<=" {
                expr = Expr::Binary( Le, Box::new(expr), Box::new(rhs) );
            } else if op == ">" {
                expr = Expr::Binary( Lt, Box::new(rhs), Box::new(expr) );
            } else {
                expr = Expr::Binary( Le, Box::new(rhs), Box::new(expr) );
            }
        }
        return expr;
    }

    // add = mul ("+" mul | "-" mul)*
    fn add(p: &mut Tokenizer) -> Expr {
        let mut expr = Parser::mul(p);
        while p.cur_token().charactors == "+" || p.cur_token().charactors == "-" {
            let op = p.cur_token().charactors;
            p.skip();
            let rhs = Parser::mul(p);
            if op == "+" {
                expr = Expr::Binary( Add, Box::new(expr), Box::new(rhs) );
            } else {
                expr = Expr::Binary( Sub, Box::new(expr), Box::new(rhs) );
            }
        }
        return expr;
    }

    // mul = unary ("*" unary | "/" unary)*
    fn mul(p: &mut Tokenizer) -> Expr {
        let mut expr = Parser::unary(p);
        while p.cur_token().charactors == "*" || p.cur_token().charactors == "/" {
            let op = p.cur_token().charactors;
            p.skip();
            let rhs = Parser::unary(p);
            if op == "*" {
                expr = Expr::Binary( Mul, Box::new(expr), Box::new(rhs) );
            } else {
                expr = Expr::Binary( Div, Box::new(expr), Box::new(rhs) );
            }
        }
        return expr;
    }


    // unary = ("+" | "-" | "*" | "&") unary | postfix
    fn unary(p: &mut Tokenizer) -> Expr {
        // ("+" | "-" | "*" | "&") unary
        if p.cur_token().equal("+") {
            p.skip();
            return Expr::Unary( Plus, Box::new(Parser::unary(p)) );
        }
        if p.cur_token().equal("-") {
            p.skip();
            return Expr::Unary( Neg, Box::new(Parser::unary(p)) );
        }
        if p.cur_token().equal("*") {
            p.skip();
            return Expr::Unary( Deref, Box::new(Parser::unary(p)) );
        }
        if p.cur_token().equal("&") {
            p.skip();
            return Expr::Unary( AddrOf, Box::new(Parser::unary(p)) );
        }
        // postfix
        return Parser::postfix(p);
    }

    // postfix = primary ("[" expr "]")*
    fn postfix(p: &mut Tokenizer) -> Expr {
        let mut expr = Parser::primary(p);
        while p.cur_token().charactors == "[" {
            p.skip();
            let index = Parser::expr(p);
            p.consume("]");
            expr = Expr::Index(Box::new(expr), Box::new(index));
        }
        return expr;
    }

    // 解析函数调用
    // funcall = ident "(" (assign ("," assign)*)? ")"
    fn func_args(p: &mut Tokenizer) -> Vec<Expr> {
        let mut args = Vec::new();
        p.consume("(");
        if p.cur_token().charactors != ")" {
            args.push(Parser::expr(p));
            while p.cur_token().charactors == "," {
                p.skip();
                args.push(Parser::expr(p));
            }
        }
        p.consume(")");
        return args;
    }


    // primary = "(" expr ")" | "sizeof" unary | ident funcArgs? | num
    fn primary(p: &mut Tokenizer) -> Expr {
        if p.cur_token().charactors == "(" {
            p.skip();
            let expr = Parser::expr(p);
            p.consume(")");
            return expr;
        }
        if p.cur_token().charactors == "sizeof" {
            p.skip();
            let expr = Parser::unary(p);
            return Expr::SizeOf(Box::new(expr));
        }
        match p.cur_token().kind {
            TokenType::TkNum(x) => {
                p.skip();
                return Expr::Int(x);
            },
            TokenType::TkIdent => {
                let name = p.cur_token().charactors;
                p.skip();
                if p.cur_token().charactors == "(" {
                    let args = Parser::func_args(p);
                    return Expr::Call(name, args);
                } else {
                    return Expr::Var(name);
                }
            },
            _ => panic!("unexpected token: {:?}", p.cur_token()),
        }
    }



}