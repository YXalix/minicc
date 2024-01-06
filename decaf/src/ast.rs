
#[derive(Debug)]
pub struct Prog {
  pub funcs: Vec<Func>,
  pub globs: Vec<Decl>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeKind {
  TyChar,
  TyInt,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ty {
  pub kind: TypeKind,  // 类型种类
  pub count: u32,      // 指针重数
}

#[derive(Debug)]
pub struct Decl {
  pub ty: Ty,
  pub name: &'static str,
  // 这个变量的数组维度，例如int a[1][2]的dims就是vec![1, 2]，如果不是数组就是空Vec
  pub dims: Vec<u32>,
  // 一个可选的初始值
  pub init: Option<Expr>,
}

#[derive(Debug)]
pub struct Func {
  pub ret: Ty,
  pub name: &'static str,
  pub params: Vec<Decl>,
  // 函数定义中stmts为Some，函数声明中stmts为None
  pub stmts: Option<Vec<Stmt>>,
}

#[derive(Debug)]
pub enum Stmt {
  Empty,
  Ret(Expr),
  Decl(Decl),
  Expr(Expr),
  // 这里的Stmt实际不可能是Stmt::Decl，parser不会生成这样的结构，While/DoWhile/For中的Stmt也是一样的
  If(Expr, Box<Stmt>, Option<Box<Stmt>>),
  Block(Vec<Stmt>),
  // for的init语句不在这里，一条for (init; cond; update) body在AST中表示为：
  // Block(vec![init, For { cond, update, body }])
  // 而且init语句只能是三种：Empty/Decl/Expr，这也是parser决定的
  For { cond: Option<Expr>, update: Option<Expr>, body: Box<Stmt> },
  Continue,
  Break,
}


#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]

// "+" | "-" | "*" | "&"
pub enum UnaryOp { Plus, Neg, Deref, AddrOf}


#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum BinaryOp { Add, Sub, Mul, Div, Mod, Lt, Le, Eq, Ne, And, Or }

#[derive(Debug)]
pub enum Expr {
  Int(i32),
  Unary(UnaryOp, Box<Expr>),
  Binary(BinaryOp, Box<Expr>, Box<Expr>),
  Var(&'static str),
  Assign(Box<Expr>, Box<Expr>),
  // 三个Box<Expr>分别是a ? b : c中的a，b，c
  Condition(Box<Expr>, Box<Expr>, Box<Expr>),
  Call(&'static str, Vec<Expr>),
  Deref(Box<Expr>),
  AddrOf(Box<Expr>),
  SizeOf(Box<Expr>),
  Cast(Ty, Box<Expr>),
  // 两个Box<Expr>分别是a[b]中的a，b。类似a[b][c][d]这样的结构用多重的Index来表示
  Index(Box<Expr>, Box<Expr>),
}
