
#[derive(Debug)]
pub struct Prog<'a> {
  pub funcs: Vec<Func<'a>>,
  pub globs: Vec<Decl<'a>>,
}

// 因为类型只可能是int或者int的若干重指针，所以只用记录指针的重数即可(即int **...*中*的个数)
pub type Ty = u32;

#[derive(Debug)]
pub struct Decl<'a> {
  pub ty: Ty,
  pub name: &'a str,
  // 这个变量的数组维度，例如int a[1][2]的dims就是vec![1, 2]，如果不是数组就是空Vec
  pub dims: Vec<u32>,
  // 一个可选的初始值
  pub init: Option<Expr<'a>>,
}

#[derive(Debug)]
pub struct Func<'a> {
  pub ret: Ty,
  pub name: &'a str,
  pub params: Vec<Decl<'a>>,
  // 函数定义中stmts为Some，函数声明中stmts为None
  pub stmts: Option<Vec<Stmt<'a>>>,
}

#[derive(Debug)]
pub enum Stmt<'a> {
  Empty,
  Ret(Expr<'a>),
  Decl(Decl<'a>),
  Expr(Expr<'a>),
  // 这里的Stmt实际不可能是Stmt::Decl，parser不会生成这样的结构，While/DoWhile/For中的Stmt也是一样的
  If(Expr<'a>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
  Block(Vec<Stmt<'a>>),
  // AST中没有While的结构，while语句直接用update为None的For来表示
  DoWhile(Box<Stmt<'a>>, Expr<'a>),
  // for的init语句不在这里，一条for (init; cond; update) body在AST中表示为：
  // Block(vec![init, For { cond, update, body }])
  // 而且init语句只能是三种：Empty/Decl/Expr，这也是parser决定的
  For { cond: Option<Expr<'a>>, update: Option<Expr<'a>>, body: Box<Stmt<'a>> },
  Continue,
  Break,
}


#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum UnaryOp { Neg, BNot, LNot }

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum BinaryOp { Add, Sub, Mul, Div, Mod, Lt, Le, Ge, Gt, Eq, Ne, And, Or }

#[derive(Debug)]
pub enum Expr<'a> {
  Int(i32),
  Unary(UnaryOp, Box<Expr<'a>>),
  Binary(BinaryOp, Box<Expr<'a>>, Box<Expr<'a>>),
  Var(&'a str),
  Assign(Box<Expr<'a>>, Box<Expr<'a>>),
  // 三个Box<Expr<'a>>分别是a ? b : c中的a，b，c
  Condition(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
  Call(&'a str, Vec<Expr<'a>>),
  Deref(Box<Expr<'a>>),
  AddrOf(Box<Expr<'a>>),
  Cast(Ty, Box<Expr<'a>>),
  // 两个Box<Expr<'a>>分别是a[b]中的a，b。类似a[b][c][d]这样的结构用多重的Index来表示
  Index(Box<Expr<'a>>, Box<Expr<'a>>),
}