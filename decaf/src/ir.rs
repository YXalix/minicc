use std::collections::{HashMap, hash_map::Entry};
use crate::ast::{*, BinaryOp::*};

#[derive(Debug)]
pub struct IrProg {
  pub funcs: Vec<IrFunc>,
  // 每一项是(名字，初始值/数组大小)
  // Result<i32, u32>为Ok时，表示int变量的初始值；为Err时，表示数组的大小(全局数组初始值只能是全0)
  pub globs: Vec<(&'static str, Result<i32, u32>)>,
}

#[derive(Debug)]
pub struct IrFunc {
  pub name: &'static str,
  // 本函数接受参数的数目
  pub param_cnt: u32,
  // 本函数局部变量的数目(参数不属于局部变量)
  pub var_cnt: u32,
  pub stmts: Vec<IrStmt>,
}

#[derive(Debug)]
pub enum IrStmt {
    // 把一个常数压入栈中
    Num(i32),
    // 弹出栈顶元素，对其进行相应的UnaryOp后把结果压入栈顶
    Unary(UnaryOp),
    // 依次弹出栈顶的两个元素，分别作为右操作数和左操作数(右操作数在栈顶，左操作数是下面一个)，对其进行相应的BinaryOp后把结果压入栈顶
    Binary(BinaryOp),
    // 将对应id的局部变量的实际地址压入栈顶
    LocalAddr(u32),
    // 将对应id的全局变量的实际地址压入栈顶
    GlobAddr(u32),
    // 加载a0指向的值
    Load,
    // 将a0的值, 写入到a1中存放的地址
    Store,
    // 将寄存器[]的值乘以[] 再存入寄存器[]
    Mul(u32, u32),
    // store 参数寄存器
    StoreParam(u32),

    // 定义一个标号，不涉及任何操作
    Label(u32),
    // 弹出栈顶的值，如果它等于0，则跳转到对应标号，否则继续执行下一条语句
    Bz(u32),
    // 弹出栈顶的值，如果它不等于0，则跳转到对应标号，否则继续执行下一条语句
    Bnz(u32),
    // 跳转到对应标号
    Jump(u32),
    // 调用IrProg.funcs对应下标的函数，调用前运算栈中需要从左到右依次压入参数(最右边的参数在栈顶
    // 调用后这些参数都被弹出，且运算栈中压入函数的返回值
    // 注意这里并不涉及调用约定的细节，比如到底是caller还是callee把参数弹出运算栈，把返回值压入运算栈的，只是说整个调用结束后结果应该是这样
    Call(u32),
    // 交换栈顶和次栈顶的两个元素，这条指令目前只是为了实现整数+指针而存在的
    Swap,
    // 入栈
    Push,
    // 弹出栈顶元素
    Pop(u32),
    // 弹出栈顶元素，将其作为返回值返回当前函数
    Ret,
}



pub fn ast2ir<'a>(p: & Prog) -> IrProg {
    // globs用来表示函数名到全局变量的映射，globs1用来保存全局变量的初始值
    // 先访问一遍所有全局变量，所以每个函数都可以使用每个全局变量，不管它们的定义的相对位置是什么样的
    let (mut globs, mut globs1) = (HashMap::new(), Vec::new());

    for d in &p.globs {
        // 全局变量要么不初始化(与用整数常量0初始化等价)，要么用整数常量初始化，不能用其他表达式初始化
        let init = match &d.init {
            None => Ok(0),
            Some(e) => match &e {
                Expr::Int(i) => Ok(*i),
                _ => panic!("global variable can only be initialized by integer constant"),
            }
        };
        if globs.insert(d.name, (globs1.len() as u32, d)).is_some() {
            panic!("duplicated global variable name: {}", d.name);
        }
        globs1.push((d.name, init));
    }
    // funcs用来表示函数名到符号的映射，funcs1用来保存所有函数定义的ir
    // funcs的V类型是(u32, &Func)，这个u32表示本函数的ir在funcs1中的下标
    let (mut funcs, mut funcs1) = (HashMap::new(), Vec::new());
    for f in &p.funcs {
        if globs.contains_key(f.name) {
            panic!("function name duplicated with global variable: {}", f.name);
        }
        match funcs.entry(f.name) {
            Entry::Vacant(v) => {
              v.insert((funcs1.len() as u32, f));
              // 这个f可能只是一个函数声明，这样构造出来的ir只有空架子，后续遇到同名的函数定义时会更新
              funcs1.push(func(f, &funcs, &globs));
            }
            Entry::Occupied(o) => {
              let (old_id, old_f) = *o.get();
              // stmts.is_some()为true则是函数定义，为false则是函数声明
              // 如果两个重名的函数都是函数定义，或者它们的参数/返回值不匹配，则不合法
              if (old_f.stmts.is_some() && f.stmts.is_some()) || old_f.ret != f.ret || old_f.params.len() != f.params.len()
                || old_f.params.iter().zip(f.params.iter()).any(|(x, y)| x.ty != y.ty) {
                panic!("conflict function definition `{}` in current context", f.name)
              }
              // 如果老函数是函数声明，新函数是函数定义，则更新ir中对应下标的函数
              if f.stmts.is_some() { funcs1[old_id as usize] = func(f, &funcs, &globs); }
            }
          }
    }
    IrProg { funcs: funcs1, globs: globs1 }
}

// 将变量的名字映射到(变量的id，变量的定义)，这个Decl中目前还没有保存有用的信息，之后会用到它的
// 这个id基本可以理解成变量在栈上的offset
type SymbolMap<'a> = HashMap<&'static str, (u32, &'a Decl)>;
type FuncMap<'a> = HashMap<&'static str, (u32, &'a Func)>;


// 为一个函数生成IR的过程中维护的一些信息
struct FuncCtx<'a, 'b> {
    // 每个语句块对应一个SymbolMap，进入一个语句块时往其中压入一个新的SymbolMap，离开一个语句块时弹出最后的SymbolMap
    names: Vec<SymbolMap<'a>>,
    stmts: Vec<IrStmt>,
    // 遇到一个循环时往其中压入一对值，分别是(这个循环中break要跳转的位置，这个循环中continue要跳转的位置)，离开循环时就弹出这个值
    // 处理break/continue时总会访问最后一个元素，如果最后一个元素不存在，就意味着break/continue在循环外
    loops: Vec<(u32, u32)>,
    funcs: &'b FuncMap<'a>,
    globs: &'b SymbolMap<'a>,
    // 本函数的返回类型
    ret: Ty,
    // 当前局部变量的数目
    var_cnt: u32,
    // 当前标号的数目
    label_cnt: u32,
}

impl<'a> FuncCtx<'a, '_> {
    fn new_label(&mut self) -> u32 { (self.label_cnt, self.label_cnt += 1).0 }
    // 在当前环境中查找对应名称的变量，如果找到了就返回(它是全局变量吗，它的id，它的定义)，否则就panic
    fn lookup(&self, name: &str) -> (bool, u32, &'a Decl) {
      // 在所有SymbolMap中逆序查找，这样就会优先找到本条语句所在的语句块中定义的变量，越往外优先级越低
      for map in self.names.iter().rev() {
        if let Some(x) = map.get(name) { return (false, x.0, x.1); }
      }
      // 全局变量是最后参与查找的
      if let Some(x) = self.globs.get(name) { return (true, x.0, x.1); }
      panic!("variable `{}` not defined in current context", name)
    }
  }


fn func<'a>(f: &Func, funcs: &FuncMap<'a>, globs: &SymbolMap<'a>) -> IrFunc {
    let mut ctx = FuncCtx {
        names: vec![SymbolMap::new()],
        stmts: Vec::new(),
        loops: Vec::new(),
        funcs,
        globs,
        ret: f.ret,
        var_cnt: 0,
        label_cnt: 0,
    };
    // 参数和局部变量一起参与id的分配，参数0的id是0，第一个局部变量的id是参数的数目.
    // 后面生成汇编代码的时候，也会设法保证参数和局部变量的id不会冲突
    for p in &f.params {
        decl(&mut ctx, p, true);
    }

    for s in f.stmts.as_ref().unwrap() {
        stmt(&mut ctx, s);
    }
    // 如果函数的指令序列不以Ret结尾，则生成一条return 0
    match ctx.stmts.last() {
        Some(IrStmt::Ret) => (),
        _ => {
            ctx.stmts.push(IrStmt::Num(0));
            ctx.stmts.push(IrStmt::Ret);
        },
    }
    // 现在的ctx.var_cnt是包含了参数数目在内的，要得到真正的局部变量的数目需要减去参数数目
    let param_cnt = f.params.len() as u32;
    IrFunc { name: f.name, param_cnt, var_cnt: ctx.var_cnt - param_cnt, stmts: ctx.stmts }
    
}


fn stmt<'a>(ctx: &mut FuncCtx<'a, '_>, s: &'a Stmt) {
    match s {
        Stmt::Empty => (),
        Stmt::Ret(e) => {
            // 为了翻译一条return语句，先翻译它return的表达式，这样a0寄存器就是这个表达式的值
            genexpr(ctx, e);
            ctx.stmts.push(IrStmt::Ret);
        },
        Stmt::Decl(d) => decl(ctx, d, false),
        Stmt::Expr(e) => {
            let (_, _) = genexpr(ctx, e);
        },
        Stmt::If(cond, then, els) => {
            // 为了翻译一个if语句，先翻译它的条件，这样栈顶就是条件的值，再生成一条Bz指令，如果栈顶的值为0，则跳转到一个新的标号
            genexpr(ctx, cond);
            let label = ctx.new_label();
            ctx.stmts.push(IrStmt::Bz(label));
            stmt(ctx, then);
            // 如果有else分支，还要生成一条跳转到另一个新的标号的指令，这样if分支执行完后就会跳过else分支
            if let Some(els) = els {
                let label2 = ctx.new_label();
                ctx.stmts.push(IrStmt::Jump(label2));
                ctx.stmts.push(IrStmt::Label(label));
                stmt(ctx, els);
                ctx.stmts.push(IrStmt::Label(label2));
            } else {
                ctx.stmts.push(IrStmt::Label(label));
            }
        },
        Stmt::Block(stmts) => {
            ctx.names.push(SymbolMap::new());
            for s in stmts {
                stmt(ctx, s);
            }
            ctx.names.pop();
        },
        Stmt::For { cond, update, body } => {
            let (before_cond, before_update, after_body) = (ctx.new_label(), ctx.new_label(), ctx.new_label());
            ctx.loops.push((after_body, before_update));
            ctx.stmts.push(IrStmt::Label(before_cond)); // 循环体的开头
            if let Some(cond) = cond {
                genexpr(ctx, cond);
                ctx.stmts.push(IrStmt::Bz(after_body));
            }
            stmt(ctx, body);
            ctx.stmts.push(IrStmt::Label(before_update)); // continue的位置，continue后仍要执行本次循环的update语句
            if let Some(update) = update {
                genexpr(ctx, update);
            }
            ctx.stmts.push(IrStmt::Jump(before_cond)); // 跳转到循环体的开头
            ctx.stmts.push(IrStmt::Label(after_body)); // 循环体的结尾
            ctx.loops.pop();
        },
        Stmt::Continue => todo!(),
        Stmt::Break => todo!(),
    }
}

// 在当前环境中定义一个变量
fn decl<'a>(ctx: &mut FuncCtx<'a, '_>, d: &'a Decl, is_param: bool) {
    // 有数组之前每个变量只占据栈中的一个位置，现在有数组了，一个变量可以占据多个位置
    // 为了让变量id能够容易地对应到变量在栈上的偏移量，这里也需要把id增加数组大小的数目
    // 而且代码生成阶段栈是向下增长的，但数组地址必须是数组中地址最低的地址，所以用数组中最后一个元素的id来表示数组
    let mut id = ctx.var_cnt + d.dims.iter().product::<u32>() - 1;
    // 只在最后一个SymbolMap，也就是当前语句所在的语句块的SymbolMap中定义这个变量
    if ctx.names.last_mut().unwrap().contains_key(d.name) {
        id = ctx.names.last_mut().unwrap().get(d.name).unwrap().0;
    } else {
        ctx.names.last_mut().unwrap().insert(d.name, (id, d));
        ctx.var_cnt += 1;
    }
    if let Some(x) = &d.init {
        if is_param {
            ctx.stmts.push(IrStmt::StoreParam(id));
        } else {
            ctx.stmts.push(IrStmt::LocalAddr(id));
            ctx.stmts.push(IrStmt::Push);
            genexpr(ctx, x);
            // 将栈顶的值弹出，将其作为地址，将a0的值写入到这个地址中
            ctx.stmts.push(IrStmt::Store);
        }
    }
}

// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存中
fn gen_addr<'a>(ctx: &mut FuncCtx<'a, '_>, e: &'a Expr) -> (Ty, Vec<u32>) {
    match e {
        Expr::Var(name) => {
            let (is_glob, id, d) = ctx.lookup(name);
            if is_glob {
                ctx.stmts.push(IrStmt::GlobAddr(id));
            } else {
                ctx.stmts.push(IrStmt::LocalAddr(id));
            }
            return (d.ty, d.dims.clone())
        },
        Expr::Unary(UnaryOp::Deref, e) => {
            let (ty, dims) = genexpr(ctx, e);
            return (Ty{kind: ty.kind, count: ty.count - 1}, dims)
        },
        _ => panic!("not an lvalue"),
    }
}


fn genexpr<'a>(ctx: &mut FuncCtx<'a, '_>, e: &'a Expr) -> (Ty, Vec<u32>) {
    match e {
        Expr::Int(i) => {
            ctx.stmts.push(IrStmt::Num(*i));
            (Ty{kind: TypeKind::TyInt, count: 0,}, vec![]) // 0重指针，也就是int
        },
        Expr::Unary(op, e) => {
            match op {
                UnaryOp::AddrOf => {
                    // 为了翻译一个取地址的表达式，先翻译它的子表达式
                    let (ty, dims) = gen_addr(ctx, e);
                    (Ty{kind: ty.kind, count: ty.count + 1}, dims)
                },
                UnaryOp::Deref => {
                    // 为了翻译一个取值的表达式，先翻译它的子表达式, 再按照子表达式的值作为地址取值
                    let (ty, dims) = genexpr(ctx, e);
                    ctx.stmts.push(IrStmt::Load);
                    return (Ty{kind: ty.kind, count: ty.count - 1}, dims)
                },
                _ => {
                    // 为了翻译一个unary表达式，先翻译它的子表达式，这样栈顶就是子表达式的值，再对栈顶的值进行相应的操作
                    let (ty, dims) = genexpr(ctx, e);
                    ctx.stmts.push(IrStmt::Unary(*op));
                    (ty, dims)
                }
            }
            
        },
        Expr::Binary(op, l, r) => {
            match op {
                Add | Sub => {
                    // 为了翻译一个binary表达式，先翻译它的左右子表达式，这样寄存器a1就是右子表达式的值，寄存器a0是左子表达式的值
                    let (ty2, dims2) = genexpr(ctx, r);
                    ctx.stmts.push(IrStmt::Push);
                    let (ty1, dims1) = genexpr(ctx, l);
                    ctx.stmts.push(IrStmt::Pop(1));
                    // 整数+整数，结果是整数
                    if ty1.count == 0 && ty2.count == 0 {
                        ctx.stmts.push(IrStmt::Binary(*op));
                        (Ty{kind: TypeKind::TyInt, count: 0,}, vec![])
                    } else if ty1.count == 0 && ty2.count > 0 {
                        // 整数+指针，结果是指针
                        ctx.stmts.push(IrStmt::Mul(0, 8));
                        ctx.stmts.push(IrStmt::Binary(*op));
                        (Ty{kind: ty2.kind, count: ty2.count,}, dims2)
                    } else if ty1.count > 0 && ty2.count == 0 {
                        // 指针+整数，结果是指针
                        ctx.stmts.push(IrStmt::Mul(1, 8));
                        ctx.stmts.push(IrStmt::Binary(*op));
                        (Ty{kind: ty1.kind, count: ty1.count,}, dims1)
                    } else {
                        // 指针+指针，不合法
                        panic!("invalid binary operation")
                    }
                },
                _ => {
                    // 为了翻译一个binary表达式，先翻译它的左右子表达式，这样栈顶就是右子表达式的值，次栈顶是左子表达式的值
                    let (_, _) = genexpr(ctx, r);
                    ctx.stmts.push(IrStmt::Push);
                    let (ty1, dims1) = genexpr(ctx, l);
                    ctx.stmts.push(IrStmt::Pop(1));
                    ctx.stmts.push(IrStmt::Binary(*op));
                    return (ty1, dims1)
                }
            }
        },
        Expr::Assign(e1, e2) => {
            let (ty1, _) = gen_addr(ctx, e1);
            ctx.stmts.push(IrStmt::Push);
            let (_, _) = genexpr(ctx, e2);
            ctx.stmts.push(IrStmt::Store);
            (ty1, vec![])
        },
        Expr::Var(_) => {
            let (ty, dims) = gen_addr(ctx, e);
            ctx.stmts.push(IrStmt::Load);
            (ty, dims)
        },
        _ => panic!("not implemented"),
    }
}