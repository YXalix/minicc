use std::sync::Mutex;

use crate::{parse::{NodeType, Node, Obj}, util::align_to, types::{Type, TypeKind}};
use lazy_static::lazy_static;

lazy_static! {
    static ref GLOBL: Mutex<Vec<Obj>> = Mutex::new(Vec::new());
}

static mut DEPTH:i32  = 0;

// 用于函数参数的寄存器们
const ARG_REG: [&str; 6] = ["a0", "a1", "a2", "a3", "a4", "a5"];


// 代码段计数
fn count() -> i32 {
    static mut I: i32 = 1;
    unsafe {
        I += 1;
        I
    }
}


fn push(){
    println!("  # 压栈, 将a0的值存入栈顶");
    println!("  addi sp, sp, -8");
    println!("  sd a0, 0(sp)");
    unsafe { DEPTH += 1 } ;
}

fn pop(reg: &str){
    println!("  # 出栈, 将栈顶的值存入{}", reg);
    println!("  ld {}, 0(sp)", reg);
    println!("  addi sp, sp, 8");
    unsafe { DEPTH -= 1 };
}

// 根据变量的链表计算出偏移量
fn assign_lvar_offsets(program: &mut Vec<Obj>) {
    for func in program {
        let mut offset:i32 = 0;
        func.locals.iter_mut().rev().for_each(|var| {
            // 每个变量分配空间
            offset += var.ty.as_ref().unwrap().size as i32;
            var.offset = - offset;
        });
        func.stack_size = align_to(offset, 16);

        offset = 0;
        func.params.iter_mut().for_each(|var| {
            offset += var.ty.as_ref().unwrap().size as i32;
            var.offset = - offset;
        });


    }
}

// 加载a0指向的值
fn load(ty: &Box<Type>){
    match ty.kind {
        TypeKind::TyArray => {},
        _ => {
            println!("  # 加载a0指向的值");
            println!("  ld a0, 0(a0)");
        },
    }
}

// 将栈顶值(为一个地址)存入a0
fn store(ty: &Box<Type>){
    pop("a1");
    println!("  # 将a0的值, 写入到a1中存放的地址");
    if ty.size == 1 {
        println!("  sb a0, 0(a1)");
    } else {
        println!("  sd a0, 0(a1)");
    }
}

// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存中
fn gen_addr(nd: &Box<Node>, func: &Obj) {
    match nd.kind {
        NodeType::NdVar => {
            let var_index = nd.var.unwrap();
            // 全局变量
            if nd.is_gobal {
                let offset = GLOBL.lock().unwrap()[var_index].offset;
                println!("  # 获取全局变量{}的栈内地址为{}", GLOBL.lock().unwrap()[var_index].name, offset);
                println!("  la a0, {}", GLOBL.lock().unwrap()[var_index].name);
                return;
            }
            let offset = func.locals[var_index].offset;
            println!("  # 获取变量{}的栈内地址为{}(fp)", func.locals[var_index].name, offset);
            println!("  addi a0, fp, {}", offset);
            return;
        },
        NodeType::NdDeref => {
            genexpr(nd.lhs.as_ref().unwrap(), func);
        },
        _ => panic!("not an lvalue"),
    }
}


fn genexpr(nd: &Box<Node>, func: &Obj) {
    match nd.kind {
        NodeType::NdNum => {
            println!("  # 将{}加载到a0中", nd.val.unwrap());
            println!("  li a0, {}", nd.val.unwrap());
            return;
        },
        NodeType::NdNeg => {
            genexpr(nd.lhs.as_ref().unwrap(),func);
            println!("  # 对a0值进行取反");
            println!("  neg a0, a0");
            return;
        },
        NodeType::NdVar => {
            gen_addr(nd, func);
            // load(&func.variables[nd.var.unwrap()].ty.as_ref().unwrap());
            load(&nd.ty.as_ref().unwrap());
            return;
        },
        NodeType::NdAssign => {
            gen_addr(nd.lhs.as_ref().unwrap(), func);
            push();
            genexpr(nd.rhs.as_ref().unwrap(), func);
            store(nd.ty.as_ref().unwrap());
            return;
        },
        NodeType::NdAddr => {
            gen_addr(nd.lhs.as_ref().unwrap(), func);
            return;
        },
        NodeType::NdDeref => {
            genexpr(nd.lhs.as_ref().unwrap(), func);
            load(&nd.ty.as_ref().unwrap());
            return;
        },
        // 函数调用
        NodeType::NdFuncCall => {
            // 计算所有参数的值，正向压栈
            nd.args.iter().for_each(|arg| {
                genexpr(arg, func);
                push();
            });
            // 将所有参数的值，反向弹栈，存入对应的寄存器中
            for i in 0..nd.args.len() {
                pop(ARG_REG[nd.args.len() - i - 1]);
            }
            println!("\n  # 调用{}函数", nd.funcname.unwrap());
            println!("  call {}", nd.funcname.unwrap());
            return;
        },
        _ => {},
    }
    genexpr(nd.rhs.as_ref().unwrap(), func);
    push();
    genexpr(nd.lhs.as_ref().unwrap(), func);
    pop("a1");
    match nd.kind {
        NodeType::NdAdd => println!("  add a0, a0, a1"),
        NodeType::NdSub => println!("  sub a0, a0, a1 "),
        NodeType::NdMul => println!("  mul a0, a0, a1"),
        NodeType::NdDiv => println!("  div a0, a0, a1"),
        NodeType::NdEq => {
            println!("  xor a0, a0, a1");
            println!("  seqz a0, a0");
        },
        NodeType::NdNe => {
            println!("  xor a0, a0, a1");
            println!("  snez a0, a0");
        },
        NodeType::NdLt => println!("  slt a0, a0, a1"),
        NodeType::NdLe => {
            println!("  slt a0, a1, a0");
            println!("  xori a0, a0, 1");
        },
        _ => panic!("unexpected node type: {:?}", nd.kind),
    }
}

fn genstmt(nd: &Box<Node>, func: &Obj) {
    match nd.kind {
        NodeType::NdIf => {
            // 代码段计数
            let c = count();
            println!("\n# =====分支语句{}==============", c);
            // 生成条件内语句
            genexpr(&nd.cond.as_ref().unwrap(), func);
            // 判断结果是否为0，为0则跳转到else标签
            println!("  beqz a0, .L.else.{}", c);
            // 生成符合条件后的语句
            genstmt(&nd.then.as_ref().unwrap(), func);
            // 执行完后跳转到if语句后面的语句
            println!("  j .L.end.{}", c);
            // else代码块，else可能为空，故输出标签
            println!(".L.else.{}:", c);
            // 生成不符合条件后的语句
            if let Some(els) = &nd.els {
                genstmt(els, func);
            }
            // 结束if语句，继续执行后面的语句
            println!(".L.end.{}:", c);
            return;
        },
        // 生成for或while循环语句
        NodeType::NdFor => {
            // 代码段计数
            let c = count();
            println!("\n# =====循环语句{}==============", c);
            // 生成for循环内的初始化语句
            if let Some(init) = &nd.init {
                genstmt(init, func);
            }
            println!(".L.begin.{}:", c);
            // 生成for循环内的条件语句
            if let Some(cond) = &nd.cond {
                // 生成条件循环语句
                genexpr(cond, func);
                // 判断结果是否为0，为0则跳转到结束部分
                println!("  beqz a0, .L.end.{}", c);
            } else {
                // 如果没有条件语句，则默认为1
                println!("  li a0, 1");
            }
            // 生成for循环内的语句
            genstmt(&nd.then.as_ref().unwrap(), func);
            // 生成for循环内的迭代语句
            if let Some(inc) = &nd.inc {
                genexpr(inc, func);
            }
            // 跳转到for循环内的条件语句
            println!("  j .L.begin.{}", c);
            // 结束for循环，继续执行后面的语句
            println!(".L.end.{}:", c);
            return;
        },
        NodeType::NdBloc => {
            nd.body.iter().for_each(|nd| {
                genstmt(nd, func);
            });
            return;
        },
        NodeType::NdExprStmt => {
            genexpr(&nd.lhs.as_ref().unwrap(), func);
            return;
        },
        NodeType::NdReturn => {
            genexpr(&nd.lhs.as_ref().unwrap(), func);
            println!("  j .L.return.{}", func.name);
            return;
        }
        _ => panic!("unexpected node type: {:?}", nd.kind),
    }
}


pub fn emit_text(program: &mut Vec<Obj>) {
    // 为每个函数单独生成代码
    for func in program {
        if func.is_function == false {
            continue;
        }
        println!("  # 定义全局{}段", func.name);
        println!(".globl {}", func.name);
        println!("  # 代码段标签");
        println!(".text");
        println!("\n# ====={}段开始================", func.name);
        println!("{}:", func.name);
        // 栈布局
        //-------------------------------// sp
        //              ra
        //-------------------------------// ra = sp-8
        //              fp
        //-------------------------------// fp = sp-16
        //             变量
        //-------------------------------// sp = sp-16-StackSize
        //           表达式计算
        //-------------------------------//
        
        // Prologue, 前言
        // 将ra寄存器压栈,保存ra的值
        println!("  # 将ra寄存器压栈,保存ra的值");
        println!("  addi sp, sp, -16");
        println!("  sd ra, 8(sp)");
        // 将fp压入栈中，保存fp的值
        println!("  # 将fp压栈, fp属于“被调用者保存”的寄存器, 需要恢复原值");
        println!("  sd fp, 0(sp)");
        // 将sp写入fp
        println!("  # 将sp的值写入fp");
        println!("  mv fp, sp");
    
        // 偏移量为实际变量所用的栈大小
        println!("  # 为局部变量分配空间");
        println!("  addi sp, sp, {}", -func.stack_size);

        func.params.iter().enumerate().for_each(|(i,param)|{
            println!("  # 将{}寄存器的值存入{}的栈地址", ARG_REG[i], param.name);
            if param.ty.as_ref().unwrap().size == 1 {
                println!("  sb {}, {}(fp)\n", ARG_REG[i], param.offset);
            } else {
                println!("  sd {}, {}(fp)\n", ARG_REG[i], param.offset);
            }
        });


        // 生成语句的代码
        println!("\n# ====={}段主体===============", func.name);
        func.body.iter().for_each(|nd| {
            genstmt(nd, func);
            assert_eq!(unsafe { DEPTH }, 0);
        });

        // Epilogue，后语
        // 输出return段标签
        println!("\n# ====={}段结束===============", func.name);
        println!(".L.return.{}:", func.name);
        // 将fp的值改写回sp
        println!("  mv sp, fp");
        // 将最早fp保存的值弹栈，恢复fp。
        println!("  ld fp, 0(sp)");
        // 将ra寄存器弹栈,恢复ra的值
        println!("  # 将ra寄存器弹栈,恢复ra的值");
        println!("  ld ra, 8(sp)");
        println!("  addi sp, sp, 16");
        // 返回
        println!("  ret");
    }
}

fn emit_data(program: &mut Vec<Obj>) {
    println!("\n# =====数据段开始================");
    for func in program {
        if func.is_function {
            continue;
        }
        println!("  # 数据段标签");
        println!(".data");
        println!("  .globl {}", func.name);
        println!("  # 全局变量{}", func.name);
        println!("{}:", func.name);
        println!("  # 零填充{}位", func.ty.as_ref().unwrap().size);
        println!("  .zero {}", func.ty.as_ref().unwrap().size);
    }
    println!("\n# =====数据段结束================");
}

pub fn codegen(program: &mut Vec<Obj>) {
    GLOBL.lock().unwrap().clone_from(program);
    assign_lvar_offsets(program);
    emit_data(program);
    emit_text(program);
}