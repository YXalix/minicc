use crate::{parse::{NodeType, Node}, PROGRAM, util::align_to};
static mut DEPTH:i32  = 0;

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
fn assign_lvar_offsets() {
    let mut offset = 0;
    PROGRAM.write().unwrap().variables.iter_mut().rev().for_each(|var| {
        offset += 8;
        var.offset = -offset;
    });
    PROGRAM.write().unwrap().stack_size = align_to(offset, 16);
}

// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存中
fn gen_lval(nd: &Box<Node>) {
    match nd.kind {
        NodeType::NdVar => {
            let var_index = nd.var.unwrap();
            let offset = PROGRAM.read().unwrap().variables[var_index].offset;
            println!("  # 获取变量{}的栈内地址为{}(fp)", PROGRAM.read().unwrap().variables[var_index].name, offset);
            println!("  addi a0, fp, {}", offset);
            return;
        },
        NodeType::NdDeref => {
            genexpr(nd.lhs.as_ref().unwrap());
        },
        _ => panic!("not an lvalue"),
    }
}


fn genexpr(nd: &Box<Node>) {
    match nd.kind {
        NodeType::NdNum => {
            println!("  # 将{}加载到a0中", nd.val.unwrap());
            println!("  li a0, {}", nd.val.unwrap());
            return;
        },
        NodeType::NdNeg => {
            genexpr(nd.lhs.as_ref().unwrap());
            println!("  # 对a0值进行取反");
            println!("  neg a0, a0");
            return;
        },
        NodeType::NdVar => {
            gen_lval(nd);
            println!("  # 将变量{}的值加载到a0中", PROGRAM.read().unwrap().variables[nd.var.unwrap()].name);
            println!("  ld a0, 0(a0)");
            return;
        },
        NodeType::NdAssign => {
            gen_lval(nd.lhs.as_ref().unwrap());
            push();
            genexpr(nd.rhs.as_ref().unwrap());
            pop("a1");
            println!("  # 将a0的值, 写入到a1中存放的地址");
            println!("  sd a0, 0(a1)");
            return;
        },
        NodeType::NdAddr => {
            gen_lval(nd.lhs.as_ref().unwrap());
            return;
        },
        NodeType::NdDeref => {
            genexpr(nd.lhs.as_ref().unwrap());
            println!("  ld a0, 0(a0)");
            return;
        },
        _ => {},
    }
    genexpr(nd.rhs.as_ref().unwrap());
    push();
    genexpr(nd.lhs.as_ref().unwrap());
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

fn genstmt(nd: &Box<Node>) {
    match nd.kind {
        NodeType::NdIf => {
            // 代码段计数
            let c = count();
            println!("\n# =====分支语句{}==============", c);
            // 生成条件内语句
            genexpr(&nd.cond.as_ref().unwrap());
            // 判断结果是否为0，为0则跳转到else标签
            println!("  beqz a0, .L.else.{}", c);
            // 生成符合条件后的语句
            genstmt(&nd.then.as_ref().unwrap());
            // 执行完后跳转到if语句后面的语句
            println!("  j .L.end.{}", c);
            // else代码块，else可能为空，故输出标签
            println!(".L.else.{}:", c);
            // 生成不符合条件后的语句
            if let Some(els) = &nd.els {
                genstmt(els);
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
                genstmt(init);
            }
            println!(".L.begin.{}:", c);
            // 生成for循环内的条件语句
            if let Some(cond) = &nd.cond {
                // 生成条件循环语句
                genexpr(cond);
                // 判断结果是否为0，为0则跳转到结束部分
                println!("  beqz a0, .L.end.{}", c);
            } else {
                // 如果没有条件语句，则默认为1
                println!("  li a0, 1");
            }
            // 生成for循环内的语句
            genstmt(&nd.then.as_ref().unwrap());
            // 生成for循环内的迭代语句
            if let Some(inc) = &nd.inc {
                genexpr(inc);
            }
            // 跳转到for循环内的条件语句
            println!("  j .L.begin.{}", c);
            // 结束for循环，继续执行后面的语句
            println!(".L.end.{}:", c);
            return;
        },
        NodeType::NdBloc => {
            nd.body.iter().for_each(|nd| {
                genstmt(nd);
            });
            return;
        },
        NodeType::NdExprStmt => {
            genexpr(&nd.lhs.as_ref().unwrap());
            return;
        },
        NodeType::NdReturn => {
            genexpr(&nd.lhs.as_ref().unwrap());
            println!("  j .L.return");
            return;
        }
        _ => panic!("unexpected node type: {:?}", nd.kind),
    }
}


pub fn codegen() {
    assign_lvar_offsets();
    println!("  # 定义全局main段");
    println!(".globl main");
    println!("\n# =====程序开始================");
    println!("main:");
    // 栈布局
    //-------------------------------// sp
    //              fp                  fp = sp-8
    //-------------------------------// fp
    //              'a'                 fp-8
    //              'b'                 fp-16
    //              ...
    //              'z'                 fp-208
    //-------------------------------// sp=sp-8-208
    //           表达式计算
    //-------------------------------//

    // Prologue, 前言
    // 将fp压入栈中，保存fp的值
    println!("  # 将fp压栈, fp属于“被调用者保存”的寄存器, 需要恢复原值");
    println!("  addi sp, sp, -8");
    println!("  sd fp, 0(sp)");
    // 将sp写入fp
    println!("  # 将sp的值写入fp");
    println!("  mv fp, sp");

    println!("  # 为局部变量分配空间");
    println!("  addi sp, sp, {}", -PROGRAM.read().unwrap().stack_size);
    let body = &PROGRAM.read().unwrap().body;

    println!("\n# =====程序主体===============");
    body.iter().for_each(|nd| {
        genstmt(nd);
        assert_eq!(unsafe { DEPTH }, 0);
    });

    println!("\n# =====程序结束===============");
    // Epilogue，后语
    // 输出return段标签
    println!(".L.return:");
    // 将fp的值改写回sp
    println!("  mv sp, fp");
    // 将最早fp保存的值弹栈，恢复fp。
    println!("  ld fp, 0(sp)");
    println!("  addi sp, sp, 8");
    // 返回
    println!("  ret");
}