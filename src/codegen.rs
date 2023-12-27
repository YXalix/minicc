use crate::{parse::{NodeType, Node, Function}, util::align_to};




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
fn assign_lvar_offsets(program: &mut Vec<Function>) {
    for func in program {
        let mut offset = 0;
        func.variables.iter_mut().rev().for_each(|var| {
            offset += 8;
            var.offset = -offset;
        });
        func.stack_size = align_to(offset, 16);

        offset = 0;
        func.params.iter_mut().for_each(|var| {
            offset += 8;
            var.offset = -offset;
        });


    }
}

// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存中
fn gen_lval(nd: &Box<Node>, func: &Function) {
    match nd.kind {
        NodeType::NdVar => {
            let var_index = nd.var.unwrap();
            let offset = func.variables[var_index].offset;
            println!("  # 获取变量{}的栈内地址为{}(fp)", func.variables[var_index].name, offset);
            println!("  addi a0, fp, {}", offset);
            return;
        },
        NodeType::NdDeref => {
            genexpr(nd.lhs.as_ref().unwrap(), func);
        },
        _ => panic!("not an lvalue"),
    }
}


fn genexpr(nd: &Box<Node>, func: &Function) {
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
            gen_lval(nd, func);
            println!("  # 将变量{}的值加载到a0中", func.variables[nd.var.unwrap()].name);
            println!("  ld a0, 0(a0)");
            return;
        },
        NodeType::NdAssign => {
            gen_lval(nd.lhs.as_ref().unwrap(), func);
            push();
            genexpr(nd.rhs.as_ref().unwrap(), func);
            pop("a1");
            println!("  # 将a0的值, 写入到a1中存放的地址");
            println!("  sd a0, 0(a1)");
            return;
        },
        NodeType::NdAddr => {
            gen_lval(nd.lhs.as_ref().unwrap(), func);
            return;
        },
        NodeType::NdDeref => {
            genexpr(nd.lhs.as_ref().unwrap(), func);
            println!("  ld a0, 0(a0)");
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

fn genstmt(nd: &Box<Node>, func: &Function) {
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
            println!("  j .L.return.{}", func.funcname);
            return;
        }
        _ => panic!("unexpected node type: {:?}", nd.kind),
    }
}


pub fn codegen(program: &mut Vec<Function>) {
    assign_lvar_offsets(program);
    // 为每个函数单独生成代码
    for func in program {
        println!("  # 定义全局{}段", func.funcname);
        println!(".globl {}", func.funcname);
        println!("\n# ====={}段开始================", func.funcname);
        println!("{}:", func.funcname);
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
            println!("  sd {}, {}(fp)\n", ARG_REG[i], param.offset);
        });


        // 生成语句的代码
        println!("\n# ====={}段主体===============", func.funcname);
        func.body.iter().for_each(|nd| {
            genstmt(nd, func);
            assert_eq!(unsafe { DEPTH }, 0);
        });

        // Epilogue，后语
        // 输出return段标签
        println!("\n# ====={}段结束===============", func.funcname);
        println!(".L.return.{}:", func.funcname);
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