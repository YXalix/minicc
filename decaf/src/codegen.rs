use crate::{ir::{IrProg, IrStmt}, ast::{UnaryOp, BinaryOp}};

// 用于函数参数的寄存器们
const ARG_REG: [&str; 6] = ["a0", "a1", "a2", "a3", "a4", "a5"];

pub fn write_asm(p: &IrProg) {
    for g in &p.globs {
        println!(".data");
        println!("{}:", g.0);
        match g.1 {
        Ok(init) => println!("  .word {}", init),
        Err(size) => println!("  .zero {}", size * 4),
        }
        println!("");
    }
    for f in &p.funcs {
        println!(".global {}", f.name);
        println!(".text");
        println!("{}:", f.name);
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
        println!("  addi sp, sp, -16");
        println!("  sd ra, 8(sp)");
        // 将fp寄存器压栈, 保存fp的值
        println!("  sd fp, 0(sp)");
        // 将sp写入fp
        println!("  mv fp, sp");

        // 偏移量为为实际变量所用的栈大小
        let offset = f.var_cnt * 8;
        // 将sp向下移动offset
        println!("  addi sp, sp, -{}", offset);

        for s in &f.stmts {
            println!("  # {:?}", s); // 输出一条注释，表示下面的汇编对应IR中的什么指令，方便调试
            match s {
                IrStmt::Mul(a, x) => {
                    println!("  mul {}, {}, x{}", ARG_REG[*a as usize], ARG_REG[*a as usize], x);
                },
                IrStmt::StoreParam(x) => {
                    println!("  sd {}, {}(fp)\n", ARG_REG[*x as usize], (x + 1) * 8 );
                }
                IrStmt::Num(x) => {
                    println!("  li a0, {}", x); // 将常数值保存到t0中
                }
                IrStmt::Unary(op) => {
                    match op {
                        UnaryOp::Neg => {
                            println!("  neg a0, a0");
                        }
                        UnaryOp::Deref => {
                            println!("  lw a0, 0(a0)");
                        }
                        UnaryOp::AddrOf => {
                            println!("  addi a0, fp, {}", 8);
                        }
                        UnaryOp::Plus => {
                            println!("  mv a0, a0");
                        }
                    }
                }

                IrStmt::Binary(op) => {
                    match op {
                        BinaryOp::Add => println!("  add a0, a0, a1"),
                        BinaryOp::Sub => println!("  sub a0, a0, a1"),
                        BinaryOp::Mul => println!("  mul a0, a0, a1"),
                        BinaryOp::Div => println!("  div a0, a0, a1"),
                        BinaryOp::Eq => {
                            println!("  xor a0, a0, a1");
                            println!("  seqz a0, a0");
                        },
                        BinaryOp::Ne => {
                            println!("  xor a0, a0, a1");
                            println!("  snez a0, a0");
                        },
                        BinaryOp::Lt => println!("  slt a0, a0, a1"),
                        BinaryOp::Le => {
                            println!("  slt a0, a1, a0");
                            println!("  xori a0, a0, 1");
                        },
                        _ => todo!(),
                    }
                },
                IrStmt::LocalAddr(x) => {
                    let addr = 8 + (f.var_cnt - x - 1) * 8; 
                    println!("  addi a0, fp, -{}", addr); 
                },
                IrStmt::GlobAddr(x) => {
                    println!("  la a0, {}", p.globs[*x as usize].0);
                },
                IrStmt::Load => {
                    // 加载a0指向的值
                    println!("  ld a0, 0(a0)");
                },
                IrStmt::Store => {
                    // 将a0的值保存到a1指向的地址
                    println!("  ld {}, 0(sp)", ARG_REG[1]);
                    println!("  addi sp, sp, 8");
                    println!("  sd a0, 0(a1)");
                },
                IrStmt::Label(c) => {
                    println!(".L{}:", c);
                },
                IrStmt::Bz(c) => {
                    println!("  beqz a0, .L{}", c);
                },
                IrStmt::Bnz(c) => {
                    println!("  bnez a0, .L{}", c);
                },
                IrStmt::Jump(c) => {
                    println!("  j .L{}", c);
                },
                IrStmt::Call(c) => {
                    println!("  call {}", c);
                }
                IrStmt::Swap => todo!(),
                IrStmt::Push => {
                    println!("  addi sp, sp, -8");
                    println!("  sd a0, 0(sp)");
                },
                IrStmt::Pop(reg_id) => {
                    println!("  ld {}, 0(sp)", ARG_REG[*reg_id as usize]);
                    println!("  addi sp, sp, 8");
                },
                IrStmt::Ret => {
                    println!("  j .L.return.{}", f.name);
                },
            }
        }

        // Epilogue, 后言
        println!(".L.return.{}:", f.name);
        // 将fp的值改写回sp
        println!("  mv sp, fp");
        // 将最早fp保存的值弹栈，恢复fp。
        println!("  ld fp, 0(sp)");
        // 将ra寄存器弹栈,恢复ra的值
        println!("  ld ra, 8(sp)");
        println!("  addi sp, sp, 16");
        println!("  ret");
    }
}