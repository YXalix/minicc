use crate::{parse::{NodeType, Node}, PROGRAM, util::align_to};
static mut DEPTH:i32  = 0;

fn push(){
    println!("  addi sp, sp, -8");
    println!("  sd a0, 0(sp)");
    unsafe { DEPTH += 1 } ;
}

fn pop(reg: &str){
    println!("  ld {}, 0(sp)", reg);
    println!("  addi sp, sp, 8");
    unsafe { DEPTH -= 1 };
}

// 根据变量的链表计算出偏移量
fn assign_lvar_offsets() {
    let mut offset = 0;
    PROGRAM.lock().unwrap().variables.iter_mut().for_each(|var| {
        offset += 8;
        var.offset = offset;
    });
    PROGRAM.lock().unwrap().stack_size = align_to(offset, 16);
}

// 计算给定节点的绝对地址
// 如果报错，说明节点不在内存中
fn gen_lval(nd: Box<Node>) {
    match nd.kind {
        NodeType::NdVar => {
            let var_index = nd.var.unwrap();
            let offset = PROGRAM.lock().unwrap().variables[var_index].offset;
            println!("  addi a0, fp, {}", -offset);
            return;
        },
        _ => panic!("not an lvalue"),
    }
}


fn genexpr(nd: Box<Node>) {
    match nd.kind {
        NodeType::NdNum => {
            println!("  li a0, {}", nd.val.unwrap());
            return;
        },
        NodeType::NdNeg => {
            genexpr(nd.lhs.unwrap());
            println!("  neg a0, a0");
            return;
        },
        NodeType::NdVar => {
            gen_lval(nd.clone());
            println!("  ld a0, 0(a0)");
            return;
        },
        NodeType::NdAssign => {
            gen_lval(nd.lhs.unwrap());
            push();
            genexpr(nd.rhs.unwrap());
            pop("a1");
            println!("  sd a0, 0(a1)");
            return;
        },
        _ => {},
    }
    genexpr(nd.rhs.unwrap());
    push();
    genexpr(nd.lhs.unwrap());
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

fn genstmt(nd: Box<Node>) {
    match nd.kind {
        NodeType::NdExprStmt => {
            genexpr(nd.lhs.unwrap());
            println!("  addi sp, sp, 8");
            return;
        },
        _ => panic!("unexpected node type: {:?}", nd.kind),
    }
}


pub fn codegen() {
    assign_lvar_offsets();
    println!(".globl main");
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
    println!("  addi sp, sp, -8");
    println!("  sd fp, 0(sp)");
    // 将sp写入fp
    println!("  mv fp, sp");

    // 26个字母*8字节=208字节，栈腾出208字节的空间
    println!("  addi sp, sp, -208\n");
    let body = PROGRAM.lock().unwrap().body.clone();
    body.iter().for_each(|nd| {
        genstmt(nd.clone());
        assert_eq!(unsafe { DEPTH }, 0);
    });
    // Epilogue，后语
    // 将fp的值改写回sp
    println!("  mv sp, fp");
    // 将最早fp保存的值弹栈，恢复fp。
    println!("  ld fp, 0(sp)");
    println!("  addi sp, sp, 8");
    // 返回
    println!("  ret");
}