use crate::parse::{NodeType, Node};

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


fn genexpr(nd: Box<Node>) {
    match nd.kind {
        NodeType::NdNum => {
            println!("  li a0, {}", nd.val);
            return;
        },
        NodeType::NdNeg => {
            genexpr(nd.lhs.unwrap());
            println!("  neg a0, a0");
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


pub fn codegen(programs: &Vec<Box<Node>>) {
    println!(".globl main");
    println!("main:");
    programs.iter().for_each(|nd| genstmt(nd.clone()));
    println!("  ret");
    assert_eq!(unsafe { DEPTH }, 0);
}