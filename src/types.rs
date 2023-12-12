

//
// 类型系统
//


// 类型种类
#[derive(Clone)]
pub enum TypeKind {
    TyInt, // 整数类型
    TyPtr, // 指针类型
}

#[derive(Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub ptr_to: Option<Box<Type>>,
}


impl Type {
    pub fn new(kind: TypeKind, ptr_to: Box<Type>) -> Box<Type> {
        Box::new(Type {
            kind: kind,
            ptr_to: Some(ptr_to),
        })
    }

    pub fn is_ptr(&self) -> bool {
        match self.kind {
            TypeKind::TyPtr => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self.kind {
            TypeKind::TyInt => true,
            _ => false,
        }
    }

}

pub fn pointer_to(base: Box<Type>) -> Box<Type> {
    Type::new(TypeKind::TyPtr, base)
}