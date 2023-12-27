

//
// 类型系统
//


// 类型种类
#[derive(Clone)]
pub enum TypeKind {
    TyInt, // 整数类型
    TyPtr, // 指针类型
    YyFunc, // 函数类型
}

#[derive(Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub ptr_to: Option<Box<Type>>,
    pub return_ty: Option<Box<Type>>, // 函数返回的类型
    pub params: Vec<Box<Type>>, // 形参
    pub tok: Option<usize>,
}


impl Type {
    pub fn new(kind: TypeKind, ptr_to: Box<Type>) -> Box<Type> {
        Box::new(Type {
            kind: kind,
            ptr_to: Some(ptr_to),
            return_ty: None,
            params: Vec::new(),
            tok: None,
        })
    }

    pub fn new_int_type() -> Box<Type> {
        Box::new(Type {
            kind: TypeKind::TyInt,
            ptr_to: None,
            return_ty: None,
            params: Vec::new(),
            tok: None,
        })
    }

    pub fn new_func_type(return_ty: Box<Type>) -> Box<Type> {
        Box::new(Type {
            kind: TypeKind::YyFunc,
            ptr_to: None,
            return_ty: Some(return_ty),
            params: Vec::new(),
            tok: None,
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

