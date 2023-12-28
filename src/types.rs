

//
// 类型系统
//


// 类型种类
#[derive(Clone)]
#[derive(PartialEq)]
pub enum TypeKind {
    TyInt, // 整数类型
    TyPtr, // 指针类型
    TyFunc, // 函数类型
    TyArray, // 数组类型
}

#[derive(Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub base: Option<Box<Type>>,
    pub return_ty: Option<Box<Type>>, // 函数返回的类型
    pub params: Vec<Box<Type>>, // 形参
    pub tok: Option<usize>,

    pub array_len: Option<usize>, // 数组长度
    pub size: usize, // 大小, sizeof 的结果
}


impl Type {
    pub fn new(kind: TypeKind, base: Option<Box<Type>>) -> Box<Type> {
        Box::new(Type {
            kind: kind,
            base: base,
            return_ty: None,
            params: Vec::new(),
            tok: None,
            array_len: None,
            size: 8,
        })
    }

    pub fn new_int_type() -> Box<Type> {
        let mut temp = Type::new(TypeKind::TyInt, None);
        temp.size = 8;
        temp
    }

    pub fn is_ptr(&self) -> bool {
        match self.kind {
            TypeKind::TyPtr => true,
            TypeKind::TyArray => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self.kind {
            TypeKind::TyInt => true,
            _ => false,
        }
    }

    pub fn array_of(base: Box<Type>, len: usize) -> Box<Type> {
        let size = base.size;
        let mut ty = Type::new(TypeKind::TyArray, Some(base));
        ty.array_len = Some(len);
        ty.size = size * len;
        ty
    }

}

pub fn pointer_to(base: Box<Type>) -> Box<Type> {
    Type::new(TypeKind::TyPtr, Some(base))
}

