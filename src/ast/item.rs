use super::{BlockStmt, Type, UseStmt}; // 导入兄弟模块中的类型
use nyanc_core::tokens::Token;

/// 代表文件中的一个顶层项目
#[derive(Debug, Clone)]
pub enum Item {
    Use(UseStmt),
    Function(FunctionDef),
    Struct(StructDef),
}

/// `fun add(a: int) -> int { ... }`
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: Token,
    pub params: Vec<Param>,
    pub return_type: Option<Type>,
    pub body: BlockStmt,
}

/// `struct Point { x: int, y: int }`
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: Token,
    pub fields: Vec<Param>, // 结构体的字段和函数参数结构相同
}

/// 函数参数或结构体字段 `name: type`
#[derive(Debug, Clone)]
pub struct Param {
    pub name: Token,
    pub param_type: Type,
}