use super::{Expr, Type};
use nyanc_core::tokens::Token;

/// 语句的枚举
#[derive(Debug, Clone)]
pub enum Stmt {
    // let a = 1;
    Let(LetStmt),
    // a: int = 1;
    Var(VarStmt),
    // return a + 1;
    Return(ReturnStmt),
    // { ... }
    Block(BlockStmt),
    // a = a + 1; or my_func();
    Expression(ExprStmt),
    // if condition { ... } else { ... }
    If(IfStmt),       
    // while condition { ... }
    While(WhileStmt), 
}

/// `let a = 1;`
#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: Token,
    pub value: Expr,
}

/// `a: int = 1;`
#[derive(Debug, Clone)]
pub struct VarStmt {
    pub name: Token,
    pub var_type: Type,
    pub value: Option<Expr>, // 初始值是可选的
}

/// `return a + 1;`
#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub keyword: Token, // "return" 关键字本身，用于错误报告
    pub value: Option<Expr>, // 允许 `return;`
}

/// `{ ... }`
#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

/// `a = a + 1;` or `my_func();`
#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: BlockStmt,
    /// else 分支是可选的，并且可以是另一个 if 语句（用于 else if）或一个代码块
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: BlockStmt,
}
