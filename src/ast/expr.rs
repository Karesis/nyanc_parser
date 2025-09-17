use nyanc_core::tokens::Token;

/// 表达式的枚举
#[derive(Debug, Clone)]
pub enum Expr {
    // a = 5
    Assignment(AssignExpr),
    // a + b
    Binary(BinaryExpr),
    // -a, !b
    Unary(UnaryExpr),
    // my_func(a, b)
    Call(CallExpr),
    // point.x
    MemberAccess(MemberAccessExpr),
    // Point { x: 1, y: 2 }
    StructInit(StructInitExpr),
    // 123, "hello", true
    Literal(LiteralExpr),
    // a, my_var
    Variable(VariableExpr),
    // (1 + 2)
    Grouping(GroupingExpr),
    // ()
    UnitLiteral,
}

/// `a = 5`
#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub target: Box<Expr>, // 被赋值的目标
    pub value: Box<Expr>,
}

/// `a + b`
#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

/// `-a`, `!b`
#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

/// `my_func(a, b)`
#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>, // 被调用的表达式（通常是变量或成员访问）
    pub args: Vec<Expr>,
}

/// `point.x`
#[derive(Debug, Clone)]
pub struct MemberAccessExpr {
    pub object: Box<Expr>,
    pub field: Token,
}

/// `Point { x: 1, y: 2 }`
#[derive(Debug, Clone)]
pub struct StructInitExpr {
    pub name: Token,
    pub fields: Vec<(Token, Expr)>, // (字段名, 字段值)
}

/// `123`, `"hello"`, `true`
#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub value: Token,
}

/// `a`, `my_var`
#[derive(Debug, Clone)]
pub struct VariableExpr {
    pub name: Token,
}

/// `(1 + 2)`
#[derive(Debug, Clone)]
pub struct GroupingExpr {
    pub expr: Box<Expr>,
}