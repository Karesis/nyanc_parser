use nyanc_core::tokens::Token;

/// 代表一个限定路径，例如 `std::io` 或 `self::utils`
#[derive(Debug, Clone)]
pub struct Path {
    /// 路径由多个段组成，每个段都是一个 Token (Identifier, Self_, etc.)
    pub segments: Vec<Token>,
}

/// `use` 语句的核心，代表导入的“树”状结构
#[derive(Debug, Clone)]
pub enum UseTree {
    /// 导入一个简单的路径，可能带有一个别名。
    /// e.g., `math` or `mathmatical as math`
    Simple { path: Path, alias: Option<Token> },
    
    /// 导入一个分组。
    /// e.g., `{math, ops}`
    Group { items: Vec<UseTree> },
    
    /// 导入所有内容（通配符）。
    /// e.g., `*`
    Wildcard { star_token: Token },
}

/// `use utils::math;`
/// e.g., `use utils::{math, ops};`
#[derive(Debug, Clone)]
pub struct UseStmt {
    pub use_keyword: Token, // 'use' 关键字本身
    /// 路径的前缀，对于 `use a::b::{c, d}` 来说，前缀是 `a::b`
    pub prefix: Option<Path>,
    /// 导入的树状结构主体
    pub tree: UseTree,
}