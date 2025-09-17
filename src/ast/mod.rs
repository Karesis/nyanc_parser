// 声明子模块
pub mod expr;
pub mod item;
pub mod stmt;
pub mod ty;
pub mod use_decl;

// 重新导出所有子模块的公共内容，方便外部使用
pub use expr::*;
pub use item::*;
pub use stmt::*;
pub use ty::*;
pub use use_decl::*;

/// 代表一个被解析的源文件（一个模块）
#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Item>,
}
