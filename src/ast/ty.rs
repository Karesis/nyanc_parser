use nyanc_core::tokens::Token;

#[derive(Debug, Clone)]
pub enum Type {
    /// `int`, `bool`, `Point`
    Identifier { name: Token },
    /// `^int`, `^Point`
    Pointer { base: Box<Type> },
    /// `()` - 单元类型
    Unit, 
}