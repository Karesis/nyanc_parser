// parser/src/lib.rs

pub mod ast;
mod ast_printer;
#[cfg(test)]
mod tests;

use ast::*;
use lexer::Lexer;
use nyanc_core::tokens::{Token, TokenType};
use nyanc_core::errors::{CompilerError, ParserError, ParserErrorKind};
use nyanc_core::Span;
use reporter::DiagnosticsEngine;

// 优先级，用于pratt解析
#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
enum Precedence {
    None,
    Assignment, // =
    Sum,        // + -
    Product,    // * /
    Prefix,     // -X or !X
    Call,       // myFunction(...)
    MemberAccess, // object.field
}

/// Parser 负责将 Token 流转换为 AST。
pub struct Parser<'a> {
    lexer: Lexer<'a>, 
    diagnostics: &'a DiagnosticsEngine,

    /// 解析器当前正在处理的 Token
    current: Token,
    /// 预读的下一个 Token
    peek: Token,
}

// 这是 Parser 的主实现块
impl<'a> Parser<'a> {
    /// 创建一个新的 Parser 实例。
    pub fn new(mut lexer: Lexer<'a>, diagnostics: &'a DiagnosticsEngine) -> Self {
        // 为了初始化，我们创建两个临时的 EOF Token
        // 实际的 Token 将在下面的 advance() 调用中被加载
        let eof_span = Span { file_id: 0, start: 0, end: 0 }; // 假设 file_id, 实际应从 lexer 获取
        let eof_token = Token { kind: TokenType::Eof, lexeme: "".to_string(), span: eof_span };
        
        let mut parser = Self {
            lexer,
            diagnostics,
            current: eof_token.clone(),
            peek: eof_token,
        };

        // “启动引擎”：调用两次 advance 来填充 current 和 peek 字段
        parser.advance();
        parser.advance();

        parser
    }

    /// `parse` 是 Parser 的主入口点。
    /// 它会持续解析顶层项目（Item），直到文件末尾，
    /// 最后返回一个代表整个文件的 `Module` AST 节点。
    pub fn parse(&mut self) -> Module {
        let mut items = Vec::new();

        // 主循环，持续直到文件末尾
        while !self.is_at_end() {
            // --- 核心修复点 ---
            // 在尝试解析一个 Item 之前，先消耗掉所有连续的换行符（即空行）
            while self.check(TokenType::Newline) {
                self.advance();
            }

            // 如果跳过空行后就到了文件末尾，就跳出主循环
            if self.is_at_end() {
                break;
            }
            
            // 现在，我们可以确定 self.current 是一个有意义的 Token，
            // 于是我们开始解析顶层项目。
            if let Some(item) = self.parse_item() {
                items.push(item);
            }
        }
        Module { items }
    }

    // -----------------------------------------------------------------
    // --- 核心工具箱 (The Core Toolkit) ---
    // -----------------------------------------------------------------
    // 以下是 Parser 最基础、最核心的辅助函数，它们是所有解析逻辑的基石。

    /// 消耗当前 Token，并让解析器前进一个位置。
    /// 这是整个解析器向前移动的唯一方式。
    fn advance(&mut self) {
        // 使用 std::mem::replace，这是一个非常高效的技巧：
        // 1. 将 self.peek 中的 Token 移动到 self.current 中。
        // 2. 从 self.lexer 中取出下一个 Token，放入 self.peek 中。
        // 3. 如果 lexer 已经结束，就用 Eof Token 填充。
        let next_token = self.lexer.next().unwrap_or_else(|| {
            // 创建一个文件末尾的 Token
            let span = self.current.span; // 使用最后一个有效 Token 的位置
            Token { kind: TokenType::Eof, lexeme: "".to_string(), span }
        });
        self.current = std::mem::replace(&mut self.peek, next_token);
    }

    /// 检查当前 Token 是否是指定的类型。
    fn check(&self, kind: TokenType) -> bool {
        self.current.kind == kind
    }

    /// 检查当前 Token 是否是文件末尾（Eof）。
    fn is_at_end(&self) -> bool {
        self.check(TokenType::Eof)
    }

    /// 断言当前 Token 必须是指定的类型。
    /// - 如果是，则消耗它并返回。
    /// - 如果不是，则报告一个语法错误。
    /// 这是处理语法错误的主要入口。
    fn consume(&mut self, expected_kind: TokenType, message: &str) -> Option<Token> {
        if self.check(expected_kind) {
            // 在返回之前，先 advance
            let token_to_return = self.current.clone();
            self.advance();
            Some(token_to_return)
        } else {
            // 报告错误时，我们看的是当前的 Token，而不是 peek
            let error = ParserError::new(
                ParserErrorKind::UnexpectedToken {
                    expected: message.to_string(),
                    found: self.current.kind,
                },
                self.current.span,
            );
            self.diagnostics.add_error(CompilerError::Parser(error));
            None
        }
    }

    /// 错误恢复机制。
    /// 当遇到语法错误时，我们不能就此放弃，而是应该尝试跳过一些 Token，
    /// 直到找到一个可以安全地重新开始解析的地方（通常是下一个语句的开头）。
    fn synchronize(&mut self) {
        self.advance(); // 消耗掉引发错误的那个 Token

        while !self.is_at_end() {
            // 如果前一个 Token 是换行符，说明可能是一个语句的结尾，可以尝试重新开始
            // (这里的逻辑可以更完善，比如检查前一个 Token)
            
            // 查看当前 Token 是否是一个新的顶层项目或语句的开头
            match self.current.kind {
                TokenType::Fun | TokenType::Struct | TokenType::Let | TokenType::Use => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }

    // pratt解析相关

    /// 根据 Token 类型查询其作为中缀操作符时的优先级。
    fn get_infix_precedence(kind: TokenType) -> Precedence {
        match kind {
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Star | TokenType::Slash => Precedence::Product,
            TokenType::Equal => Precedence::Assignment,
            TokenType::LeftParen => Precedence::Call,
            TokenType::Dot => Precedence::MemberAccess,
            _ => Precedence::None,
        }
    }

    /// 根据 Token 类型查询其作为前缀操作符时的优先级。
    fn get_prefix_precedence(kind: TokenType) -> Precedence {
        match kind {
            TokenType::Minus => Precedence::Prefix,
            // 未来可以加入 ! 等前缀操作符
            _ => Precedence::None,
        }
    }

    /// 解析一个顶层项目（use 声明、函数定义、结构体定义）。
    /// 这是所有顶层解析的“分派中心”。
    fn parse_item(&mut self) -> Option<Item> {
        // 直接匹配当前 Token
        match self.current.kind {
            TokenType::Use => self.parse_use_statement(),
            TokenType::Fun => self.parse_function_definition(),
            TokenType::Struct => self.parse_struct_definition(),

            // --- 新增：明确禁止顶层语句 ---
            TokenType::Let | TokenType::Identifier | TokenType::If | TokenType::While | TokenType::Return | TokenType::LeftBrace => {
                let err = ParserError::new(
                    ParserErrorKind::NoToplevelStatements,
                    self.current.span,
                );
                self.diagnostics.add_error(CompilerError::Parser(err));
                self.synchronize(); // 尝试跳过这个非法的语句
                None
            },

            TokenType::Eof => None, // 到达文件末尾，正常结束
            
            _ => { // 报告错误
                let error = ParserError::new(
                    ParserErrorKind::UnexpectedToken {
                        expected: "a top-level item (like 'fun', 'struct', or 'use')".to_string(),
                        found: self.current.kind,
                    },
                    self.current.span,
                );
                self.diagnostics.add_error(CompilerError::Parser(error));
                self.synchronize(); // 尝试恢复
                None
            }
        }
    }

    /// 解析一个 `use` 声明。
    /// e.g., `use std::io`
    /// e.g., `use utils::{math as m, ops}`
    fn parse_use_statement(&mut self) -> Option<Item> {
        // 1. 消耗 'use' 关键字
        let use_keyword = self.consume(TokenType::Use, "Expected 'use' to begin an import statement.")?;
        
        // 2. 解析核心的 UseTree
        // 我们将复杂的逻辑委托给一个专门的辅助函数
        let tree = self.parse_use_tree()?;
        
        // 4. 构建并返回 AST 节点
        Some(Item::Use(UseStmt {
            use_keyword,
            prefix: None, // 注意：为了简化，我们暂时不支持 prefix，下一步可以加入
            tree,
        }))
    }

    /// 解析 `use` 声明中的树状部分。这是一个递归函数。
    fn parse_use_tree(&mut self) -> Option<UseTree> {
        if self.check(TokenType::LeftBrace) {
            // --- 解析分组: `{...}` ---
            self.advance(); // 消耗 '{'
            let mut items = Vec::new();

            // 循环解析逗号分隔的子树
            while !self.check(TokenType::RightBrace) && !self.is_at_end() {
                items.push(self.parse_use_tree()?);
                if !self.check(TokenType::RightBrace) {
                    self.consume(TokenType::Comma, "Expected ',' to separate import items in a group.")?;
                }
            }

            self.consume(TokenType::RightBrace, "Expected '}' to close an import group.")?;
            Some(UseTree::Group { items })

        } else if self.check(TokenType::Star) {
            // --- 解析通配符: `*` ---
            let star = self.current.clone();
            self.advance();
            Some(UseTree::Wildcard { star_token: star })

        } else {
            // --- 解析简单路径: `path` 或 `path as alias` ---
            let path = self.parse_path()?;
            let alias = if self.check(TokenType::As) {
                self.advance(); // 消耗 'as'
                Some(self.consume(TokenType::Identifier, "Expected an alias name after 'as'.")?)
            } else {
                None
            };
            Some(UseTree::Simple { path, alias })
        }
    }

    /// 解析一个 `a::b::c` 形式的路径。
    fn parse_path(&mut self) -> Option<Path> {
        let mut segments = Vec::new();
        
        // 使用 consume 来处理第一个段，如果失败它会自动报告错误
        let first_segment = self.consume(
            TokenType::Identifier, // consume 暂不支持多类型，我们先简化
            "Expected an identifier or 'self' to start a path.",
        )?;
        segments.push(first_segment);

        while self.check(TokenType::DoubleColon) {
            self.advance(); // 消耗 '::'
            segments.push(self.consume(TokenType::Identifier, "Expected an identifier after '::'.")?);
        }

        Some(Path { segments })
    }

    // --- 其他顶层项目解析函数的占位符 ---
    
    /// 解析一个函数定义。
    /// e.g., `fun add(a: int, b: int) -> int { ... }`
    fn parse_function_definition(&mut self) -> Option<Item> {
        // 1. 消耗 'fun' 关键字
        let fun_keyword = self.consume(TokenType::Fun, "Expected 'fun' to start a function definition.")?;
        
        // 2. 消耗函数名
        let name = self.consume(TokenType::Identifier, "Expected a function name after 'fun'.")?;
        
        // 3. 解析参数列表 `(...)`
        let params = self.parse_parameters()?;
        
        // 4. 解析可选的返回类型 `-> type`
        let return_type = if self.check(TokenType::Arrow) {
            self.advance(); // 消耗 '->'
            Some(self.parse_type()?)
        } else {
            None
        };
        
        // 5. 解析函数体 `{...}`
        let body = self.parse_block_statement()?;
        
        // 6. 构建并返回 AST 节点
        Some(Item::Function(FunctionDef {
            name,
            params,
            return_type,
            body,
        }))
    }
    
    fn parse_struct_definition(&mut self) -> Option<Item> {
        unimplemented!("Parsing for struct definitions is not yet implemented.");
    }

    // --- 语法组件解析 (辅助函数) ---

    /// 解析函数定义的参数列表 `(a: int, b: int)`
    fn parse_parameters(&mut self) -> Option<Vec<Param>> {
        self.consume(TokenType::LeftParen, "Expected '(' after function name.")?;
        let mut params = Vec::new();

        if !self.check(TokenType::RightParen) {
            // 解析第一个参数
            params.push(self.parse_single_parameter()?);
            // 循环解析后续的参数
            while self.check(TokenType::Comma) {
                self.advance(); // 消耗 ','
                params.push(self.parse_single_parameter()?);
            }
        }
        
        self.consume(TokenType::RightParen, "Expected ')' to close the parameter list.")?;
        Some(params)
    }

    /// 解析单个参数 `name: type`
    fn parse_single_parameter(&mut self) -> Option<Param> {
        let name = self.consume(TokenType::Identifier, "Expected a parameter name.")?;
        self.consume(TokenType::Colon, "Expected ':' after parameter name.")?;
        let param_type = self.parse_type()?;
        Some(Param { name, param_type })
    }

    /// 解析一个类型注解，支持指针 e.g., `int`, `^Point`, `^^bool`
    fn parse_type(&mut self) -> Option<Type> {
        if self.check(TokenType::Caret) {
            // --- 解析指针类型 ---
            self.advance(); // 消耗 '^'
            // 递归调用 parse_type 来处理多重指针，例如 `^^int`
            let base_type = self.parse_type()?;
            Some(Type::Pointer { base: Box::new(base_type) })
        } else if self.check(TokenType::LeftParen) {
            // --- 新增：解析单元类型 () ---
            self.advance(); // 消耗 '('
            self.consume(TokenType::RightParen, "Expected ')' to form the unit type '()'.")?;
            Some(Type::Unit)
        } else {
            // --- 解析标识符类型 ---
            let name = self.consume(TokenType::Identifier, "Expected a type name.")?;
            Some(Type::Identifier { name })
        }
    }

    // --- 语句解析 (新层级！) ---
    
    /// 解析一个代码块 `{...}`
    fn parse_block_statement(&mut self) -> Option<BlockStmt> {
        self.consume(TokenType::LeftBrace, "Expected '{' to start a block.")?;
        let mut stmts = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            // 在解析一条语句之前，先跳过所有无关的空行
            while self.check(TokenType::Newline) {
                self.advance();
            }

            // 如果跳过空行后就到了 '}'，说明是块的结尾
            if self.check(TokenType::RightBrace) {
                break;
            }
            // 解析块内部的每一个语句
            if let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
            }
            
            // --- 新增逻辑 ---
            // 如果解析完一条语句后，后面不是 `}`，那么我们期望至少有一个换行符来分隔语句
            if !self.check(TokenType::RightBrace) {
                // 如果没有换行符，就报错
                self.consume(TokenType::Newline, "Expected a newline to separate statements inside a block.")?;
            }
            
            // 消耗掉所有多余的空行
            while self.check(TokenType::Newline) {
                self.advance();
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' to close a block.")?;
        Some(BlockStmt { stmts })
    }

    /// 解析一条语句。这是所有语句解析的“分派中心”。
    fn parse_statement(&mut self) -> Option<Stmt> {
        // 匹配当前 Token
        match self.current.kind {
            TokenType::If => self.parse_if_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Let => self.parse_let_statement(),
            TokenType::LeftBrace => self.parse_block_statement().map(Stmt::Block),
            TokenType::Identifier => {
                if self.peek_is_explicit_var_decl() {
                    self.parse_var_statement()
                } else {
                    self.parse_expression_statement()
                }
            }
            _ => self.parse_expression_statement(),
        }
    }

    /// 预读以判断当前是否是一个显式变量声明 (`ident: type`)
    fn peek_is_explicit_var_decl(&self) -> bool {
        // 现在我们只需要直接检查 current 和 peek 字段即可！
        self.current.kind == TokenType::Identifier && self.peek.kind == TokenType::Colon
    }

    /// 解析一个显式变量声明语句
    /// e.g., `x: int` or `y: bool = false`
    fn parse_var_statement(&mut self) -> Option<Stmt> {
        let name = self.consume(TokenType::Identifier, "Expected a variable name.")?;
        self.consume(TokenType::Colon, "Expected ':' for a type annotation.")?;
        let var_type = self.parse_type()?;
        
        let value = if self.check(TokenType::Equal) {
            self.advance(); // 消耗 '='
            Some(self.parse_expression(Precedence::Assignment)?)
        } else {
            None
        };

        Some(Stmt::Var(VarStmt { name, var_type, value }))
    }

    /// 解析 if-else 语句 self.consume(TokenType::Newline, "Expected a newline after a variable declaration.");
    fn parse_if_statement(&mut self) -> Option<Stmt> {
        self.consume(TokenType::If, "Expected 'if'.")?;
        let condition = self.parse_expression(Precedence::None)?;
        let then_branch = self.parse_block_statement()?;
        
        let else_branch = if self.check(TokenType::Else) {
            self.advance(); // 消耗 'else'
            // `else` 后面可以跟着另一个 `if` (构成 else if) 或者一个代码块
            if self.check(TokenType::If) {
                // 这是 'else if' 的情况
                // 递归调用 parse_if_statement，它会返回一个 Stmt::If
                Some(Box::new(self.parse_if_statement()?))
            } else {
                // 这是 'else { ... }' 的情况
                // parse_block_statement 返回 BlockStmt，我们需要把它包装成 Stmt::Block
                Some(Box::new(Stmt::Block(self.parse_block_statement()?)))
            }
        } else {
            None
        };

        Some(Stmt::If(IfStmt { condition, then_branch, else_branch }))
    }

    /// 解析 while 循环
    fn parse_while_statement(&mut self) -> Option<Stmt> {
        self.consume(TokenType::While, "Expected 'while'.")?;
        let condition = self.parse_expression(Precedence::None)?;
        let body = self.parse_block_statement()?;

        Some(Stmt::While(WhileStmt { condition, body }))
    }

    /// 解析 `return` 语句
    /// e.g., `return` or `return 5`
    fn parse_return_statement(&mut self) -> Option<Stmt> {
        let keyword = self.consume(TokenType::Return, "Expected 'return'.")?;
        
        let value = if !self.check(TokenType::Newline) {
            // 如果 return 后面不是直接跟换行，说明它带有一个返回值
            Some(self.parse_expression(Precedence::Assignment)?)
        } else {
            None
        };

        Some(Stmt::Return(ReturnStmt { keyword, value }))
    }

    /// 解析 `let` 语句 (类型推断式声明)
    /// e.g., `let x = 10`
    fn parse_let_statement(&mut self) -> Option<Stmt> {
        self.consume(TokenType::Let, "Expected 'let'.")?;
        let name = self.consume(TokenType::Identifier, "Expected an identifier after 'let'.")?;
        self.consume(TokenType::Equal, "Expected '=' after identifier in a let statement.")?;
        
        let value = self.parse_expression(Precedence::Assignment)?;

        Some(Stmt::Let(LetStmt { name, value }))
    }
    
    /// 解析一个表达式语句
    /// e.g., `x = 10` or `my_func()`
    fn parse_expression_statement(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression(Precedence::Assignment)?;
        Some(Stmt::Expression(ExprStmt { expr }))
    }

    // --- 表达式解析 (The Expression Parsing Layer - Placeholder) ---
    
    /// 解析一个表达式。
    /// 这是我们将要构建的 Pratt Parser 的入口。
    /// 解析一个表达式（Pratt Parser 的主入口）。
    /// `min_precedence` 参数是算法的核心，它告诉函数“只处理比我优先级更高的操作符”。
    fn parse_expression(&mut self, min_precedence: Precedence) -> Option<Expr> {
        // 1. 解析前缀表达式
        // 任何表达式的开头都必须是一个值、变量、或前缀操作。
        let mut left_expr = self.parse_prefix_expression()?;

        // 2. 循环处理中缀表达式
        // 这个循环是 Pratt Parsing 的精髓所在。
        // 只要下一个 Token 是一个比我们当前优先级更高的中缀操作符，
        // 我们就 向右“结合”表达式。
        while min_precedence < Self::get_infix_precedence(self.current.kind) {
            left_expr = self.parse_infix_expression(left_expr)?;
        }

        Some(left_expr)
    }

    /// 解析前缀表达式（字面量、变量、分组、一元操作等）。
    fn parse_prefix_expression(&mut self) -> Option<Expr> {
        // “先保存，后前进”
        let token = self.current.clone();
        self.advance();
        
        match token.kind {
            // 字面量
            TokenType::Integer | TokenType::Float | TokenType::String | TokenType::Bool => {
                Some(Expr::Literal(LiteralExpr { value: token }))
            }
            // 变量 或 结构体初始化 ---
            TokenType::Identifier => {
                // 这是关键：在消耗掉标识符后，我们需要“偷看”下一个 Token
                if self.check(TokenType::LeftBrace) {
                    // 如果后面是 '{'，那么这是一个结构体初始化
                    self.parse_struct_init_expression(token)
                } else {
                    // 否则，它就是一个普通的变量
                    Some(Expr::Variable(VariableExpr { name: token }))
                }
            }
            // 一元操作符
            TokenType::Minus => {
                let right = self.parse_expression(Precedence::Prefix)?;
                Some(Expr::Unary(UnaryExpr {
                    operator: token,
                    right: Box::new(right),
                }))
            }
            // 分组
            TokenType::LeftParen => {
                let expr = self.parse_expression(Precedence::None)?; // 括号内优先级重置
                self.consume(TokenType::RightParen, "Expected ')' after expression.")?;
                Some(Expr::Grouping(GroupingExpr { expr: Box::new(expr) }))
            }
            _ => {
                // 错误：不是一个合法的表达式开头
                let err = ParserError::new(ParserErrorKind::ExpectedExpression, token.span);
                self.diagnostics.add_error(CompilerError::Parser(err));
                None
            }
        }
    }

    // 解析中缀表达式（二元操作、函数调用、成员访问等）。
    fn parse_infix_expression(&mut self, left: Expr) -> Option<Expr> {
        let operator_token = self.current.clone();
        self.advance(); // 消耗掉操作符

        let precedence = Self::get_infix_precedence(operator_token.kind);
        
        match operator_token.kind {
            // --- 二元操作 ---
            TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash => {
                let right = self.parse_expression(precedence)?;
                Some(Expr::Binary(BinaryExpr {
                    left: Box::new(left),
                    operator: operator_token,
                    right: Box::new(right),
                }))
            }
            
            // --- 赋值 ---
            TokenType::Equal => self.parse_assignment_expression(left),
            
            // --- 函数调用 ---
            TokenType::LeftParen => self.parse_call_expression(left),

            // --- 成员访问 ---
            TokenType::Dot => {
                let field = self.consume(TokenType::Identifier, "Expected a field name after '.'.")?;
                Some(Expr::MemberAccess(MemberAccessExpr {
                    object: Box::new(left),
                    field,
                }))
            }

            _ => {
                unreachable!("Invalid infix operator found: {:?}", operator_token);
            }
        }
    }

    /// 解析函数调用的参数列表 `(...)`
    fn parse_call_expression(&mut self, callee: Expr) -> Option<Expr> {
        let mut args = Vec::new();

        if !self.check(TokenType::RightParen) {
            // 解析第一个参数
            args.push(self.parse_expression(Precedence::None)?);
            // 循环解析后续的参数
            while self.check(TokenType::Comma) {
                self.advance(); // 消耗 ','
                args.push(self.parse_expression(Precedence::None)?);
            }
        }
        
        self.consume(TokenType::RightParen, "Expected ')' to close the argument list.")?;
        Some(Expr::Call(CallExpr {
            callee: Box::new(callee),
            args,
        }))
    }

    /// 解析结构体初始化 `{ field: value, ... }`
    fn parse_struct_init_expression(&mut self, name: Token) -> Option<Expr> {
        self.consume(TokenType::LeftBrace, "Expected '{' to start a struct initializer.")?;
        let mut fields = Vec::new();

        if !self.check(TokenType::RightBrace) {
            // 解析第一个字段
            fields.push(self.parse_struct_field()?);
            // 循环解析后续的字段
            while self.check(TokenType::Comma) {
                self.advance(); // 消耗 ','
                // 允许结尾有一个可选的逗号
                if self.check(TokenType::RightBrace) { break; }
                fields.push(self.parse_struct_field()?);
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' to close a struct initializer.")?;
        Some(Expr::StructInit(StructInitExpr { name, fields }))
    }

    /// 解析单个结构体字段 `name: value`
    fn parse_struct_field(&mut self) -> Option<(Token, Expr)> {
        let name = self.consume(TokenType::Identifier, "Expected a field name.")?;
        self.consume(TokenType::Colon, "Expected ':' after field name.")?;
        let value = self.parse_expression(Precedence::None)?;
        Some((name, value))
    }

    /// 解析一个赋值表达式 `target = value`
    fn parse_assignment_expression(&mut self, target: Expr) -> Option<Expr> {
        // 在解析右侧时，我们传入比 Assignment 更低的优先级 (None)，
        // 这将正确处理像 a = b = c 这样的右结合链式赋值。
        let value = self.parse_expression(Precedence::None)?;
        
        // TODO: 在语义分析阶段，检查 target 是否是一个合法的“左值”（l-value）
        // 例如，变量和成员访问是合法的，但 `1 + 2 = 3` 是不合法的。
        
        Some(Expr::Assignment(AssignExpr {
            target: Box::new(target),
            value: Box::new(value),
        }))
    }
}