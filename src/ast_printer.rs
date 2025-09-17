// parser/src/ast_printer.rs

use crate::ast::*;

/// 负责将 AST 节点序列化为 S-表达式风格的字符串，用于快照测试。
pub struct AstPrinter;

impl AstPrinter {
    /// 打印整个模块的入口点
    pub fn print_module(&self, module: &Module) -> String {
        let mut parts = Vec::new();
        for item in &module.items {
            parts.push(self.print_item(item));
        }
        self.parenthesize("module", &parts)
    }

    // --- Item Printers ---

    fn print_item(&self, item: &Item) -> String {
        match item {
            Item::Use(stmt) => self.print_use_statement(stmt),
            Item::Function(def) => self.print_function_definition(def),
            Item::Struct(def) => self.print_struct_definition(def),
        }
    }

    fn print_use_statement(&self, stmt: &UseStmt) -> String {
        // e.g., (use (path self utils)) or (use utils::{...})
        if let Some(prefix) = &stmt.prefix {
            let prefix_str = self.print_path(prefix);
            let tree_str = self.print_use_tree(&stmt.tree);
            self.parenthesize(&format!("use {}", prefix_str), &[tree_str])
        } else {
            self.parenthesize("use", &[self.print_use_tree(&stmt.tree)])
        }
    }

    fn print_use_tree(&self, tree: &UseTree) -> String {
        match tree {
            UseTree::Simple { path, alias } => {
                let path_str = self.print_path(path);
                if let Some(alias) = alias {
                    format!("(as {} {})", path_str, &alias.lexeme)
                } else {
                    path_str
                }
            }
            UseTree::Group { items } => {
                let mut item_strs = Vec::new();
                for item in items {
                    item_strs.push(self.print_use_tree(item));
                }
                format!("{{{}}}", item_strs.join(" "))
            }
            UseTree::Wildcard { .. } => "*".to_string(),
        }
    }

    fn print_path(&self, path: &Path) -> String {
        path.segments
            .iter()
            .map(|tok| tok.lexeme.as_str())
            .collect::<Vec<_>>()
            .join("::")
    }

    fn print_function_definition(&self, def: &FunctionDef) -> String {
        let mut parts = Vec::new();

        let params_str = if def.params.is_empty() {
            // 如果没有参数，就明确地打印 "()"
            "()".to_string()
        } else {
            // 如果有参数，就正常地将它们连接起来
            def.params.iter()
               .map(|param| format!("({}: {})", &param.name.lexeme, self.print_type(&param.param_type)))
               .collect::<Vec<_>>()
               .join(" ")
        };
        parts.push(self.parenthesize("params", &[params_str]));

        if let Some(ret_type) = &def.return_type {
            parts.push(self.parenthesize("returns", &[self.print_type(ret_type)]));
        }

        parts.push(self.print_statement(&Stmt::Block(def.body.clone())));
        self.parenthesize(&format!("fun {}", &def.name.lexeme), &parts)
    }
    
    fn print_struct_definition(&self, def: &StructDef) -> String {
        // --- 完整实现 ---
        // 我们将字段列表格式化，这与打印函数参数的逻辑非常相似
        let fields_str = if def.fields.is_empty() {
            "()".to_string() // 处理没有字段的空结构体
        } else {
            def.fields.iter()
               .map(|field| format!("({}: {})", &field.name.lexeme, self.print_type(&field.param_type)))
               .collect::<Vec<_>>()
               .join(" ")
        };
        
        // 为了清晰，我们用 `fields` 而不是 `params` 来包裹
        let parts = vec![self.parenthesize("fields", &[fields_str])];
        
        self.parenthesize(&format!("struct {}", &def.name.lexeme), &parts)
    }


    // --- Statement Printers ---

    fn print_statement(&self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Let(s) => self.parenthesize(&format!("let {}", &s.name.lexeme), &[self.print_expression(&s.value)]),
            Stmt::Var(s) => {
                let type_str = self.print_type(&s.var_type);
                if let Some(val) = &s.value {
                    self.parenthesize(&format!("var {}: {}", &s.name.lexeme, type_str), &[self.print_expression(val)])
                } else {
                    format!("(var {}: {})", &s.name.lexeme, type_str)
                }
            }
            Stmt::Return(s) => match &s.value {
                Some(val) => self.parenthesize("return", &[self.print_expression(val)]),
                None => "(return)".to_string(),
            },
            Stmt::Block(s) => {
                let mut stmts_str = Vec::new();
                for stmt in &s.stmts {
                    stmts_str.push(self.print_statement(stmt));
                }
                self.parenthesize("block", &stmts_str)
            }
            Stmt::Expression(s) => self.parenthesize("expr_stmt", &[self.print_expression(&s.expr)]),
            Stmt::If(s) => {
                let cond = self.print_expression(&s.condition);
                let then = self.print_statement(&Stmt::Block(s.then_branch.clone()));
                if let Some(else_br) = &s.else_branch {
                    let else_str = self.print_statement(else_br);
                    self.parenthesize("if", &[cond, then, format!("(else {})", else_str)])
                } else {
                    self.parenthesize("if", &[cond, then])
                }
            }
            Stmt::While(s) => {
                let cond = self.print_expression(&s.condition);
                let body = self.print_statement(&Stmt::Block(s.body.clone()));
                self.parenthesize("while", &[cond, body])
            }
        }
    }

    // --- Expression Printers ---

    fn print_expression(&self, expr: &Expr) -> String {
        match expr {
            Expr::Assignment(e) => self.parenthesize("=", &[self.print_expression(&e.target), self.print_expression(&e.value)]),
            Expr::Binary(e) => self.parenthesize(&e.operator.lexeme, &[self.print_expression(&e.left), self.print_expression(&e.right)]),
            Expr::Unary(e) => self.parenthesize(&e.operator.lexeme, &[self.print_expression(&e.right)]),
            Expr::Call(e) => {
                let mut parts = vec![self.print_expression(&e.callee)];
                for arg in &e.args {
                    parts.push(self.print_expression(arg));
                }
                self.parenthesize("call", &parts)
            },
            Expr::MemberAccess(e) => self.parenthesize(".", &[self.print_expression(&e.object), e.field.lexeme.clone()]),
            Expr::StructInit(e) => {
                let mut fields_str = Vec::new();
                for (name, value) in &e.fields {
                    fields_str.push(self.parenthesize(&name.lexeme, &[self.print_expression(value)]));
                }
                self.parenthesize(&format!("init {}", &e.name.lexeme), &fields_str)
            },
            Expr::Literal(e) => e.value.lexeme.clone(),
            Expr::Variable(e) => e.name.lexeme.clone(),
            Expr::Grouping(e) => self.parenthesize("group", &[self.print_expression(&e.expr)]),
            Expr::UnitLiteral => "()".to_string(),
        }
    }
    
    // --- Type Printer ---
    
    fn print_type(&self, ty: &Type) -> String {
        match ty {
            Type::Identifier { name } => name.lexeme.clone(),
            Type::Pointer { base } => self.parenthesize("^", &[self.print_type(base)]),
            Type::Unit => "()".to_string(),
        }
    }
    
    /// 辅助函数，用于生成 `(name part1 part2 ...)` 格式的字符串
    fn parenthesize(&self, name: &str, parts: &[String]) -> String {
        let mut result = String::new();
        result.push('(');
        result.push_str(name);
        for part in parts.iter() {
            result.push(' ');
            result.push_str(part);
        }
        result.push(')');
        result
    }
}