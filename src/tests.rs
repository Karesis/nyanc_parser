use nyanc_core::FileId;
use super::*;
use lexer::Lexer;
use reporter::DiagnosticsEngine;

/// 核心测试辅助函数，带有详细的错误报告功能
fn check_parsing(source: &str, expected_ast_str: &str) {
    // --- 1. 准备环境 ---
    let diagnostics = DiagnosticsEngine::new();
    // 关键：为每一次测试创建一个全新的、独立的 Ast 仓库 (Arena)
    let mut ast = ast::Ast::new(); 
    
    let file_id: FileId = 0; // 在测试中，文件ID设为0即可
    
    // --- 2. 运行解析 ---
    let lexer = Lexer::new(source, file_id, &diagnostics);
    // 关键：将仓库的可变引用传入 Parser，让它有地方存储节点
    let mut parser = Parser::new(lexer, &diagnostics, &mut ast);

    // 关键：parse() 现在返回的是 Module 的 ID，而不是 Module 对象本身
    let module_id = parser.parse();

    // --- 3. 检查解析错误 (这部分逻辑不变) ---
    if diagnostics.has_errors() {
        let mut error_report = String::new();
        let errors = diagnostics.errors.borrow();
        error_report.push_str("Collected errors:\n");
        for (i, error) in errors.iter().enumerate() {
            error_report.push_str(&format!("  {}: {:?}\n", i + 1, error));
        }
        panic!(
            "Parsing produced unexpected errors for source:\n---\n{}\n---\n{}",
            source,
            error_report
        );
    }
    
    // --- 4. 打印并验证 AST 快照 ---
    // 关键：将仓库的不可变引用传入 Printer，让它能根据 ID 查找到节点
    let printer = ast::ast_printer::AstPrinter::new(&ast); 
    
    // 关键：使用返回的 module_id 从仓库中查找出真正的 Module 节点
    let module_node = &ast.modules[module_id.get_raw() as usize];
    let generated_ast_str = printer.print_module(module_node);

    // 快照对比逻辑不变
    if generated_ast_str.trim() != expected_ast_str.trim() {
        println!("--- TEST FAILED: AST Snapshot Mismatch ---");
        println!("--- SOURCE CODE ---\n{}", source);
        println!("--- EXPECTED AST ---\n{}", expected_ast_str.trim());
        println!("--- GENERATED AST ---\n{}", generated_ast_str.trim());
        panic!("AST snapshot mismatch!");
    }
}

#[test]
fn test_let_statement_inside_function() {
    // 将 let 语句放入一个函数体中，使其成为合法的 nyan 代码
    let source = r#"
    fun main() {
        let answer = (1 + 2) * 3
    }
    "#;
    
    // 期望的 AST 快照也需要相应地更新
    let expected = r#"(module (fun main (params ()) (block (let answer (* (group (+ 1 2)) 3)))))"#;
    
    check_parsing(source, expected);
}

#[test]
fn test_var_declaration_inside_function() {
    let source = r#"
        fun test_fn() {
            pos: ^^int
        }
    "#;
    let expected = r#"(module (fun test_fn (params ()) (block (var pos: (^ (^ int))))))"#;
    check_parsing(source, expected);
}

#[test]
fn test_if_else_statement() {
    let source = r#"
        fun test_fn() {
            if x > 5 {
                return true
            } else {
                return false
            }
        }
    "#;
    let expected = r#"(module (fun test_fn (params ()) (block (if (> x 5) (block (return true)) (else (block (return false)))))))"#;
    check_parsing(source, expected);
}

#[test]
fn test_function_definition() {
    let source = r#"
    fun add(a: int, b: int) -> int {
        return a + b
    }
    "#;
    let expected = r#"(module (fun add (params (a: int) (b: int)) (returns int) (block (return (+ a b)))))"#;
    check_parsing(source, expected);
}

#[test]
fn test_complex_use_statement() {
    // 我们暂时测试一个没有 prefix 的复杂 use 语句
    let source = "use {io as iostream, fs}\n";
    let expected = "(module (use {(as io iostream) fs}))";
    check_parsing(source, expected);
}

#[test]
fn test_struct_definition() {
    // 我们测试一个包含多个字段、指针类型和可选结尾逗号的典型场景
    let source = r#"
        struct Point {
            x: int,
            y: ^int,
        }
    "#;
    
    // 这是我们期望的、由 AstPrinter 生成的精确快照
    let expected = r#"(module (struct Point (fields (x: int) (y: (^ int)))))"#;
    
    check_parsing(source, expected);
}

#[test]
fn test_empty_struct_definition() {
    let source = r#"
        struct Empty {}
    "#;
    let expected = r#"(module (struct Empty (fields ())))"#;
    check_parsing(source, expected);
}

#[test]
fn test_simple_use_path() {
    let source = "use std::io::Error\n";
    // 修正后的快照
    let expected = "(module (use std::io::Error))";
    check_parsing(source, expected);
}

#[test]
fn test_simple_use_with_alias() {
    let source = "use std::io::Error as IoError\n";
    // 修正后的快照
    let expected = "(module (use (as std::io::Error IoError)))";
    check_parsing(source, expected);
}

#[test]
fn test_use_with_group() {
    let source = "use std::{io, fs, net}\n";
    // 修正后的快照
    let expected = "(module (use std::{io fs net}))";
    check_parsing(source, expected);
}

#[test]
fn test_use_with_group_and_aliases() {
    let source = "use std::{io as iostream, fs}\n";
    // 修正后的快照
    let expected = "(module (use std::{(as io iostream) fs}))";
    check_parsing(source, expected);
}

#[test]
fn test_use_with_wildcard() {
    let source = "use std::io::*\n";
    // 修正后的快照
    let expected = "(module (use std::io::*))";
    check_parsing(source, expected);
}

#[test]
fn test_use_self() {
    let source = "use self::utils\n";
    // 修正后的快照
    let expected = "(module (use self::utils))";
    check_parsing(source, expected);
}