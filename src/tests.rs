use super::ast_printer::AstPrinter;
use super::*;
use lexer::Lexer;
use reporter::DiagnosticsEngine;

/// 核心测试辅助函数，带有详细的错误报告功能
fn check_parsing(source: &str, expected_ast_str: &str) {
    let diagnostics = DiagnosticsEngine::new();
    let lexer = Lexer::new(source, 0, &diagnostics);
    let mut parser = Parser::new(lexer, &diagnostics);

    let module = parser.parse();

    // --- 升级后的错误处理 ---
    if diagnostics.has_errors() {
        // 创建一个字符串来收集所有错误的详细信息
        let mut error_report = String::new();
        
        // 从 RefCell 中借用错误列表
        let errors = diagnostics.errors.borrow();
        
        error_report.push_str("Collected errors:\n");
        for (i, error) in errors.iter().enumerate() {
            // 使用 {:?} (Debug Trait) 来打印出错误的完整结构，包括枚举变体！
            error_report.push_str(&format!("  {}: {:?}\n", i + 1, error));
        }

        // 在 panic 中包含这份详细的报告
        panic!(
            "Parsing produced unexpected errors for source:\n---\n{}\n---\n{}",
            source,
            error_report
        );
    }
    
    let printer = AstPrinter;
    let generated_ast_str = printer.print_module(&module);

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