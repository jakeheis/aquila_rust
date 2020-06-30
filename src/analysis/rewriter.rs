use crate::lexing::*;
use crate::parsing::*;
use crate::library::*;
use crate::source::{self, *};

pub struct Rewriter {
    lib: Lib,
}

impl Rewriter {
    pub fn rewrite(mut lib: Lib) -> Lib {
        let stmts = std::mem::replace(&mut lib.other, Vec::new());

        let mut writer = Rewriter {
            lib
        };

        let stmts = stmts.into_iter().map(|s| writer.rewrite_stmt(s)).collect();
        std::mem::replace(&mut writer.lib.other, stmts);
        writer.lib
    }

    fn rewrite_stmt(&self, mut stmt: Stmt) -> Stmt {
        let stmt_span = std::mem::replace(&mut stmt.span, Span::empty());
        match stmt.kind {
            StmtKind::PrintStmt(value) if value.is_some() => {
                self.rewrite_print(value.unwrap())
            },
            kind => Stmt::new(kind, stmt_span),
        }
    }

    fn rewrite_print(&self, expr: Expr) -> Stmt {
        let print_type = expr.get_type().unwrap();
        if let NodeType::Instance(i, spec) = print_type {
            println!("Rewriting print {}", expr.span().lexeme());

            let full_symbol = Symbol::new_str(&i, "write");
            let metadata = self.lib.function_metadata(&full_symbol).unwrap();
            let function_name = metadata.function_name(&self.lib, &spec);
            
            let call_token = self.fake_identifier(&function_name);
            call_token.set_symbol(full_symbol.clone());

            self.lib.specialization_tracker.add_call(Symbol::main_symbol(&self.lib), full_symbol, spec);

            let call = Expr::new(ExprKind::FunctionCall(Some(Box::new(expr)), call_token, Vec::new()), Span::empty());
            let s = Stmt::expression(call);
            let ar = [s];
            ASTPrinter::stdout().print(&ar);
            let [s] = ar;
            s
        } else {
            Stmt::expression(expr)
        }
    }

    fn fake_identifier(&self, lexeme: &str) -> ResolvedToken {
        let source = source::text(lexeme);
        let span = Span::new(&source, 0, source.length(), 1);
        let token = Token::new(TokenKind::Identifier, span);
        ResolvedToken::new_non_specialized(token)
    }
}
