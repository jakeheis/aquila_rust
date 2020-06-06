use crate::program::*;
use crate::parsing::*;
use crate::lexing::*;
use crate::diagnostic::*;
use crate::source::*;

pub type Result = DiagnosticResult<NodeType>;

pub struct TypeChecker {
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
        }
    }

    pub fn check(&mut self, program: ParsedProgram) {
        self.check_list(&program.statements)
    }

    pub fn check_list(&mut self, stmt_list: &[Stmt]) {
        for stmt in stmt_list {
            stmt.accept(self);
        }
    }

    // fn require(&self, expr: &Expr, given: ExprType, expected: ExprType) -> Result {
    //     if given == expected {
    //         Ok(given)
    //     } else  {
    //         let message = format!("Expected {:#?}, got {:#?}", expected, given);
    //         Err(Diagnostic::error_expr(expr, &message))
    //     }
    // }

    fn type_mismatch(&self, expr: &Expr, given: NodeType, expected: NodeType) -> Result {
        let message = format!("Expected {:#?}, got {:#?}", expected, given);
        Err(Diagnostic::error_expr(expr, &message))
    }
}

impl StmtVisitor for TypeChecker {
    type StmtResult = ();

    fn visit_type_decl(
        &mut self,
        name: &Token,
        fields: &[Stmt],
        methods: &[Stmt],
    ) {
        
    }

    fn visit_function_decl(
        &mut self,
        name: &Token,
        params: &[Stmt],
        return_type: &Option<Token>,
        body: &[Stmt],
    ) {
        
    }

    fn visit_variable_decl(
        &mut self,
        name: &Token,
        kind: &Option<Token>,
        value: &Option<Expr>,
    ) {
        
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        body: &[Stmt],
        else_body: &[Stmt],
    ) {}

    fn visit_expression_stmt(&mut self, expr: &Expr) {
        match expr.accept(self) {
            Ok(_) => (),
            Err(diag) => DefaultReporter::new().report(diag)
        }
    }
}

impl ExprVisitor for TypeChecker {
    type ExprResult = Result;

    fn visit_assignment_expr(&mut self, target: &Expr, value: &Expr) -> Self::ExprResult {
        Ok(NodeType::Int)
    }

    fn visit_binary_expr(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::ExprResult {
        let lhs_type = lhs.accept(self)?;
        let rhs_type = rhs.accept(self)?;

        match op.kind {
            TokenKind::Plus => {
                if !lhs_type.can_add() {
                    self.type_mismatch(lhs, lhs_type, NodeType::Int)
                } else if !rhs_type.can_add() {
                    self.type_mismatch(rhs, rhs_type, NodeType::Int)
                } else if lhs_type != rhs_type {
                    self.type_mismatch(rhs, rhs_type, lhs_type)
                } else {
                    Ok(lhs_type)
                }
            }
            TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                if !lhs_type.can_math() {
                    self.type_mismatch(lhs, lhs_type, NodeType::Int)
                } else if !rhs_type.can_math() {
                    self.type_mismatch(rhs, rhs_type, NodeType::Int)
                } else {
                    Ok(lhs_type)
                }
            },
            TokenKind::AmpersandAmpersand | TokenKind::BarBar => {
                if !lhs_type.can_logic() {
                    self.type_mismatch(lhs, lhs_type, NodeType::Int)
                } else if !rhs_type.can_logic() {
                    self.type_mismatch(rhs, rhs_type, NodeType::Int)
                } else {
                    Ok(NodeType::Bool)
                }
            },
            TokenKind::EqualEqual | TokenKind::BangEqual => {
                if !lhs_type.can_equal() {
                    self.type_mismatch(lhs, lhs_type, NodeType::Int)
                } else if !rhs_type.can_equal() {
                    self.type_mismatch(rhs, rhs_type, NodeType::Int)
                } else if lhs_type != rhs_type {
                    self.type_mismatch(rhs, rhs_type, lhs_type)
                } else {
                    Ok(NodeType::Bool)
                }
            },
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual => {
                if !lhs_type.can_compare() {
                    self.type_mismatch(lhs, lhs_type, NodeType::Int)
                } else if !rhs_type.can_compare() {
                    self.type_mismatch(rhs, rhs_type, NodeType::Int)
                } else {
                    Ok(NodeType::Bool)
                }
            }
            _ => panic!()
        }
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Self::ExprResult {
        let operand_type = expr.accept(self)?;
        match op.kind {
            TokenKind::Bang if !operand_type.can_invert() => {
                self.type_mismatch(expr, operand_type, NodeType::Bool)
            },
            TokenKind::Minus if !operand_type.can_negate() => {
                self.type_mismatch(expr, operand_type, NodeType::Int)
            },
            _ => Ok(operand_type)
        }
    }

    fn visit_call_expr(&mut self, target: &Expr, args: &[Expr]) -> Self::ExprResult {
        Ok(NodeType::Int)
    }

    fn visit_field_expr(&mut self, target: &Expr, field: &Token) -> Self::ExprResult {
        Ok(NodeType::Int)
    }

    fn visit_literal_expr(&mut self, token: &Token) -> Self::ExprResult {
        match token.kind {
            TokenKind::Number => Ok(NodeType::Int),
            TokenKind::True => Ok(NodeType::Bool),
            TokenKind::False => Ok(NodeType::Bool),
            _ => panic!()
        }
    }

    fn visit_variable_expr(&mut self, name: &Token, var_type: &Option<Token>) -> Self::ExprResult {
        Ok(NodeType::Int)
    }

}

#[derive(Debug, PartialEq)]
pub enum NodeType {
    Int,
    Bool,
    Function(Vec<NodeType>, Box<NodeType>),
}

impl NodeType {
    fn can_invert(&self) -> bool {
        match self {
            NodeType::Bool => true,
            _ => false,
        }
    }

    fn can_negate(&self) -> bool {
        match self {
            NodeType::Int => true,
            _ => false,
        }
    }

    fn can_add(&self) -> bool {
        match self {
            NodeType::Int => true,
            _ => false,
        }
    }

    fn can_math(&self) -> bool {
        match self {
            NodeType::Int => true,
            _ => false,
        }
    }

    fn can_logic(&self) -> bool {
        match self {
            NodeType::Bool => true,
            _ => false,
        }
    }

    fn can_equal(&self) -> bool {
        match self {
            NodeType::Int | NodeType::Bool => true,
            _ => false,
        }
    }

    fn can_compare(&self) -> bool {
        match self {
            NodeType::Int => true,
            _ => false,
        }
    }
}
