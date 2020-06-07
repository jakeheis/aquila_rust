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

        let entries: &[BinaryEntry] = match op.kind {
            TokenKind::Plus => &ADDITION_ENTRIES,
            TokenKind::Minus | TokenKind::Star | TokenKind::Slash => &MATH_ENTRIES,
            TokenKind::AmpersandAmpersand | TokenKind::BarBar => &LOGIC_ENTRIES,
            TokenKind::EqualEqual | TokenKind::BangEqual => &EQUALITY_ENTRIES,
            TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual => &COMPARISON_ENTRIES,
            _ => panic!()
        };

        if let Some(matching_entry) = entries.iter().find(|e| e.0 == lhs_type && e.1 == rhs_type) {
            Ok(matching_entry.2.clone())
        } else {
            let message = format!("Cannot {} on {} and {}", op.lexeme(), lhs_type, rhs_type);
            Err(Diagnostic::error_span(Span::join(lhs, rhs), &message))
        }
    }

    fn visit_unary_expr(&mut self, op: &Token, expr: &Expr) -> Self::ExprResult {
        let operand_type = expr.accept(self)?;
        
        let entries: &[NodeType] = match op.kind {
            TokenKind::Minus => &NEGATE_ENTRIES,
            TokenKind::Bang => &INVERT_ENTRIES,
            _ => panic!()
        };

        if entries.contains(&operand_type) {
            Ok(operand_type.clone())
        } else {
            let message = format!("Cannot {} on {}", op.lexeme(), operand_type);
            Err(Diagnostic::error_expr(expr, &message))
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

// NodeType

#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    Int,
    Bool,
    Function(Vec<NodeType>, Box<NodeType>),
}

impl std::fmt::Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { 
        let kind = match self {
            NodeType::Int => String::from("int"),
            NodeType::Bool => String::from("bool"),
            NodeType::Function(params, ret) => {
                let mut string = String::from("(");
                if let Some(first) = params.first() {
                    string += &first.to_string()
                }
                params.iter().skip(1).for_each(|p| {
                    string += &format!(", {}", p)
                });
                string
            }
        };
        write!(f, "{}", kind)
     }
}

const NEGATE_ENTRIES: [NodeType; 1] = [
    NodeType::Int
];

const INVERT_ENTRIES: [NodeType; 1] = [
    NodeType::Bool
];

type BinaryEntry = (NodeType, NodeType, NodeType);

const ADDITION_ENTRIES: [BinaryEntry; 1] = [
    (NodeType::Int, NodeType::Int, NodeType::Int)
];

const MATH_ENTRIES: [BinaryEntry; 1] = [
    (NodeType::Int, NodeType::Int, NodeType::Int)
];

const LOGIC_ENTRIES: [BinaryEntry; 1] = [
    (NodeType::Bool, NodeType::Bool, NodeType::Bool)
];

const EQUALITY_ENTRIES: [BinaryEntry; 2] = [
    (NodeType::Int, NodeType::Int, NodeType::Bool),
    (NodeType::Bool, NodeType::Bool, NodeType::Bool)
];

const COMPARISON_ENTRIES: [BinaryEntry; 1] = [
    (NodeType::Int, NodeType::Int, NodeType::Bool),
];
