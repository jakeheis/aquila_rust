use crate::library::*;

#[derive(Debug)]
pub struct IRProgram {
    pub structures: Vec<IRStructure>,
    pub functions: Vec<IRFunction>
}

impl IRProgram {
    pub fn new() -> Self {
        IRProgram {
            structures: Vec::new(),
            functions: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct IRStructure {
    pub name: String,
    pub fields: Vec<IRVariable>
}

#[derive(Debug)]
pub struct IRFunction {
    pub name: String,
    pub parameters: Vec<IRVariable>,
    pub return_type: NodeType,
    pub statements: Vec<IRStatement>
}

#[derive(Debug)]
pub struct IRVariable {
    pub name: String,
    pub var_type: NodeType
}

#[derive(Debug)]
pub enum IRStatement {
    DeclLocal(IRVariable),
    AssignLocal(String, IRExpr),
    AssignField(IRExpr, String, IRExpr),
    Loop(Vec<IRStatement>),
    Condition(IRExpr, Vec<IRStatement>, Vec<IRStatement>),
    Execute(IRExpr),
    Return(Option<IRExpr>),
    Break,
}

#[derive(Debug)]
pub struct IRExpr {
    pub kind: IRExprKind,
    pub expr_type: NodeType
}

#[derive(Debug)]
pub enum IRExprKind {
    FieldAccess(Box<IRExpr>, String),
    DerefFieldAccess(Box<IRExpr>, String),
    Call(String, Vec<IRExpr>),
    Subscript(Box<IRExpr>, Box<IRExpr>),
    Binary(Box<IRExpr>, String, Box<IRExpr>),
    Unary(String, Box<IRExpr>),
    Literal(String),
    Variable(String),
    ExplicitType,
    Cast(Box<IRExpr>)
}
