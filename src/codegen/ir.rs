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

#[derive(Clone, Debug)]
pub struct IRVariable {
    pub name: String,
    pub var_type: NodeType
}

impl IRVariable {
    pub fn new(name: &str, var_type: NodeType) -> Self {
        IRVariable {
            name: String::from(name),
            var_type
        }
    }
}

#[derive(Debug)]
pub enum IRStatement {
    DeclLocal(IRVariable),
    // AssignLocal(String, IRExpr),
    // AssignField(IRExpr, String, IRExpr),
    Assign(IRExpr, IRExpr),
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

impl IRExpr {
    pub fn int_literal(slice: &str) -> Self {
        IRExpr {
            kind: IRExprKind::Literal(String::from(slice)),
            expr_type: NodeType::Int
        }
    }

    pub fn variable(var: &IRVariable) -> Self {
        IRExpr {
            kind: IRExprKind::Variable(var.name.clone()),
            expr_type: var.var_type.clone()
        }
    }

    pub fn address_of(var: &IRVariable) -> Self {
        let var = IRExpr::variable(var);
        let expr_type = NodeType::pointer_to(var.expr_type.clone());
        IRExpr {
            kind: IRExprKind::Unary(String::from("&"), Box::new(var)),
            expr_type
        }
    }

    pub fn field(var: &IRVariable, field: &str, field_type: NodeType) -> Self {
        let var = IRExpr::variable(var);
        IRExpr {
            kind: IRExprKind::FieldAccess(Box::new(var), String::from(field)),
            expr_type: field_type,
        }
    }
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
