use crate::library::*;

#[derive(Debug)]
pub struct IRProgram {
    pub structures: Vec<IRStructure>,
    pub functions: Vec<IRFunction>,
}

impl IRProgram {
    pub fn new() -> Self {
        IRProgram {
            structures: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn dump(&self) {
        println!("Structures:");
        for structure in &self.structures {
            println!("  {}", structure.name);
            for field in &structure.fields {
                println!("    {}: {}", field.name, field.var_type);
            }
            for spec in &structure.specializations {
                println!("    spec: {}", spec);
            }
        }

        println!("Functions:");
        for func in &self.functions {
            println!("  {}", func.name);
            for param in &func.parameters {
                println!("    param: {}: {}", param.name, param.var_type);
            }
            println!("    ret: {}", func.return_type);
            for spec in &func.specializations {
                println!("    spec: {}", spec);
            }
        }
    }
}

#[derive(Debug)]
pub struct IRStructure {
    pub name: String,
    pub fields: Vec<IRVariable>,
    pub specializations: Vec<GenericSpecialization>,
}

#[derive(Debug)]
pub struct IRFunction {
    pub name: String,
    pub parameters: Vec<IRVariable>,
    pub return_type: NodeType,
    pub statements: Vec<IRStatement>,
    pub specializations: Vec<GenericSpecialization>
}

#[derive(Clone, Debug)]
pub struct IRVariable {
    pub name: String,
    pub var_type: NodeType,
}

impl IRVariable {
    pub fn new(name: &str, var_type: NodeType) -> Self {
        IRVariable {
            name: String::from(name),
            var_type,
        }
    }

    pub fn new_sym(symbol: &Symbol, var_type: NodeType) -> Self {
        IRVariable {
            name: symbol.mangled(),
            var_type,
        }
    }
}

#[derive(Debug)]
pub enum IRStatement {
    DeclLocal(IRVariable),
    Assign(IRExpr, IRExpr),
    Loop(Vec<IRStatement>),
    Condition(IRExpr, Vec<IRStatement>, Vec<IRStatement>),
    Execute(IRExpr),
    Return(Option<IRExpr>),
    Break,
}

#[derive(Clone, Debug)]
pub struct IRExpr {
    pub kind: IRExprKind,
    pub expr_type: NodeType,
}

impl IRExpr {
    pub fn int_literal(slice: &str) -> Self {
        IRExpr::literal(slice, NodeType::Int)
    }

    pub fn string_literal(slice: &str) -> Self {
        let string = format!("\"{}\"", slice);
        IRExpr {
            kind: IRExprKind::Literal(string),
            expr_type: NodeType::pointer_to(NodeType::Byte)
        }
    }

    pub fn literal(slice: &str, expr_type: NodeType) -> Self {
        IRExpr {
            kind: IRExprKind::Literal(String::from(slice)),
            expr_type,
        }
    }

    pub fn variable(var: &IRVariable) -> Self {
        IRExpr {
            kind: IRExprKind::Variable(var.name.clone()),
            expr_type: var.var_type.clone(),
        }
    }

    pub fn field(var: &IRVariable, field: &str, field_type: NodeType) -> Self {
        let var = IRExpr::variable(var);
        IRExpr {
            kind: IRExprKind::FieldAccess(Box::new(var), String::from(field)),
            expr_type: field_type,
        }
    }

    pub fn call_generic(func: &str, spec: GenericSpecialization, args: Vec<IRExpr>, ret_type: NodeType) -> Self {
        IRExpr {
            kind: IRExprKind::Call(String::from(func), spec, args),
            expr_type: ret_type,
        }
    }

    pub fn call_nongenric(func: &str, args: Vec<IRExpr>, ret_type: NodeType) -> Self {
        IRExpr {
            kind: IRExprKind::Call(String::from(func), GenericSpecialization::empty(), args),
            expr_type: ret_type,
        }
    }

    pub fn cast(expr: IRExpr, to: NodeType) -> Self {
        IRExpr {
            kind: IRExprKind::Cast(Box::new(expr)),
            expr_type: to,
        }
    }

    pub fn address_of(var: &IRVariable) -> Self {
        let expr_type = NodeType::pointer_to(var.var_type.clone());
        IRExpr::unary(IRUnaryOperator::Reference, var, expr_type)
    }

    pub fn unary(op: IRUnaryOperator, var: &IRVariable, expr_type: NodeType) -> Self {
        let var = IRExpr::variable(var);
        IRExpr {
            kind: IRExprKind::Unary(op, Box::new(var)),
            expr_type,
        }
    }

    pub fn binary(lhs: IRExpr, op: IRBinaryOperator, rhs: IRExpr, expr_type: NodeType) -> Self {
        IRExpr {
            kind: IRExprKind::Binary(Box::new(lhs), op, Box::new(rhs)),
            expr_type,
        }
    }
}

#[derive(Clone, Debug)]
pub enum IRBinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    EqualEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
}

#[derive(Clone, Debug)]
pub enum IRUnaryOperator {
    Negate,
    Invert,
    Reference,
    Dereference,
}

#[derive(Clone, Debug)]
pub enum IRExprKind {
    FieldAccess(Box<IRExpr>, String),
    DerefFieldAccess(Box<IRExpr>, String),
    Call(String, GenericSpecialization, Vec<IRExpr>),
    Array(Vec<IRExpr>),
    Subscript(Box<IRExpr>, Box<IRExpr>),
    Binary(Box<IRExpr>, IRBinaryOperator, Box<IRExpr>),
    Unary(IRUnaryOperator, Box<IRExpr>),
    Literal(String),
    Variable(String),
    ExplicitType,
    Cast(Box<IRExpr>),
}
