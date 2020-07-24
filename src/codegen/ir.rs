use crate::library::{GenericSpecialization, NodeType, Symbol, VarMetadata, TypeMetadata, SymbolStore};

#[derive(Debug)]
pub struct IRStructure {
    pub name: Symbol,
    pub fields: Vec<IRVariable>,
}

#[derive(Debug)]
pub struct IRFunction {
    pub name: Symbol,
    pub parameters: Vec<IRVariable>,
    pub return_type: NodeType,
    pub statements: Vec<IRStatement>,
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

    pub fn new_meta(metadata: &VarMetadata, parent: &Symbol) -> Self {
        let symbol = parent.child(&metadata.name);
        IRVariable::new_sym(&symbol, metadata.var_type.clone())
    }

    pub fn self_var(owner: &TypeMetadata) -> Self {
        IRVariable {
            name: "self".to_owned(),
            var_type: owner.ref_to_unspecialized_type()
        }
    }
}

#[derive(Clone, Debug)]
pub enum IRCompilationCondition {
    ConformanceCheck(Symbol, Symbol),
    TypeEqualityCheck(Symbol, NodeType),
}

impl IRCompilationCondition {
    pub fn satisfied_by(&self, spec: &GenericSpecialization, store: &SymbolStore) -> bool {
        match self {
            IRCompilationCondition::ConformanceCheck(gen_sym, trait_sym) => {
                let resolved_type = spec.type_for(gen_sym).unwrap();
                if let NodeType::Instance(type_sym, ..) = resolved_type {
                    let metadata = store.type_metadata(&type_sym).unwrap();
                    metadata.conforms_to(trait_sym)
                } else {
                    false
                }
            }
            IRCompilationCondition::TypeEqualityCheck(gen_sym, node_type) => {
                let resolved_type = spec.type_for(gen_sym).unwrap();
                resolved_type.matches(node_type)
            }
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
    CompilationCondition(IRCompilationCondition, Vec<IRStatement>),
    Break,
}

#[derive(Debug)]
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
            expr_type: NodeType::pointer_to(NodeType::Byte),
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

    pub fn field_deref(var: &IRVariable, field: &str, field_type: NodeType) -> Self {
        let var = IRExpr::variable(var);
        IRExpr {
            kind: IRExprKind::DerefFieldAccess(Box::new(var), String::from(field)),
            expr_type: field_type,
        }
    }

    pub fn call(
        func: Symbol,
        spec: GenericSpecialization,
        args: Vec<IRExpr>,
        ret_type: NodeType,
    ) -> Self {
        IRExpr {
            kind: IRExprKind::Call(func.clone(), spec, args),
            expr_type: ret_type,
        }
    }

    pub fn call_extern(func: &str, args: Vec<IRExpr>, ret_type: NodeType) -> Self {
        IRExpr {
            kind: IRExprKind::Call(Symbol::lib_root(func), GenericSpecialization::empty(), args),
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

    pub fn has_defined_location(&self) -> bool {
        match &self.kind {
            IRExprKind::Variable(_) | IRExprKind::ExplicitType => true,
            IRExprKind::FieldAccess(object, _) | IRExprKind::DerefFieldAccess(object, _) => {
                object.has_defined_location()
            }
            IRExprKind::Cast(object) => object.has_defined_location(),
            IRExprKind::Unary(..) | IRExprKind::Binary(..) => false,
            IRExprKind::Literal(..) => false,
            IRExprKind::Call(..) => false,
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

#[derive(Debug)]
pub enum IRExprKind {
    FieldAccess(Box<IRExpr>, String),
    DerefFieldAccess(Box<IRExpr>, String),
    Call(Symbol, GenericSpecialization, Vec<IRExpr>),
    Binary(Box<IRExpr>, IRBinaryOperator, Box<IRExpr>),
    Unary(IRUnaryOperator, Box<IRExpr>),
    Literal(String),
    Variable(String),
    ExplicitType,
    Cast(Box<IRExpr>),
}
