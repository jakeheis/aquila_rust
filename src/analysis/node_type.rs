use super::symbol_table::*;
use crate::guard;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::*;

#[derive(Clone)]
pub enum NodeType {
    Void,
    Int,
    Bool,
    Byte,
    Type(Symbol),
    Pointer(Box<NodeType>),
    Array(Box<NodeType>, ArraySize),
    Function(Vec<NodeType>, Box<NodeType>),
    Metatype(Symbol),
    FlexibleFunction(fn(&[NodeType]) -> bool),
    Ambiguous,
}

impl NodeType {
    pub fn primitive(token: &Token) -> Option<Self> {
        match token.lexeme() {
            "void" => Some(NodeType::Void),
            "int" => Some(NodeType::Int),
            "bool" => Some(NodeType::Bool),
            "byte" => Some(NodeType::Byte),
            _ => None,
        }
    }

    pub fn pointer_to(pointee: NodeType) -> Self {
        NodeType::Pointer(Box::new(pointee))
    }

    pub fn deduce_from(expr: &Expr, lib: &Lib, context: &[Symbol]) -> Option<Self> {
        if let Some(already_typed) = expr.get_type() {
            return Some(already_typed.clone());
        }

        guard!(ExprKind::ExplicitType[category] = &expr.kind);

        let node_type = match category {
            ExplicitTypeCategory::Simple(token) => {
                if let Some(primitive) = NodeType::primitive(&token.token) {
                    primitive
                } else {
                    NodeType::Type(NodeType::symbol_for_type_token(&token.token, lib, context)?)
                }
            }
            ExplicitTypeCategory::Pointer(to) => {
                let inner = NodeType::deduce_from(to, lib, context)?;
                NodeType::Pointer(Box::new(inner))
            }
            ExplicitTypeCategory::Array(of, count_token) => {
                let inner = NodeType::deduce_from(of, lib, context)?;
                let count = count_token.lexeme().parse::<usize>().ok().unwrap();
                let size = ArraySize::Known(count);
                NodeType::Array(Box::new(inner), size)
            }
        };

        expr.set_type(node_type).ok()
    }

    fn symbol_for_type_token(token: &Token, lib: &Lib, context: &[Symbol]) -> Option<Symbol> {
        for parent in context.iter().rev() {
            let non_top_level_symbol = Symbol::new(Some(parent), token);
            if let Some(NodeType::Metatype(_)) = lib.resolve_symbol(&non_top_level_symbol) {
                return Some(non_top_level_symbol);
            }
        }

        let top_level_symbol = Symbol::new(None, token);
        if let Some(NodeType::Metatype(_)) = lib.resolve_symbol(&top_level_symbol) {
            Some(top_level_symbol)
        } else {
            None
        }
    }

    pub fn contains_ambiguity(&self) -> bool {
        match self {
            NodeType::Ambiguous => true,
            NodeType::Pointer(ptr) => ptr.contains_ambiguity(),
            NodeType::Array(of, count) => of.contains_ambiguity() || count == &ArraySize::Unknown,
            NodeType::Function(params, ret) => {
                for param in params {
                    if param.contains_ambiguity() {
                        return true;
                    }
                }
                ret.contains_ambiguity()
            }
            _ => false,
        }
    }

    pub fn matches(&self, unambiguous: &NodeType) -> bool {
        match (self, unambiguous) {
            (NodeType::Void, NodeType::Void)
            | (NodeType::Int, NodeType::Int)
            | (NodeType::Bool, NodeType::Bool)
            | (NodeType::Byte, NodeType::Byte) => true,
            (NodeType::Type(lhs), NodeType::Type(rhs))
            | (NodeType::Metatype(lhs), NodeType::Metatype(rhs))
                if lhs == rhs =>
            {
                true
            }
            (NodeType::Pointer(lhs), NodeType::Pointer(rhs)) => lhs.matches(&rhs),
            (NodeType::Function(lhs_params, lhs_ret), NodeType::Function(rhs_params, rhs_ret))
                if lhs_params.len() == rhs_params.len() =>
            {
                if !lhs_ret.matches(&rhs_ret) {
                    return false;
                }

                for (lhs_param, rhs_param) in lhs_params.iter().zip(rhs_params) {
                    if !lhs_param.matches(&rhs_param) {
                        return false;
                    }
                }

                true
            }
            (NodeType::Array(lhs_kind, lhs_size), NodeType::Array(rhs_kind, rhs_size)) => {
                if !lhs_kind.matches(rhs_kind) {
                    return false;
                }

                if let (ArraySize::Known(k1), ArraySize::Known(k2)) = (lhs_size, rhs_size) {
                    if k1 != k2 {
                        return false;
                    }
                }

                true
            }
            (NodeType::Ambiguous, _) => true,
            _ => false,
        }
    }

    // pub fn tolerant_match(&self, rhs: &NodeType) -> bool {
    //     if self.matches(rhs) {
    //         true
    //     } else {
    //         match (self, rhs) {
    //             (NodeType::Array(l_inner, l_size), NodeType::Array(r_inner, r_size)) => {
    //                 match (l_size, r_size) {
    //                     (ArraySize::Known(k1), ArraySize::Known(k2)) if k1 != k2 => return false,
    //                     (ArraySize::Unknown, ArraySize::Unknown) => return false,
    //                     _ => (),
    //                 }

    //                 let l_inner: &NodeType = &l_inner;
    //                 let r_inner: &NodeType = &r_inner;

    //                 if let (NodeType::Ambiguous, NodeType::Ambiguous) = (l_inner, r_inner) {
    //                     return true;
    //                 }

    //                 l_inner.tolerant_match(r_inner)
    //             },
    //             _ => false
    //         }
    //     }
    // }

    pub fn is_pointer_to(&self, node_type: NodeType) -> bool {
        if let NodeType::Pointer(pointee) = self {
            if pointee.matches(&node_type) {
                return true;
            }
        }
        false
    }
}

impl std::fmt::Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let kind = match self {
            NodeType::Void => String::from("void"),
            NodeType::Int => String::from("int"),
            NodeType::Bool => String::from("bool"),
            NodeType::Byte => String::from("byte"),
            NodeType::Function(params, ret) => {
                let mut string = String::from("def(");
                if let Some(first) = params.first() {
                    string += &first.to_string()
                }
                params
                    .iter()
                    .skip(1)
                    .for_each(|p| string += &format!(", {}", p));
                string += &format!("): {}", ret);
                string
            }
            NodeType::Pointer(ty) => format!("ptr<{}>", ty),
            NodeType::Array(ty, size) => format!("Array<{}, count={}>", ty, size),
            NodeType::Type(ty) => ty.id.clone(),
            NodeType::Metatype(ty) => ty.id.clone() + "_Meta",
            NodeType::Ambiguous => String::from("<ambiguous>"),
            NodeType::FlexibleFunction(_) => String::from("<flexible function>"),
        };
        write!(f, "{}", kind)
    }
}

// ArraySize

#[derive(Clone, Debug, PartialEq)]
pub enum ArraySize {
    Known(usize),
    Unknown,
}

impl std::fmt::Display for ArraySize {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ArraySize::Known(size) => write!(f, "{}", size),
            ArraySize::Unknown => write!(f, "<unknown>"),
        }
    }
}
