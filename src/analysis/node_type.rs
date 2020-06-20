use super::symbol_table::*;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::*;
use log::trace;

#[derive(Clone)]
pub enum NodeType {
    Void,
    Int,
    Bool,
    Byte,
    Type(Symbol),
    Pointer(Box<NodeType>),
    Array(Box<NodeType>, usize),
    Function(Vec<NodeType>, Box<NodeType>),
    Generic(Symbol, usize),
    GenericMeta(Symbol, usize),
    Metatype(Symbol),
    FlexibleFunction(fn(&[NodeType]) -> bool),
    Ambiguous,
    Any,
}

impl NodeType {
    pub fn primitive(token: &Token) -> Option<Self> {
        match token.lexeme() {
            "void" => Some(NodeType::Void),
            "int" => Some(NodeType::Int),
            "bool" => Some(NodeType::Bool),
            "byte" => Some(NodeType::Byte),
            "any" => Some(NodeType::Any),
            _ => None,
        }
    }

    pub fn pointer_to(pointee: NodeType) -> Self {
        NodeType::Pointer(Box::new(pointee))
    }

    pub fn deduce_from_lib(
        explicit_type: &ExplicitType,
        lib: &Lib,
        context: &[Symbol],
    ) -> Option<Self> {
        NodeType::deduce_from(explicit_type, &lib.symbols, &lib.dependencies, context)
    }

    pub fn deduce_from(
        explicit_type: &ExplicitType,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
    ) -> Option<Self> {
        let node_type = match &explicit_type.kind {
            ExplicitTypeKind::Simple(token) => {
                if let Some(primitive) = NodeType::primitive(&token.token) {
                    primitive
                } else {
                    NodeType::symbol_for_type_token(&token.token, table, deps, context)?
                }
            }
            ExplicitTypeKind::Pointer(to) => {
                let to: &ExplicitType = &to;
                let inner = NodeType::deduce_from(to, table, deps, context)?;
                NodeType::Pointer(Box::new(inner))
            }
            ExplicitTypeKind::Array(of, count_token) => {
                let of: &ExplicitType = &of;
                let inner = NodeType::deduce_from(of, table, deps, context)?;
                let count = count_token.lexeme().parse::<usize>().ok().unwrap();
                NodeType::Array(Box::new(inner), count)
            }
        };

        Some(node_type)
    }

    fn symbol_for_type_token(
        token: &Token,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
    ) -> Option<NodeType> {
        trace!(target: "symbol_table", "Trying to find symbol for {} -- ({})", token.lexeme(), token.span.entire_line().0);

        for parent in context.iter().rev() {
            let non_top_level_symbol = Symbol::new(Some(parent), token);
            let resolved = table.get_type(&non_top_level_symbol);
            match resolved {
                Some(NodeType::Metatype(metatype)) => {
                    let instance_type = NodeType::Type(metatype.clone());
                    trace!(target: "symbol_table", "Resolving {} as {}", token.lexeme(), instance_type);
                    return Some(instance_type);
                }
                Some(NodeType::GenericMeta(symbol, index)) => {
                    let instance_type = NodeType::Generic(symbol.clone(), *index);
                    trace!(target: "symbol_table", "Resolving {} as {}", token.lexeme(), instance_type);
                    return Some(instance_type.clone());
                }
                _ => (),
            }
        }

        let top_level_symbol = Symbol::new(None, token);

        if let Some(NodeType::Metatype(metatype)) = table.get_type(&top_level_symbol) {
            let instance_type = NodeType::Type(metatype.clone());
            trace!(target: "symbol_table", "Resolving {} as Type({})", token.lexeme(), instance_type);
            Some(instance_type)
        } else {
            for dep in deps {
                if let Some(NodeType::Metatype(metatype)) = dep.resolve_symbol(&top_level_symbol) {
                    let instance_type = NodeType::Type(metatype.clone());
                    trace!(target: "symbol_table", "Resolving {} as other lib Type({})", token.lexeme(), instance_type);
                    return Some(instance_type);
                }
            }
            None
        }
    }

    pub fn contains_ambiguity(&self) -> bool {
        match self {
            NodeType::Ambiguous => true,
            NodeType::Pointer(ptr) => ptr.contains_ambiguity(),
            NodeType::Array(of, _) => of.contains_ambiguity(),
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

    // pub fn exact_match(&self, other: &NodeType) -> bool {
    //     match (self, other) {
    //         (NodeType::Void, NodeType::Void)
    //         | (NodeType::Int, NodeType::Int)
    //         | (NodeType::Bool, NodeType::Bool)
    //         | (NodeType::Byte, NodeType::Byte) => true,
    //         (NodeType::Type(lhs), NodeType::Type(rhs))
    //         | (NodeType::Metatype(lhs), NodeType::Metatype(rhs))
    //             if lhs == rhs =>
    //         {
    //             true
    //         }
    //         (NodeType::Pointer(lhs), NodeType::Pointer(rhs)) => lhs.exact_match(&rhs),
    //         (NodeType::Function(lhs_params, lhs_ret), NodeType::Function(rhs_params, rhs_ret))
    //             if lhs_params.len() == rhs_params.len() =>
    //         {
    //             if !lhs_ret.exact_match(&rhs_ret) {
    //                 return false;
    //             }

    //             for (lhs_param, rhs_param) in lhs_params.iter().zip(rhs_params) {
    //                 if !lhs_param.exact_match(&rhs_param) {
    //                     return false;
    //                 }
    //             }

    //             true
    //         }
    //         (NodeType::Array(lhs_kind, lhs_size), NodeType::Array(rhs_kind, rhs_size)) => {
    //             if !lhs_kind.exact_match(rhs_kind) {
    //                 return false;
    //             }

    //             lhs_size == rhs_size
    //         },
    //         _ => false,
    //     }
    // }

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

                lhs_size == rhs_size
            }
            (NodeType::Generic(f1, n1), NodeType::Generic(f2, n2)) if f1 == f2 && n1 == n2 => true,
            (NodeType::Ambiguous, _) => true,
            (_, NodeType::Any) => true,
            _ => false,
        }
    }

    pub fn specialize_opt(&self, specialization: Option<&GenericSpecialization>) -> NodeType {
        match specialization {
            Some(spec) => self.specialize(spec),
            None => self.clone(),
        }
    }

    pub fn specialize(&self, specialization: &GenericSpecialization) -> NodeType {
        match self {
            NodeType::Generic(_, index) => specialization.node_types[*index].clone(),
            NodeType::Pointer(to) => NodeType::pointer_to(to.specialize(specialization)),
            NodeType::Array(of, size) => {
                let specialized = of.specialize(specialization);
                return NodeType::Array(Box::new(specialized), *size);
            }
            _ => self.clone(),
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

    pub fn coerce_array_to_ptr(&self) -> NodeType {
        match self {
            NodeType::Array(inner, _) => NodeType::Pointer(inner.clone()),
            _ => self.clone(),
        }
    }

    pub fn symbolic_form(&self) -> String {
        match self {
            NodeType::Type(ty) => ty.mangled(),
            NodeType::Int | NodeType::Void | NodeType::Bool | NodeType::Byte => self.to_string(),
            NodeType::Pointer(to) => format!("ptr__{}", to.symbolic_form()),
            NodeType::Generic(sy, index) => format!("{}__{}", sy.mangled(), index),
            _ => panic!("can't get symbolic form of type {}", self),
        }
    }

    pub fn infer_generic_type(param: &NodeType, arg: &NodeType) -> Option<(usize, NodeType)> {
        match (param, arg) {
            (NodeType::Generic(_, index), arg) => Some((*index, arg.clone())),
            (NodeType::Pointer(param_to), NodeType::Pointer(arg_to)) => {
                NodeType::infer_generic_type(param_to, arg_to)
            }
            _ => None,
        }
    }

    // pub fn can_automatically_cast_to(&self, other: &NodeType) -> {
    //     if self.exact_match(other) {
    //         true
    //     } else {

    //     }
    // }
}

impl std::fmt::Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let kind = match self {
            NodeType::Void => String::from("void"),
            NodeType::Int => String::from("int"),
            NodeType::Bool => String::from("bool"),
            NodeType::Byte => String::from("byte"),
            NodeType::Any => String::from("any"),
            NodeType::Function(params, ret) => {
                let mut string = String::from("def");
                string += "(";
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
            NodeType::Type(ty) => format!("Type({})", ty.id),
            NodeType::Metatype(ty) => format!("Metatype({})", ty.id),
            NodeType::Generic(symbol, index) => format!("Generic({}, index: {})", symbol, index),
            NodeType::GenericMeta(symbol, index) => {
                format!("GenericMeta({}, index: {})", symbol, index)
            }
            NodeType::Ambiguous => String::from("_"),
            NodeType::FlexibleFunction(_) => String::from("<flexible function>"),
        };
        write!(f, "{}", kind)
    }
}
