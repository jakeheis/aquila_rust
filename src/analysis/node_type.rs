use super::metadata::*;
use super::symbol_table::*;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::*;
use log::trace;

#[derive(Clone)]
pub struct FunctionType {
    pub parameters: Vec<NodeType>,
    pub return_type: NodeType
}

impl FunctionType {
    pub fn new(parameters: Vec<NodeType>, return_type: NodeType) -> Self {
        FunctionType {
            parameters,
            return_type
        }
    }

    pub fn specialize(&self, spec: &GenericSpecialization) -> Self {
        FunctionType {
            parameters: self.parameters.iter().map(|p| p.specialize(spec)).collect(),
            return_type: self.return_type.specialize(spec),
        }
    }
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut string = String::from("def(");
        if let Some(first) = self.parameters.first() {
            string += &first.to_string()
        }
        self.parameters
            .iter()
            .skip(1)
            .for_each(|p| string += &format!(", {}", p));
        string += &format!("): {}", self.return_type);
        write!(f, "{}", string)
    }
}

#[derive(Clone)]
pub enum NodeType {
    Void,
    Int,
    Bool,
    Byte,
    Type(Symbol, Option<GenericSpecialization>),
    Metatype(Symbol, Option<GenericSpecialization>),
    Pointer(Box<NodeType>),
    Array(Box<NodeType>, usize),
    Function(Box<FunctionType>),
    Generic(Symbol, usize),
    GenericMeta(Symbol, usize),
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

    pub fn function(parameters: Vec<NodeType>, return_type: NodeType) -> Self {
        let function = FunctionType {
            parameters,
            return_type
        };
        NodeType::Function(Box::new(function))
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
            ExplicitTypeKind::Simple(token, specialization) => {
                let mut resolved_spec = Vec::new();
                for explict_spec in specialization {
                    resolved_spec.push(NodeType::deduce_from(explict_spec, table, deps, context)?);
                }

                if let Some(primitive) = NodeType::primitive(&token.token) {
                    primitive
                } else {
                    NodeType::search_for_type_token(&token.token, table, deps, context, resolved_spec)?
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

    fn search_for_type_token(
        token: &Token,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
        specialization: Vec<NodeType>,
    ) -> Option<Self> {
        trace!(target: "symbol_table", "Trying to find symbol for {} -- ({})", token.lexeme(), token.span.entire_line().0);

        for parent in context.iter().rev() {
            let non_top_level_symbol = Symbol::new(Some(parent), token);
            let resolved = table.get_type(&non_top_level_symbol);
            match resolved {
                Some(NodeType::Metatype(metatype, _)) => {
                    let spec = if specialization.is_empty() {
                        None
                    } else {
                        Some(GenericSpecialization::new(metatype, specialization))
                    };
                    let instance_type = NodeType::Type(metatype.clone(), spec);
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

        if let Some(NodeType::Metatype(metatype, _)) = table.get_type(&top_level_symbol) {
            let spec = if specialization.is_empty() {
                None
            } else {
                Some(GenericSpecialization::new(metatype, specialization))
            };
            let instance_type = NodeType::Type(metatype.clone(), spec);
            trace!(target: "symbol_table", "Resolving {} as Type({})", token.lexeme(), instance_type);
            Some(instance_type)
        } else {
            for dep in deps {
                if let Some(NodeType::Metatype(metatype, _)) = dep.resolve_symbol(&top_level_symbol) {
                    let spec = if specialization.is_empty() {
                        None
                    } else {
                        Some(GenericSpecialization::new(&metatype, specialization))
                    };
                    let instance_type = NodeType::Type(metatype.clone(), spec);
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
            NodeType::Function(func_type) => {
                for param in &func_type.parameters {
                    if param.contains_ambiguity() {
                        return true;
                    }
                }
                func_type.return_type.contains_ambiguity()
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
            (NodeType::Type(lhs, lhs_spec), NodeType::Type(rhs, rhs_spec))
            | (NodeType::Metatype(lhs, lhs_spec), NodeType::Metatype(rhs, rhs_spec))
                if lhs == rhs && lhs_spec == rhs_spec =>
            {
                true
            }
            (NodeType::Pointer(lhs), NodeType::Pointer(rhs)) => lhs.matches(&rhs),
            (NodeType::Function(lhs), NodeType::Function(rhs))
                if lhs.parameters.len() == rhs.parameters.len() =>
            {
                if !lhs.return_type.matches(&rhs.return_type) {
                    return false;
                }

                for (lhs_param, rhs_param) in lhs.parameters.iter().zip(&rhs.parameters) {
                    if !lhs_param.matches(rhs_param) {
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
                NodeType::Array(Box::new(specialized), *size)
            }
            NodeType::Function(func_type) =>{
                NodeType::Function(Box::new(func_type.specialize(specialization)))
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
            NodeType::Type(ty, spec) => {
                if let Some(spec) = spec.as_ref() {
                    ty.mangled() + "__" + &spec.id
                } else {
                    ty.mangled()
                }
            },
            NodeType::Int | NodeType::Void | NodeType::Bool | NodeType::Byte => self.to_string(),
            NodeType::Pointer(to) => format!("ptr__{}", to.symbolic_form()),
            NodeType::Generic(sy, index) => format!("{}__{}", sy.mangled(), index),
            _ => {
                // panic!("can't get symbolic form of type {}", self),
                String::new()
            }
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
}

impl std::fmt::Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let kind = match self {
            NodeType::Void => String::from("void"),
            NodeType::Int => String::from("int"),
            NodeType::Bool => String::from("bool"),
            NodeType::Byte => String::from("byte"),
            NodeType::Any => String::from("any"),
            NodeType::Function(func_type) => func_type.to_string(),
            NodeType::Pointer(ty) => format!("ptr<{}>", ty),
            NodeType::Array(ty, size) => format!("Array<{}, count={}>", ty, size),
            NodeType::Type(ty, specialization) => format!("Type({}, spec: {})", ty.id, str_or_none(specialization.as_ref())),
            NodeType::Metatype(ty, spec) => format!("Metatype({}, spec: {})", ty.id, str_or_none(spec.as_ref())),
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

fn str_or_none<T: std::fmt::Display>(opt: Option<&T>) -> String {
    match opt {
        Some(value) => value.to_string(),
        None => String::from("<none>")
    }
}
