use super::metadata::*;
use super::symbol_table::*;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::*;
use log::trace;

#[derive(Clone, Debug)]
pub struct FunctionType {
    pub parameters: Vec<NodeType>,
    pub return_type: NodeType,
}

impl FunctionType {
    pub fn new(parameters: Vec<NodeType>, return_type: NodeType) -> Self {
        FunctionType {
            parameters,
            return_type,
        }
    }

    pub fn specialize(&self, lib: &Lib, spec: &GenericSpecialization) -> Self {
        FunctionType {
            parameters: self
                .parameters
                .iter()
                .map(|p| p.specialize(lib, spec))
                .collect(),
            return_type: self.return_type.specialize(lib, spec),
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

#[derive(Clone, Debug)]
pub enum NodeType {
    Void,
    Int,
    Double,
    Bool,
    Byte,
    Instance(Symbol, GenericSpecialization),
    Metatype(Symbol, GenericSpecialization),
    Pointer(Box<NodeType>),
    Array(Box<NodeType>, usize),
    Function(Box<FunctionType>),
    // FlexibleFunction(fn(&[NodeType]) -> bool),
    Ambiguous,
    Any,
}

impl NodeType {
    pub fn primitive(token: &Token) -> Option<Self> {
        match token.lexeme() {
            "void" => Some(NodeType::Void),
            "int" => Some(NodeType::Int),
            "double" => Some(NodeType::Double),
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
            return_type,
        };
        NodeType::Function(Box::new(function))
    }

    pub fn deduce_from(
        explicit_type: &ExplicitType,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
    ) -> Option<Self> {
        let node_type = match &explicit_type.kind {
            ExplicitTypeKind::Simple(token) => {
                NodeType::deduce_from_simple_explicit(token, table, deps, context)?
            }
            ExplicitTypeKind::Pointer(to) => {
                let inner = NodeType::deduce_from(to.as_ref(), table, deps, context)?;
                NodeType::Pointer(Box::new(inner))
            }
            ExplicitTypeKind::Array(of, count_token) => {
                let inner = NodeType::deduce_from(of.as_ref(), table, deps, context)?;
                let count = count_token.lexeme().parse::<usize>().ok().unwrap();
                NodeType::Array(Box::new(inner), count)
            }
        };

        Some(node_type)
    }

    pub fn deduce_from_simple_explicit(
        token: &ResolvedToken,
        table: &SymbolTable,
        deps: &[Lib],
        context: &[Symbol],
    ) -> Option<Self> {
        let mut resolved_spec = Vec::new();
        for explict_spec in &token.specialization {
            resolved_spec.push(explict_spec.resolve(table, deps, context)?);
        }

        if let Some(primitive) = NodeType::primitive(&token.token) {
            Some(primitive)
        } else {
            NodeType::search_for_type_token(&token.token, table, deps, context, resolved_spec)
        }
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
            if let Some(type_metadata) = table.get_type_metadata(&non_top_level_symbol) {
                let instance_type = NodeType::Instance(
                    non_top_level_symbol.clone(),
                    GenericSpecialization::new(&type_metadata.generics, specialization),
                );
                trace!(target: "symbol_table", "Resolving {} as {}", token.lexeme(), instance_type);
                return Some(instance_type);
            }
        }

        let top_level_symbol = Symbol::new(None, token);
        if let Some(type_metadata) = table.get_type_metadata(&top_level_symbol) {
            let spec = GenericSpecialization::new(&type_metadata.generics, specialization);
            let instance_type = NodeType::Instance(top_level_symbol.clone(), spec);

            trace!(target: "symbol_table", "Resolving {} as Type({})", token.lexeme(), instance_type);
            Some(instance_type)
        } else {
            for dep in deps {
                if let Some(type_metadata) = dep.type_metadata(&top_level_symbol) {
                    let instance_type = NodeType::Instance(
                        top_level_symbol.clone(),
                        GenericSpecialization::new(&type_metadata.generics, specialization),
                    );
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
            | (NodeType::Double, NodeType::Double)
            | (NodeType::Bool, NodeType::Bool)
            | (NodeType::Byte, NodeType::Byte) => true,
            (NodeType::Instance(lhs, lhs_spec), NodeType::Instance(rhs, rhs_spec))
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
            (NodeType::Ambiguous, _) => true,
            (_, NodeType::Any) => true,
            _ => false,
        }
    }

    pub fn specialize_opt(
        &self,
        lib: &Lib,
        specialization: Option<&GenericSpecialization>,
    ) -> NodeType {
        match specialization {
            Some(spec) => self.specialize(lib, spec),
            None => self.clone(),
        }
    }

    pub fn specialize(&self, lib: &Lib, specialization: &GenericSpecialization) -> NodeType {
        match self {
            NodeType::Instance(symbol, spec) => {
                if let Some(specialized_type) = specialization.type_for(symbol) {
                    // spec will be empty; generic parameters aren't specialized themselves, so it can be ignored
                    specialized_type.clone()
                } else {
                    let new_spec = spec.resolve_generics_using(lib, specialization);
                    NodeType::Instance(symbol.clone(), new_spec)
                }
            }
            NodeType::Metatype(symbol, spec) => {
                if let Some(specialized_type) = specialization.type_for(symbol) {
                    // spec will be empty; generic parameters aren't specialized themselves, so it can be ignored
                    specialized_type.clone()
                } else {
                    let new_spec = spec.resolve_generics_using(lib, specialization);
                    NodeType::Metatype(symbol.clone(), new_spec)
                }
            }
            NodeType::Pointer(to) => NodeType::pointer_to(to.specialize(lib, specialization)),
            NodeType::Array(of, size) => {
                let specialized = of.specialize(lib, specialization);
                NodeType::Array(Box::new(specialized), *size)
            }
            NodeType::Function(func_type) => {
                NodeType::Function(Box::new(func_type.specialize(lib, specialization)))
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
            NodeType::Instance(ty, spec) => {
                if spec.map.is_empty() {
                    ty.mangled()
                } else {
                    ty.mangled() + "__" + &spec.symbolic_list()
                }
            }
            NodeType::Int | NodeType::Double | NodeType::Void | NodeType::Bool | NodeType::Byte => {
                self.to_string()
            }
            NodeType::Pointer(to) => format!("ptr__{}", to.symbolic_form()),
            _ => panic!("can't get symbolic form of type {}", self),
        }
    }

    pub fn infer_generic_type(
        lib: &Lib,
        param: &NodeType,
        arg: &NodeType,
    ) -> Option<(Symbol, NodeType)> {
        match (param, arg) {
            (NodeType::Instance(symbol, _), arg) => {
                if let Some(generic_def) = lib.type_metadata(symbol) {
                    Some((generic_def.symbol.clone(), arg.clone()))
                } else {
                    None
                }
            }
            (NodeType::Pointer(param_to), NodeType::Pointer(arg_to)) => {
                NodeType::infer_generic_type(lib, param_to, arg_to)
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
            NodeType::Double => String::from("double"),
            NodeType::Bool => String::from("bool"),
            NodeType::Byte => String::from("byte"),
            NodeType::Any => String::from("any"),
            NodeType::Function(func_type) => func_type.to_string(),
            NodeType::Pointer(ty) => format!("ptr<{}>", ty),
            NodeType::Array(ty, size) => format!("Array<{}, count={}>", ty, size),
            NodeType::Instance(ty, specialization) => {
                let mut string = ty.id.clone();
                if !specialization.map.is_empty() {
                    string += &format!("[{}]", specialization.display_list());
                }
                string
            }
            NodeType::Metatype(ty, spec) => format!("Metatype({}, spec: {})", ty.id, spec),
            NodeType::Ambiguous => String::from("_"),
            // NodeType::FlexibleFunction(_) => String::from("<flexible function>"),
        };
        write!(f, "{}", kind)
    }
}

// fn str_or_none<T: std::fmt::Display>(opt: Option<&T>) -> String {
//     match opt {
//         Some(value) => value.to_string(),
//         None => String::from("<none>")
//     }
// }
