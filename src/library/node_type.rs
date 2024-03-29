use super::metadata::GenericSpecialization;
use super::symbol_table::Symbol;

#[derive(Clone, Debug)]
pub enum NodeType {
    Void,
    Int,
    Double,
    Bool,
    Byte,
    Instance(Symbol, GenericSpecialization),
    GenericInstance(Symbol),
    Metatype(Box<NodeType>),
    Pointer(Box<NodeType>),
    Reference(Box<NodeType>),
    Array(Box<NodeType>, usize),
    Function(Box<FunctionType>),
    Ambiguous,
    Any,
}

impl NodeType {
    pub fn primitive(lexeme: &str) -> Option<Self> {
        match lexeme {
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

    pub fn reference_to(pointee: NodeType) -> Self {
        NodeType::Reference(Box::new(pointee))
    }

    pub fn function(parameters: Vec<NodeType>, return_type: NodeType) -> Self {
        let function = FunctionType {
            parameters,
            return_type,
        };
        NodeType::Function(Box::new(function))
    }

    pub fn contains_ambiguity(&self) -> bool {
        match self {
            NodeType::Ambiguous => true,
            NodeType::Pointer(obj) | NodeType::Reference(obj) | NodeType::Array(obj, _) => obj.contains_ambiguity(),
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

    pub fn matches(&self, unambiguous: &NodeType) -> bool {
        match (self, unambiguous) {
            (NodeType::Void, NodeType::Void)
            | (NodeType::Int, NodeType::Int)
            | (NodeType::Double, NodeType::Double)
            | (NodeType::Bool, NodeType::Bool)
            | (NodeType::Byte, NodeType::Byte) => true,
            (NodeType::Instance(lhs, lhs_spec), NodeType::Instance(rhs, rhs_spec))
                if lhs == rhs && lhs_spec == rhs_spec =>
            {
                true
            }
            (NodeType::GenericInstance(lhs), NodeType::GenericInstance(rhs)) if lhs == rhs => true,
            (NodeType::Metatype(lhs), NodeType::Metatype(rhs)) if lhs.matches(rhs) => true,
            (NodeType::Pointer(lhs), NodeType::Pointer(rhs)) 
            | (NodeType::Reference(lhs), NodeType::Reference(rhs)) => lhs.matches(&rhs),
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

    pub fn specialize(&self, specialization: &GenericSpecialization) -> NodeType {
        match self {
            NodeType::GenericInstance(sym) => {
                if let Some(node_type) = specialization.type_for(sym) {
                    node_type.clone()
                } else {
                    self.clone()
                }
            },
            NodeType::Instance(symbol, spec) => {
                let new_spec = spec.resolve_generics_using(specialization);
                NodeType::Instance(symbol.clone(), new_spec)
            }
            NodeType::Metatype(inner) => {
                let inner = inner.specialize(specialization);
                NodeType::Metatype(Box::new(inner))
            }
            NodeType::Pointer(to) => NodeType::pointer_to(to.specialize(specialization)),
            NodeType::Reference(to) => NodeType::reference_to(to.specialize(specialization)),
            NodeType::Array(of, size) => {
                let specialized = of.specialize(specialization);
                NodeType::Array(Box::new(specialized), *size)
            }
            NodeType::Function(func_type) => {
                NodeType::Function(Box::new(func_type.specialize(specialization)))
            }
            _ => self.clone(),
        }
    }

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
            NodeType::GenericInstance(sym) => sym.mangled(),
            NodeType::Int | NodeType::Double | NodeType::Void | NodeType::Bool | NodeType::Byte => {
                self.to_string()
            }
            NodeType::Pointer(to) => format!("ptr__{}", to.symbolic_form()),
            _ => panic!("can't get symbolic form of type {}", self),
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
            NodeType::Pointer(ty) => format!("ptr {}", ty),
            NodeType::Reference(ty) => format!("ref {}", ty),
            NodeType::Array(ty, size) => format!("Array[{}, count={}]", ty, size),
            NodeType::Instance(ty, specialization) => {
                let mut string = ty.unique_id().to_owned();
                if !specialization.map.is_empty() {
                    string += &format!("[{}]", specialization.display_list());
                }
                string
            }
            NodeType::GenericInstance(ty) => format!("Generic({})", ty.unique_id()),
            NodeType::Metatype(inner) => format!("{}.Metatype", inner),
            NodeType::Ambiguous => String::from("_"),
        };
        write!(f, "{}", kind)
    }
}

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
