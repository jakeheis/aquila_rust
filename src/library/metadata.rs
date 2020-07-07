use super::{FunctionType, Lib, NodeType, Symbol};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
pub struct TypeMetadata {
    pub symbol: Symbol,
    pub generics: Vec<Symbol>,
    pub field_symbols: Vec<Symbol>,
    pub field_types: Vec<NodeType>,
    pub field_visibilities: Vec<bool>,
    pub methods: Vec<Symbol>,
    pub meta_methods: Vec<Symbol>,
    pub trait_impls: RefCell<Vec<Symbol>>,
    pub is_public: bool,
}

impl TypeMetadata {
    pub fn new(symbol: Symbol, is_public: bool) -> Self {
        TypeMetadata {
            symbol: symbol,
            generics: Vec::new(),
            field_symbols: Vec::new(),
            field_types: Vec::new(),
            field_visibilities: Vec::new(),
            methods: Vec::new(),
            meta_methods: Vec::new(),
            trait_impls: RefCell::new(Vec::new()),
            is_public: is_public,
        }
    }

    pub fn generic(owner: &Symbol, name: &str) -> Self {
        TypeMetadata::new(owner.child(name), false)
    }

    pub fn field_named(&self, name: &str) -> Option<(Symbol, &NodeType, bool)> {
        let possible_symbol = self.symbol.child(name);
        if let Some(index) = self
            .field_symbols
            .iter()
            .position(|s| s == &possible_symbol)
        {
            Some((
                possible_symbol,
                &self.field_types[index],
                self.field_visibilities[index],
            ))
        } else {
            None
        }
    }

    pub fn method_named(&self, name: &str) -> Option<Symbol> {
        let possible_symbol = self.symbol.child(name);
        if self.methods.contains(&possible_symbol) {
            Some(possible_symbol)
        } else {
            None
        }
    }

    pub fn meta_method_named(&self, name: &str) -> Option<Symbol> {
        let possible_symbol = self.symbol.meta_symbol().child(name);
        if self.meta_methods.contains(&possible_symbol) {
            Some(possible_symbol)
        } else {
            None
        }
    }

    pub fn add_trait_impl(&self, trait_symbol: &Symbol) {
        self.trait_impls.borrow_mut().push(trait_symbol.clone());
    }

    pub fn conforms_to(&self, trait_symbol: &Symbol) -> bool {
        self.trait_impls.borrow().contains(trait_symbol)
    }

    pub fn dummy_specialization(&self) -> GenericSpecialization {
        let dummy_generics = self
            .generics
            .iter()
            .map(|g| NodeType::Instance(g.clone(), GenericSpecialization::empty()))
            .collect();
        GenericSpecialization::new(&self.generics, dummy_generics)
    }

    pub fn unspecialized_type(&self) -> NodeType {
        NodeType::Instance(self.symbol.clone(), self.dummy_specialization())
    }
}

impl std::fmt::Display for TypeMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(
            f,
            "Type({}, public: {})",
            self.symbol.mangled(),
            self.is_public
        )?;
        if !self.generics.is_empty() {
            let gens = self
                .generics
                .iter()
                .map(|symbol| format!("{}", symbol.name))
                .collect::<Vec<_>>()
                .join(",");
            writeln!(f, "  generics: {}", gens)?;
        }
        let fields = self
            .field_symbols
            .iter()
            .zip(&self.field_types)
            .map(|(symbol, field_type)| format!("{}: {}", symbol.mangled(), field_type))
            .collect::<Vec<_>>()
            .join(",");
        writeln!(f, "  fields: {}", fields)?;
        let methods = self
            .methods
            .iter()
            .map(|m| m.mangled())
            .collect::<Vec<_>>()
            .join(",");
        writeln!(f, "  methods: {}", methods)?;
        let meta_methods = self
            .meta_methods
            .iter()
            .map(|m| m.mangled())
            .collect::<Vec<_>>()
            .join(",");
        writeln!(f, "  meta methods: {}", meta_methods)?;
        let trait_impls = self
            .trait_impls
            .borrow()
            .iter()
            .map(|t| t.mangled())
            .collect::<Vec<_>>()
            .join(",");
        write!(f, "  trait impls: {}", trait_impls)?;

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum FunctionKind {
    TopLevel,
    Method(Symbol),
    MetaMethod(Symbol),
}

#[derive(Clone, Debug)]
pub struct FunctionMetadata {
    pub symbol: Symbol,
    pub kind: FunctionKind,
    pub generics: Vec<Symbol>,
    pub parameter_symbols: Vec<Symbol>,
    pub parameter_types: Vec<NodeType>,
    pub return_type: NodeType,
    pub is_public: bool,
    pub include_caller: bool,
}

impl FunctionMetadata {
    pub fn main() -> Self {
        FunctionMetadata {
            symbol: Symbol::main_symbol(),
            kind: FunctionKind::TopLevel,
            generics: Vec::new(),
            parameter_symbols: Vec::new(),
            parameter_types: Vec::new(),
            return_type: NodeType::Int,
            is_public: false,
            include_caller: false,
        }
    }

    pub fn full_type(&self) -> FunctionType {
        FunctionType::new(self.parameter_types.clone(), self.return_type.clone())
    }

    pub fn node_type(&self) -> NodeType {
        NodeType::Function(Box::new(self.full_type()))
    }
}

impl fmt::Display for FunctionMetadata {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let line = match &self.kind {
            FunctionKind::TopLevel => format!("Function({}", self.symbol.mangled()),
            FunctionKind::Method(owner) => format!(
                "Method(object: {}, method: {}",
                owner.mangled(),
                self.symbol.name
            ),
            FunctionKind::MetaMethod(owner) => format!(
                "MetaMethod(object: {}, meta_method: {}",
                owner.mangled(),
                self.symbol.name
            ),
        };
        writeln!(f, "{}, public: {})", line, self.is_public)?;

        let generics: Vec<&str> = self
            .generics
            .iter()
            .map(|g| g.name.as_str())
            .collect();
        let generic_porition = if generics.is_empty() {
            String::new()
        } else {
            format!("[{}]", generics.join(","))
        };
        let parameters: Vec<String> = self
            .parameter_symbols
            .iter()
            .zip(&self.parameter_types)
            .map(|(symbol, node_type)| format!("{}: {}", symbol.mangled(), node_type))
            .collect();

        let parameters = parameters.join(",");
        write!(
            f,
            "  def {}{}({}): {})",
            self.symbol.mangled(),
            generic_porition,
            parameters,
            self.return_type
        )?;

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct TraitMetadata {
    pub symbol: Symbol,
    pub function_requirements: Vec<Symbol>,
}

impl fmt::Display for TraitMetadata {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Trait({})", self.symbol.mangled())?;
        for req in &self.function_requirements {
            writeln!(f, "  {}", req.mangled())?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct GenericSpecialization {
    pub map: HashMap<Symbol, NodeType>,
}

impl GenericSpecialization {
    pub fn new(generic_list: &[Symbol], node_types: Vec<NodeType>) -> Self {
        let mut map: HashMap<Symbol, NodeType> = HashMap::new();
        for (symbol, node_type) in generic_list.iter().zip(node_types) {
            map.insert(symbol.clone(), node_type.clone());
        }
        GenericSpecialization { map }
    }

    pub fn empty() -> Self {
        GenericSpecialization {
            map: HashMap::new(),
        }
    }

    pub fn infer(
        lib: &Lib,
        metadata: &FunctionMetadata,
        arg_types: &[NodeType],
    ) -> Result<Self, Symbol> {
        let mut spec_map: HashMap<Symbol, NodeType> = HashMap::new();
        for gen in &metadata.generics {
            spec_map.insert(gen.clone(), NodeType::Ambiguous);
        }

        for (param_type, arg_type) in metadata.parameter_types.iter().zip(arg_types).rev() {
            if let Some((symbol, specialized_type)) =
                NodeType::infer_generic_type(lib, param_type, arg_type)
            {
                spec_map.insert(symbol, specialized_type);
            }
        }
        for (symbol, spec) in spec_map.iter() {
            if spec.contains_ambiguity() {
                return Err(symbol.clone());
            }
        }

        Ok(GenericSpecialization { map: spec_map })
    }

    pub fn resolve_generics_using(
        &self,
        specialization: &GenericSpecialization,
    ) -> GenericSpecialization {
        GenericSpecialization {
            map: self
                .map
                .iter()
                .map(|(key, value)| (key.clone(), value.specialize(specialization)))
                .collect(),
        }
    }

    pub fn merge(&self, specialization: &GenericSpecialization) -> GenericSpecialization {
        let mut resolved_self = self.resolve_generics_using(specialization);
        for (symbol, node_type) in &specialization.map {
            if !self.map.contains_key(symbol) {
                resolved_self.map.insert(symbol.clone(), node_type.clone());
            }
        }
        resolved_self
    }

    pub fn subset(&self, owner: &Symbol) -> Self {
        let map = self
            .map
            .iter()
            .flat_map(|(symbol, node_type)| {
                if owner.directly_owns(symbol) {
                    Some((symbol.clone(), node_type.clone()))
                } else {
                    None
                }
            })
            .collect();
        GenericSpecialization { map }
    }

    pub fn type_for(&self, symbol: &Symbol) -> Option<&NodeType> {
        self.map.get(symbol)
    }

    pub fn display_list(&self) -> String {
        let mut keys: Vec<_> = self.map.keys().collect();
        keys.sort();
        keys.iter()
            .map(|symbol| {
                format!(
                    "{}={}",
                    symbol.name,
                    self.map.get(symbol).unwrap()
                )
            })
            .collect::<Vec<String>>()
            .join(",")
    }

    pub fn symbolic_list(&self) -> String {
        let mut keys: Vec<_> = self.map.keys().collect();
        keys.sort();
        keys.iter()
            .map(|k| self.map.get(k).unwrap().symbolic_form())
            .collect::<Vec<String>>()
            .join("__")
    }
}

impl PartialEq for GenericSpecialization {
    fn eq(&self, rhs: &Self) -> bool {
        self.symbolic_list() == rhs.symbolic_list()
        // if self.map.len() != rhs.map.len() {
        //     return false;
        // }
        // for (symbol, node_type) in &self.map {
        //     match rhs.type_for(symbol) {
        //         Some(other_type) if node_type.matches(other_type) => (),
        //         _ => return false,
        //     }
        // }
        // true
    }
}

impl Eq for GenericSpecialization {}

impl std::hash::Hash for GenericSpecialization {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbolic_list().hash(state);
    }
}

impl std::fmt::Display for GenericSpecialization {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "GenericSpecialization({})", self.display_list())
    }
}
