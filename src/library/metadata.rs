use super::{FunctionType, Lib, NodeType, Symbol};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct TypeMetadata {
    pub symbol: Symbol,
    pub generics: Vec<Symbol>,
    pub field_symbols: Vec<Symbol>,
    pub field_types: Vec<NodeType>,
    pub methods: Vec<Symbol>,
    pub meta_methods: Vec<Symbol>,
    pub specializations: HashSet<GenericSpecialization>,
}

impl TypeMetadata {
    pub fn new(symbol: Symbol) -> Self {
        TypeMetadata {
            symbol: symbol,
            generics: Vec::new(),
            field_symbols: Vec::new(),
            field_types: Vec::new(),
            methods: Vec::new(),
            meta_methods: Vec::new(),
            specializations: HashSet::new(),
        }
    }

    pub fn generic(owner: &Symbol, name: &str) -> Self {
        TypeMetadata {
            symbol: Symbol::new_str(Some(owner), name),
            generics: Vec::new(),
            field_symbols: Vec::new(),
            field_types: Vec::new(),
            methods: Vec::new(),
            meta_methods: Vec::new(),
            specializations: HashSet::new(),
        }
    }

    pub fn type_name(&self, specialization: &GenericSpecialization) -> String {
        let specialization = self
            .generics
            .iter()
            .map(|g| {
                specialization
                    .type_for(g)
                    .expect(&format!(
                        "Expected type for generic {}\n{}",
                        g, specialization
                    ))
                    .symbolic_form()
            })
            .collect::<Vec<_>>();
        if specialization.is_empty() {
            self.symbol.mangled()
        } else {
            self.symbol.mangled() + "__" + &specialization.join("__")
        }
    }

    pub fn field_named(&self, name: &str) -> Option<(Symbol, &NodeType)> {
        let possible_symbol = Symbol::new_str(Some(&self.symbol), name);
        if let Some(index) = self
            .field_symbols
            .iter()
            .position(|s| s == &possible_symbol)
        {
            Some((possible_symbol, &self.field_types[index]))
        } else {
            None
        }
    }

    pub fn method_named(&self, name: &str) -> Option<Symbol> {
        let possible_symbol = Symbol::new_str(Some(&self.symbol), name);
        if self.methods.contains(&possible_symbol) {
            Some(possible_symbol)
        } else {
            None
        }
    }

    pub fn meta_method_named(&self, name: &str) -> Option<Symbol> {
        let possible_symbol = Symbol::new_str(Some(&Symbol::meta_symbol(Some(&self.symbol))), name);
        if self.meta_methods.contains(&possible_symbol) {
            Some(possible_symbol)
        } else {
            None
        }
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
        writeln!(f, "Type {}", self.symbol.mangled())?;
        if !self.generics.is_empty() {
            let gens = self
                .generics
                .iter()
                .map(|symbol| format!("{}", symbol.last_component()))
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
        write!(f, "  meta methods: {}", meta_methods)?;

        if !self.specializations.is_empty() {
            for spec in &self.specializations {
                write!(f, "\n  {}", spec)?;
            }
        }

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
    pub specializations: HashSet<GenericSpecialization>,
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
            specializations: HashSet::new(),
        }
    }

    pub fn function_name(&self, lib: &Lib, specialization: &GenericSpecialization) -> String {
        let func_specialization = self
            .generics
            .iter()
            .map(|g| specialization.type_for(g).unwrap().symbolic_form())
            .collect::<Vec<_>>()
            .join("__");

        match &self.kind {
            FunctionKind::Method(owner) | FunctionKind::MetaMethod(owner) => {
                let type_meta = lib.type_metadata(&owner).unwrap();
                format!(
                    "{}__{}__{}",
                    type_meta.type_name(specialization),
                    self.symbol.last_component(),
                    func_specialization
                )
            }
            FunctionKind::TopLevel => self.symbol.mangled() + &func_specialization,
        }
    }

    pub fn full_type(&self) -> FunctionType {
        FunctionType::new(self.parameter_types.clone(), self.return_type.clone())
    }

    pub fn node_type(&self) -> NodeType {
        NodeType::Function(Box::new(self.full_type()))
    }
}

impl std::fmt::Display for FunctionMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            FunctionKind::TopLevel => writeln!(f, "Function({})", self.symbol.mangled()),
            FunctionKind::Method(owner) => writeln!(
                f,
                "Method(object: {}, method: {})",
                owner.mangled(),
                self.symbol.last_component()
            ),
            FunctionKind::MetaMethod(owner) => writeln!(
                f,
                "MetaMethod(object: {}, meta_method: {})",
                owner.mangled(),
                self.symbol.last_component()
            ),
        }?;

        let generics: Vec<String> = self
            .generics
            .iter()
            .map(|g| g.last_component().to_string())
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

        if !self.specializations.is_empty() {
            for spec in &self.specializations {
                write!(f, "\n  {}", spec)?;
            }
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
        lib: &Lib,
        specialization: &GenericSpecialization,
    ) -> GenericSpecialization {
        GenericSpecialization {
            map: self
                .map
                .iter()
                .map(|(key, value)| (key.clone(), value.specialize(lib, specialization)))
                .collect(),
        }
    }

    pub fn merge(
        &self,
        lib: &Lib,
        specialization: &GenericSpecialization,
    ) -> GenericSpecialization {
        let mut resolved_self = self.resolve_generics_using(lib, specialization);
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
                if owner.owns(symbol) {
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
                    symbol.last_component(),
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
