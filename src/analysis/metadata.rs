use crate::analysis::{NodeType, Symbol, FunctionType};
use crate::library::Lib;

#[derive(Clone)]
pub struct TypeMetadata {
    pub symbol: Symbol,
    pub generics: Vec<Symbol>,
    pub field_symbols: Vec<Symbol>,
    pub field_types: Vec<NodeType>,
    pub methods: Vec<Symbol>,
    pub meta_methods: Vec<Symbol>,
    pub generic_index: Option<usize>,
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
            generic_index: None
        }
    }

    pub fn generic(owner: &Symbol, name: &str, index: usize) -> Self {
        TypeMetadata {
            symbol: Symbol::new_str(Some(owner), name),
            generics: Vec::new(),
            field_symbols: Vec::new(),
            field_types: Vec::new(),
            methods: Vec::new(),
            meta_methods: Vec::new(),
            generic_index: Some(index)
        }
    }

    pub fn field_named(&self, name: &str) -> Option<(Symbol, &NodeType)> {
        let possible_symbol = Symbol::new_str(Some(&self.symbol), name);
        if let Some(index) = self.field_symbols.iter().position(|s| s == &possible_symbol) {
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

    pub fn is_generic_parameter(&self) -> bool {
        self.generic_index.is_some()
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
        write!(f, "  meta methods: {}", meta_methods)
    }
}


#[derive(Clone)]
pub enum FunctionKind {
    TopLevel,
    Method(Symbol),
    MetaMethod(Symbol),
}

#[derive(Clone)]
pub struct FunctionMetadata {
    pub symbol: Symbol,
    pub kind: FunctionKind,
    pub generics: Vec<Symbol>,
    pub parameter_symbols: Vec<Symbol>,
    pub parameter_types: Vec<NodeType>,
    pub return_type: NodeType,
    pub specializations: Vec<GenericSpecialization>,
}

impl FunctionMetadata {
    pub fn function_name(&self, specialization: &GenericSpecialization) -> String {
        specialization.id.clone()
    }

    pub fn full_type(&self) -> FunctionType {
        FunctionType::new(self.parameter_types.clone(), self.return_type.clone())
    }
}

impl std::fmt::Display for FunctionMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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

        let start = match &self.kind {
            FunctionKind::TopLevel => String::from("Function("),
            FunctionKind::Method(owner) => format!("Method(object: {}, ", owner.mangled()),
            FunctionKind::MetaMethod(owner) => format!("MetaMethod(object: {}, ", owner.mangled()),
        };

        let parameters = parameters.join(",");
        write!(
            f,
            "{}def {}{}({}): {})",
            start,
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

#[derive(Clone)]
pub struct GenericSpecialization {
    pub owner: Symbol,
    pub id: String,
    pub node_types: Vec<NodeType>,
}

impl GenericSpecialization {
    pub fn new(owner: &Symbol, node_types: Vec<NodeType>) -> Self {
        let special_part = node_types
            .iter()
            .map(|s| s.symbolic_form())
            .collect::<Vec<_>>()
            .join("__");

        GenericSpecialization {
            owner: owner.clone(),
            id: owner.mangled() + &special_part,
            node_types,
        }
    }

    pub fn empty(owner: &Symbol) -> Self {
        GenericSpecialization {
            owner: owner.clone(),
            id: owner.mangled(),
            node_types: Vec::new()
        }
    }

    pub fn infer(lib: &Lib, metadata: &FunctionMetadata, arg_types: &[NodeType]) -> Result<Self, usize> {
        let mut specializations: Vec<_> = std::iter::repeat(NodeType::Ambiguous)
            .take(metadata.generics.len())
            .collect();
        for (param_type, arg_type) in metadata.parameter_types.iter().zip(arg_types).rev() {
            if let Some((index, specialized_type)) =
                NodeType::infer_generic_type(lib, param_type, arg_type)
            {
                specializations[index] = specialized_type;
            }
        }
        for (index, spec) in specializations.iter().enumerate() {
            if spec.contains_ambiguity() {
                return Err(index);
            }
        }

        Ok(GenericSpecialization::new(
            &metadata.symbol,
            specializations,
        ))
    }

    pub fn resolve_generics_using(
        &self,
        lib: &Lib,
        specialization: &GenericSpecialization,
    ) -> GenericSpecialization {
        let node_types: Vec<_> = self
            .node_types
            .iter()
            .map(|arg_type| match arg_type {
                NodeType::Instance(symbol, _) if specialization.owner.owns(symbol) => {
                    let generic_index = lib.type_metadata(symbol).and_then(|t| t.generic_index).unwrap();
                    specialization.node_types[generic_index].clone()
                },
                _ => arg_type.clone(),
            })
            .collect();

        GenericSpecialization::new(&self.owner, node_types)
    }
}

impl PartialEq for GenericSpecialization {
    fn eq(&self, rhs: &Self) -> bool {
        println!("Compraing {} to {}", self.id, rhs.id);
        self.id == rhs.id
    }
}

impl std::fmt::Display for GenericSpecialization {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let descrs = self
            .node_types
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(",");
        write!(f, "GenericSpecialization(owner: {}, specs: {})", self.owner, descrs)
    }
}
