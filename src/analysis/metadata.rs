use crate::analysis::{NodeType, Symbol};

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
    pub fn specialize(&self, specialization: &GenericSpecialization) -> (Vec<NodeType>, NodeType) {
        let params: Vec<NodeType> = self
            .parameter_types
            .iter()
            .map(|node_type| node_type.specialize(specialization))
            .collect();
        let ret = self.return_type.specialize(specialization);
        (params, ret)
    }

    pub fn function_name(&self, specialization: Option<&GenericSpecialization>) -> String {
        specialization
            .map(|s| s.id.clone())
            .unwrap_or(self.symbol.mangled())
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
            format!("|{}|", generics.join(","))
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
pub struct TypeMetadata {
    pub symbol: Symbol,
    // pub generics: Vec<Symbol>,
    pub field_symbols: Vec<Symbol>,
    pub field_types: Vec<NodeType>,
    pub methods: Vec<Symbol>,
    pub meta_methods: Vec<Symbol>,
}

impl std::fmt::Display for TypeMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Type {}", self.symbol.mangled())?;
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
pub struct GenericSpecialization {
    pub function: Symbol,
    pub id: String,
    pub node_types: Vec<NodeType>,
}

impl GenericSpecialization {
    pub fn new(function: &Symbol, node_types: Vec<NodeType>) -> Self {
        let special_part = node_types
            .iter()
            .map(|s| s.symbolic_form())
            .collect::<Vec<_>>()
            .join("__");

        GenericSpecialization {
            function: function.clone(),
            id: function.mangled() + &special_part,
            node_types,
        }
    }

    pub fn infer(metadata: &FunctionMetadata, arg_types: &[NodeType]) -> Result<Self, usize> {
        let mut specializations: Vec<_> = std::iter::repeat(NodeType::Ambiguous)
            .take(metadata.generics.len())
            .collect();
        for (param_type, arg_type) in metadata.parameter_types.iter().zip(arg_types).rev() {
            if let Some((index, specialized_type)) =
                NodeType::infer_generic_type(param_type, arg_type)
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
        specialization: &GenericSpecialization,
    ) -> GenericSpecialization {
        let node_types: Vec<_> = self
            .node_types
            .iter()
            .map(|arg_type| match arg_type {
                NodeType::Generic(_, index) => specialization.node_types[*index].clone(),
                _ => arg_type.clone(),
            })
            .collect();

        GenericSpecialization::new(&self.function, node_types)
    }
}

impl PartialEq for GenericSpecialization {
    fn eq(&self, rhs: &Self) -> bool {
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
        write!(f, "GenericSpecialization({})", descrs)
    }
}
