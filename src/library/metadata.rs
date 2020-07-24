use super::{FunctionType, NodeType, Symbol};
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug)]
pub struct VarMetadata {
    pub name: String,
    pub var_type: NodeType,
    pub public: bool,
}

impl fmt::Display for VarMetadata {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}: {} (public = {})",
            self.name, self.var_type, self.public
        )
    }
}

#[derive(Clone, Debug)]
pub struct TypeMetadata {
    pub symbol: Symbol,
    pub generics: Vec<String>,
    pub fields: Vec<VarMetadata>,
    pub methods: Vec<String>,
    pub meta_methods: Vec<String>,
    pub trait_impls: Vec<Symbol>,
    pub is_public: bool,
}

impl TypeMetadata {
    pub fn new(symbol: Symbol, is_public: bool) -> Self {
        TypeMetadata {
            symbol: symbol,
            generics: Vec::new(),
            fields: Vec::new(),
            methods: Vec::new(),
            meta_methods: Vec::new(),
            trait_impls: vec![Symbol::any_object_symbol()],
            is_public: is_public,
        }
    }

    pub fn generic(owner: &Symbol, name: &str) -> Self {
        TypeMetadata::new(owner.child(name), false)
    }

    pub fn field_named(&self, name: &str) -> Option<&VarMetadata> {
        self.fields.iter().find(|f| &f.name == name)
    }

    pub fn method_named(&self, name: &str) -> Option<&String> {
        self.methods.iter().find(|m| m.as_str() == name)
    }

    pub fn method_symbols<'a>(&'a self) -> impl Iterator<Item = Symbol> + 'a {
        let sym = self.symbol.clone();
        self.methods.iter().map(move |m| sym.child(&m))
    }

    pub fn meta_method_named(&self, name: &str) -> Option<&String> {
        self.meta_methods.iter().find(|m| m.as_str() == name)
    }

    pub fn meta_method_symbols<'a>(&'a self) -> impl Iterator<Item = Symbol> + 'a {
        let sym = self.symbol.meta_symbol();
        self.meta_methods.iter().map(move |m| sym.child(&m))
    }

    pub fn symbol_for_field(&self, var: &VarMetadata) -> Symbol {
        self.symbol.child(&var.name)
    }

    pub fn conforms_to(&self, trait_symbol: &Symbol) -> bool {
        self.trait_impls.contains(trait_symbol)
    }

    pub fn dummy_specialization(&self) -> GenericSpecialization {
        let dummy_generics = self
            .generics
            .iter()
            .map(|g| {
                let sym = self.symbol.child(&g);
                NodeType::GenericInstance(sym)
            })
            .collect();
        GenericSpecialization::new(&self.symbol, &self.generics, dummy_generics)
    }

    pub fn unspecialized_type(&self) -> NodeType {
        NodeType::Instance(self.symbol.clone(), self.dummy_specialization())
    }

    pub fn ref_to_unspecialized_type(&self) -> NodeType {
        NodeType::reference_to(self.unspecialized_type())
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
            let gens = self.generics.join(",");
            writeln!(f, "  generics: {}", gens)?;
        }
        let fields = self
            .fields
            .iter()
            .map(|field| field.to_string())
            .collect::<Vec<_>>()
            .join(",");
        writeln!(f, "  fields: {}", fields)?;
        let methods = self.methods.join(",");
        writeln!(f, "  methods: {}", methods)?;
        let meta_methods = self.meta_methods.join(",");
        writeln!(f, "  meta methods: {}", meta_methods)?;
        let trait_impls = self
            .trait_impls
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
    pub generics: Vec<String>,
    pub parameters: Vec<VarMetadata>,
    pub return_type: NodeType,
    pub generic_restrictions: Vec<(Symbol, Symbol)>,
    pub is_public: bool,
    pub include_caller: bool,
}

impl FunctionMetadata {
    pub fn full_type(&self) -> FunctionType {
        let parameter_types: Vec<_> = self.parameters.iter().map(|p| p.var_type.clone()).collect();
        FunctionType::new(parameter_types, self.return_type.clone())
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
                self.symbol.name()
            ),
            FunctionKind::MetaMethod(owner) => format!(
                "MetaMethod(object: {}, meta_method: {}",
                owner.mangled(),
                self.symbol.name()
            ),
        };
        writeln!(f, "{}, public: {})", line, self.is_public)?;

        let generic_porition = if self.generics.is_empty() {
            String::new()
        } else {
            format!("[{}]", self.generics.join(","))
        };
        let parameters: Vec<String> = self
            .parameters
            .iter()
            .map(|param| param.to_string())
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

pub struct TraitMetadata {
    pub symbol: Symbol,
    pub function_requirements: Vec<String>,
}

impl fmt::Display for TraitMetadata {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Trait({})", self.symbol.mangled())?;
        for req in &self.function_requirements {
            writeln!(f, "  {}", req)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct GenericSpecialization {
    pub map: HashMap<Symbol, NodeType>,
}

impl GenericSpecialization {
    pub fn new(owner: &Symbol, generic_list: &[String], node_types: Vec<NodeType>) -> Self {
        let mut map: HashMap<Symbol, NodeType> = HashMap::new();
        for (g, node_type) in generic_list.iter().zip(node_types) {
            let symbol = owner.child(&g);
            map.insert(symbol, node_type.clone());
        }
        GenericSpecialization { map }
    }

    pub fn empty() -> Self {
        GenericSpecialization {
            map: HashMap::new(),
        }
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

    pub fn type_for(&self, symbol: &Symbol) -> Option<&NodeType> {
        self.map.get(symbol)
    }

    pub fn display_list(&self) -> String {
        let mut keys: Vec<_> = self.map.keys().collect();
        keys.sort();
        keys.iter()
            .map(|symbol| format!("{}={}", symbol.name(), self.map.get(symbol).unwrap()))
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
        let mut keys: Vec<_> = self.map.keys().collect();
        keys.sort();
        let list = keys.iter()
            .map(|symbol| format!("{}={}", symbol.unique_id(), self.map.get(symbol).unwrap()))
            .collect::<Vec<String>>()
            .join(",");
        write!(f, "GenericSpecialization({})", list)
    }
}
