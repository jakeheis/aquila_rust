use super::metadata::*;
use crate::lexing::Token;
use crate::source::Span;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Symbol {
    id: String,
}

impl Symbol {
    pub fn lib_root(name: &str) -> Self {
        Symbol {
            id: name.to_owned(),
        }
    }

    pub fn child_token(&self, name: &Token) -> Self {
        self.child(name.lexeme())
    }

    pub fn child(&self, name: &str) -> Self {
        Symbol {
            id: self.id.clone() + "$" + name,
        }
    }

    pub fn owner_symbol(&self) -> Option<Self> {
        let mut components = self.id.split("$").collect::<Vec<_>>();
        if components.pop().is_some() {
            Some(Symbol {
                id: components.join("$"),
            })
        } else {
            None
        }
    }

    pub fn main_symbol(lib: &str) -> Self {
        Symbol::lib_root(lib).child("main")
    }
    
    pub fn stdlib(name: &str) -> Self {
        Symbol::lib_root("stdlib").child(name)
    }

    pub fn writable_symbol() -> Self {
        Symbol::stdlib("Writable")
    }

    pub fn iterable_symbol() -> Self {
        Symbol::stdlib("Iterable")
    }

    pub fn any_object_symbol() -> Self {
        Symbol::stdlib("AnyObject")
    }

    pub fn meta_symbol(&self) -> Self {
        self.child("Meta")
    }

    pub fn init_symbol(&self) -> Self {
        self.child("init")
    }

    pub fn deinit_symbol(&self) -> Self {
        self.child("deinit")
    }

    pub fn self_symbol(&self) -> Self {
        self.child("self")
    }

    pub fn caller_symbol(&self) -> Self {
        self.child("caller")
    }

    pub fn write_symbol(&self) -> Self {
        self.child("write")
    }

    pub fn unique_id(&self) -> &str {
        &self.id
    }

    pub fn mangled(&self) -> String {
        self.id.replace("$", "__")
    }

    pub fn is_self(&self) -> bool {
        self.name() == "self"
    }
    
    pub fn is_root(&self) -> bool {
        self.id.contains("$")
    }

    pub fn directly_owns(&self, child: &Symbol) -> bool {
        if let Some(owner) = child.owner_symbol() {
            self == &owner
        } else {
            false
        }
    }

    pub fn lib(&self) -> &str {
        self.id.split("$").next().unwrap()
    }

    pub fn name(&self) -> &str {
        self.id.rsplit("$").next().unwrap()
    }

    // pub fn is_in_lib(&self, lib: &str) -> bool {

    // }

    pub fn add_spec_suffix(&self, spec: &GenericSpecialization) -> String {
        let spec = spec.symbolic_list();
        if spec.len() > 0 {
            self.mangled() + "__" + &spec
        } else {
            self.mangled()
        }
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Symbol({})", self.unique_id())
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub lib: Symbol,
    pub type_metadata: HashMap<Symbol, TypeMetadata>,
    function_metadata: HashMap<Symbol, FunctionMetadata>,
    trait_metadata: HashMap<Symbol, TraitMetadata>,
    span_map: HashMap<Symbol, Span>,
}

impl SymbolTable {
    pub fn new(name: &str) -> Self {
        SymbolTable {
            lib: Symbol::lib_root(name),
            type_metadata: HashMap::new(),
            function_metadata: HashMap::new(),
            trait_metadata: HashMap::new(),
            span_map: HashMap::new(),
        }
    }

    pub fn insert_type_metadata(&mut self, symbol: Symbol, metadata: TypeMetadata) {
        self.type_metadata.insert(symbol, metadata);
    }

    pub fn get_type_metadata(&self, symbol: &Symbol) -> Option<&TypeMetadata> {
        self.type_metadata.get(symbol)
    }

    pub fn insert_func_metadata(&mut self, symbol: Symbol, metadata: FunctionMetadata) {
        self.function_metadata.insert(symbol, metadata);
    }

    pub fn get_func_metadata(&self, symbol: &Symbol) -> Option<&FunctionMetadata> {
        self.function_metadata.get(symbol)
    }

    pub fn insert_trait_metadata(&mut self, symbol: Symbol, metadata: TraitMetadata) {
        self.trait_metadata.insert(symbol, metadata);
    }

    pub fn get_trait_metadata(&self, symbol: &Symbol) -> Option<&TraitMetadata> {
        self.trait_metadata.get(symbol)
    }

    pub fn get_span(&self, symbol: &Symbol) -> Option<&Span> {
        self.span_map.get(symbol)
    }
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "SymbolTable:")?;
        writeln!(f, "Type metadata:")?;
        for (_, metadata) in self.type_metadata.iter() {
            writeln!(f, "{}", metadata)?;
        }
        writeln!(f, "Function metadata:")?;
        for (_, meta) in self.function_metadata.iter() {
            writeln!(f, "{}", meta)?;
        }
        writeln!(f, "Trait metadata:")?;
        for (_, meta) in self.trait_metadata.iter() {
            writeln!(f, "{}", meta)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct SymbolStore {
    sources: Vec<Rc<SymbolTable>>
}

impl SymbolStore {
    pub fn new() -> Self {
        Self {
            sources: vec![],
        }
    }

    pub fn add_source(&mut self, source: Rc<SymbolTable>) {
        self.sources.push(source);
    }
}

impl SymbolProvider for SymbolStore {
    fn search<'a, F, U>(&'a self, block: &F) -> Option<&U>
    where
        F: Fn(&'a SymbolTable) -> Option<&'a U>,
    {
        for src in &self.sources {
            if let Some(found) = block(src.as_ref()) {
                return Some(found);
            }
        }
        None
    }
}

pub trait SymbolProvider {
    fn search<'a, F, U>(&'a self, block: &F) -> Option<&U>
    where
        F: Fn(&'a SymbolTable) -> Option<&'a U>;

    fn type_metadata(&self, symbol: &Symbol) -> Option<&TypeMetadata> {
        self.search(&|sym| sym.get_type_metadata(symbol))
    }

    fn type_metadata_named(&self, name: &str) -> Option<&TypeMetadata> {
        self.search(&|symbols| {
            let type_symbol = symbols.lib.child(name);
            symbols.get_type_metadata(&type_symbol)
        })
    }

    fn top_level_function_named(&self, name: &str) -> Option<&FunctionMetadata> {
        self.search(&|symbols| {
            let func_symbol = symbols.lib.child(name);
            symbols.get_func_metadata(&func_symbol)
        })
    }

    fn function_metadata(&self, symbol: &Symbol) -> Option<&FunctionMetadata> {
        self.search(&|symbols| symbols.get_func_metadata(symbol))
    }

    fn trait_metadata(&self, name: &str) -> Option<&TraitMetadata> {
        self.search(&|symbols| {
            let trait_symbol = symbols.lib.child(name);
            symbols.get_trait_metadata(&trait_symbol)
        })
    }

    fn trait_metadata_symbol(&self, name: &Symbol) -> Option<&TraitMetadata> {
        self.search(&|symbols| symbols.get_trait_metadata(name))
    }

    fn symbol_span(&self, symbol: &Symbol) -> Option<&Span> {
        self.search(&|symbols| symbols.get_span(symbol))
    }
}
