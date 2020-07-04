use super::metadata::*;
use super::Lib;
use crate::lexing::Token;
use crate::source::Span;
use std::collections::HashMap;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Symbol {
    pub id: String,
}

impl Symbol {
    pub fn lib_root(lib: &Lib) -> Self {
        Symbol {
            id: lib.name.clone(),
        }
    }

    pub fn new(parent: &Symbol, name: &Token) -> Self {
        Symbol::new_str(parent, name.lexeme())
    }

    pub fn new_str(parent: &Symbol, name: &str) -> Self {
        let id = parent.id.clone() + "$" + name;
        Symbol { id }
    }

    pub fn top_level(lib: &Lib, name: &Token) -> Self {
        Symbol::new(&Symbol::lib_root(lib), name)
    }

    pub fn meta_symbol(parent: &Symbol) -> Self {
        Symbol::new_str(parent, "Meta")
    }

    pub fn init_symbol(parent: &Symbol) -> Self {
        Symbol::new_str(parent, "init")
    }

    pub fn self_symbol(parent: &Symbol) -> Self {
        Symbol::new_str(parent, "self")
    }

    pub fn main_symbol(lib: &Lib) -> Self {
        Symbol::new_str(&Symbol::lib_root(lib), "main")
    }

    pub fn stdlib_root() -> Self {
        Symbol {
            id: String::from("stdlib"),
        }
    }

    pub fn stdlib(name: &str) -> Self {
        Symbol::new_str(&Symbol::stdlib_root(), name)
    }

    pub fn writable_symbol() -> Self {
        // Temporarily hard coded
        Symbol::stdlib("Writable")
    }

    pub fn mangled(&self) -> String {
        self.id.replace("$", "__")
    }

    pub fn owns(&self, other: &Symbol) -> bool {
        let name = other.id.rsplit("$").nth(0).unwrap();
        let expected = self.id.clone() + "$" + &name;
        other.id == expected
    }

    pub fn lib_component(&self) -> &str {
        self.id.split("$").next().unwrap()
    }

    pub fn parent(&self) -> Option<Symbol> {
        let mut comopnents: Vec<_> = self.id.split("$").collect();
        comopnents.pop();
        if comopnents.is_empty() {
            None
        } else {
            Some(Symbol {
                id: comopnents.join("$"),
            })
        }
    }

    pub fn last_component(&self) -> &str {
        self.id.split("$").last().unwrap()
    }

    pub fn is_meta(&self) -> bool {
        self.last_component() == "Meta"
    }

    pub fn is_self(&self) -> bool {
        self.last_component() == "self"
    }

    pub fn is_main(&self) -> bool {
        self.last_component() == "main"
    }

    pub fn is_init(&self) -> bool {
        self.last_component() == "init"
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Symbol({})", self.id)
    }
}

#[derive(Clone)]
pub struct SymbolTable {
    pub type_metadata: HashMap<Symbol, TypeMetadata>,
    function_metadata: HashMap<Symbol, FunctionMetadata>,
    trait_metadata: HashMap<Symbol, TraitMetadata>,
    span_map: HashMap<Symbol, Span>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
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
