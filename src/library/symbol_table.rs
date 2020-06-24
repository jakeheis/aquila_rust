use super::metadata::*;
use crate::lexing::Token;
use crate::source::Span;
use std::collections::HashMap;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Symbol {
    pub id: String,
}

impl Symbol {
    pub fn new(parent: Option<&Symbol>, name: &Token) -> Self {
        Symbol::new_str(parent, name.lexeme())
    }

    pub fn new_str(parent: Option<&Symbol>, name: &str) -> Self {
        let id = parent.map(|p| p.id.clone() + "$").unwrap_or("".to_string()) + name;
        Symbol { id }
    }

    pub fn meta_symbol(parent: Option<&Symbol>) -> Self {
        Symbol::new_str(parent, "Meta")
    }

    pub fn init_symbol(parent: Option<&Symbol>) -> Self {
        Symbol::new_str(parent, "init")
    }

    pub fn self_symbol() -> Self {
        Symbol::new_str(None, "self")
    }

    pub fn main_symbol() -> Self {
        Symbol::new_str(None, "main")
    }

    pub fn mangled(&self) -> String {
        self.id.replace("$", "__")
    }

    pub fn owns(&self, other: &Symbol) -> bool {
        let name = other.id.rsplit("$").nth(0).unwrap();
        let expected = self.id.clone() + "$" + &name;
        other.id == expected
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
        self.id == "self"
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
    pub function_metadata: HashMap<Symbol, FunctionMetadata>,
    pub span_map: HashMap<Symbol, Span>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            type_metadata: HashMap::new(),
            function_metadata: HashMap::new(),
            span_map: HashMap::new(),
        }
    }

    pub fn insert_type_metadata(&mut self, symbol: Symbol, metadata: TypeMetadata) {
        self.type_metadata.insert(symbol, metadata);
    }

    pub fn get_type_metadata(&self, symbol: &Symbol) -> Option<&TypeMetadata> {
        self.type_metadata.get(symbol)
    }

    pub fn get_type_metadata_mut(&mut self, symbol: &Symbol) -> Option<&mut TypeMetadata> {
        self.type_metadata.get_mut(symbol)
    }

    pub fn insert_func_metadata(&mut self, symbol: Symbol, metadata: FunctionMetadata) {
        self.function_metadata.insert(symbol, metadata);
    }

    pub fn get_func_metadata(&self, symbol: &Symbol) -> Option<&FunctionMetadata> {
        self.function_metadata.get(symbol)
    }

    pub fn get_func_metadata_mut(&mut self, symbol: &Symbol) -> Option<&mut FunctionMetadata> {
        self.function_metadata.get_mut(symbol)
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
        Ok(())
    }
}
