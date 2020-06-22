use super::metadata::*;
use super::node_type::*;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use crate::source::*;
use log::trace;
use std::collections::HashMap;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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

    pub fn generic_symbol(parent: Option<&Symbol>, index: usize) -> Self {
        // Symbol::new_str(parent, &format!("{}__Specialization", name.lexeme()))
        Symbol::new_str(parent, &format!("{}", index))
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

    pub fn is_meta_owned(&self) -> bool {
        self.parent().map(|p| p.is_meta()).unwrap_or(false)
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
    pub call_map: HashMap<Symbol, Vec<(Symbol, GenericSpecialization)>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            type_metadata: HashMap::new(),
            function_metadata: HashMap::new(),
            span_map: HashMap::new(),
            call_map: HashMap::new(),
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
        for (symbol, meta) in self.function_metadata.iter() {
            writeln!(f, "  {} -> {}", symbol, meta)?;
        }
        Ok(())
    }
}

pub struct SymbolTableBuilder<'a> {
    symbols: SymbolTable,
    deps: &'a [Lib],
    context: Vec<Symbol>,
}

impl<'a> SymbolTableBuilder<'a> {
    pub fn build_symbols(
        type_decls: &[TypeDecl],
        function_decls: &[FunctionDecl],
        builtins: &[FunctionDecl],
        deps: &[Lib],
    ) -> SymbolTable {
        let mut builder = SymbolTableBuilder {
            symbols: SymbolTable::new(),
            deps: deps,
            context: Vec::new(),
        };

        builder.build_type_headers(&type_decls);
        builder.build_type_internals(&type_decls);
        builder.build_functions(&function_decls);
        builder.build_functions(&builtins);

        builder.symbols
    }

    fn build_type_headers(&mut self, type_decls: &[TypeDecl]) {
        for type_decl in type_decls {
            self.build_type_header(type_decl);
        }
    }

    fn insert_func_metadata(&mut self, symbol: Symbol, metadata: FunctionMetadata) {
        self.symbols.insert_func_metadata(symbol, metadata)
    }

    // fn insert_type_metadata(&mut self, symbol: Symbol, metadata: TypeMetadata) {
    //     self.symbols.insert_type_metadata(symbol, metadata)
    // }

    fn build_type_header(&mut self, decl: &TypeDecl) {
        let new_symbol = Symbol::new(self.context.last(), &decl.name.token);

        let metadata = TypeMetadata::new(new_symbol.clone());
        self.symbols.insert_type_metadata(new_symbol.clone(), metadata);

        decl.name.set_symbol(new_symbol);
    }

    fn build_type_internals(&mut self, type_decls: &[TypeDecl]) {
        for type_decl in type_decls {
            self.build_type_internal(type_decl);
        }
    }

    fn build_type_internal(&mut self, decl: &TypeDecl) {
        let type_symbol = decl.name.get_symbol().unwrap();

        trace!(target: "symbol_table", "Building type {} (symbol = {})", decl.name.token.lexeme(), type_symbol);

        self.context.push(type_symbol.clone());

        let mut type_metadata = self.symbols.get_type_metadata(&type_symbol).unwrap().clone();

        type_metadata.generics = self.insert_generics(&type_symbol, &decl.generics);

        for field in &decl.fields {
            let (token, field_type) = self.var_decl_type(field);
            type_metadata.field_types.push(field_type.clone());

            let field_symbol = Symbol::new(self.context.last(), token);
            type_metadata.field_symbols.push(field_symbol.clone());

            trace!(target: "symbol_table", "Inserting field {} (symbol = {})", token.lexeme(), field_symbol);
        }

        type_metadata.methods = self.build_functions(&decl.methods);

        self.context.push(Symbol::meta_symbol(self.context.last()));
        type_metadata.meta_methods = self.build_functions(&decl.meta_methods);

        let init_symbol = Symbol::init_symbol(self.context.last());
        if self.symbols.get_func_metadata(&init_symbol).is_none() {
            let generic_types: Vec<_> = type_metadata
                    .generics
                    .iter()
                    .map(|symbol| NodeType::Instance(symbol.clone(), GenericSpecialization::empty(&symbol)))
                    .collect();
            let instance_type = NodeType::Instance(type_symbol.clone(), GenericSpecialization::new(&type_symbol, generic_types));

            type_metadata.meta_methods.push(init_symbol.clone());

            self.insert_func_metadata(
                init_symbol.clone(),
                FunctionMetadata {
                    symbol: init_symbol,
                    kind: FunctionKind::MetaMethod(type_symbol.clone()),
                    generics: Vec::new(),
                    parameter_symbols: type_metadata.field_symbols.clone(),
                    parameter_types: type_metadata.field_types.clone(),
                    return_type: instance_type,
                    specializations: Vec::new(),
                },
            )
        }

        self.context.pop(); // Meta pop

        self.context.pop(); // Type pop

        self.symbols.type_metadata.insert(type_symbol, type_metadata);

        trace!(target: "symbol_table", "Finished building type {}", decl.name.token.lexeme());
    }

    fn build_functions(&mut self, decls: &[FunctionDecl]) -> Vec<Symbol> {
        decls.iter().map(|decl| self.build_function(decl)).collect()
    }

    fn build_function(&mut self, decl: &FunctionDecl) -> Symbol {
        let function_symbol = Symbol::new(self.context.last(), &decl.name.token);

        trace!(target: "symbol_table", "Building function {} (symbol = {})", decl.name.token.lexeme(), function_symbol);

        self.context.push(function_symbol.clone());

        let generic_symbols = self.insert_generics(&function_symbol, &decl.generics);

        let mut param_types: Vec<NodeType> = Vec::new();
        let mut param_symbols: Vec<Symbol> = Vec::new();
        for param in &decl.parameters {
            let (token, node_type) = self.var_decl_type(param);
            param_types.push(node_type);
            param_symbols.push(Symbol::new(Some(&function_symbol), token));
        }

        let return_type = decl
            .return_type
            .as_ref()
            .map(|r| self.resolve_explicit_type(r))
            .unwrap_or(NodeType::Void);

        self.context.pop();

        let new_type = NodeType::function(param_types.clone(), return_type.clone());
        let function_kind = match self.context.last() {
            Some(p) if p.is_meta() => FunctionKind::MetaMethod(p.clone()),
            Some(p) => {
                if self.symbols.get_type_metadata(&p).is_some() {
                    FunctionKind::Method(p.clone())
                } else {
                    FunctionKind::TopLevel
                }
            }
            _ => FunctionKind::TopLevel,
        };

        let function_metadata = FunctionMetadata {
            symbol: function_symbol.clone(),
            kind: function_kind,
            generics: generic_symbols,
            parameter_symbols: param_symbols,
            parameter_types: param_types,
            return_type: return_type,
            specializations: Vec::new(),
        };
        self.insert_func_metadata(function_symbol.clone(), function_metadata);

        trace!(target: "symbol_table", "Finished building function {} -- {}", decl.name.token.lexeme(), new_type);

        decl.name.set_type(new_type.clone());

        function_symbol
    }

    fn insert_generics(&mut self, owner: &Symbol, generics: &[TypedToken]) -> Vec<Symbol> {
        let mut generic_symbols = Vec::new();
        for (index, generic) in generics.iter().enumerate() {
            let generic_symbol = Symbol::new(self.context.last(), &generic.token);
            let generic_type = TypeMetadata::generic(owner, generic.token.lexeme(), index);
            self.symbols.type_metadata.insert(generic_symbol.clone(), generic_type);

            trace!(target: "symbol_table", "Inserting generic {} (symbol = {})", generic.token.lexeme(), generic_symbol);
            generic_symbols.push(generic_symbol);
        }
        generic_symbols
    }

    fn var_decl_type<'b>(&self, var_decl: &'b VariableDecl) -> (&'b Token, NodeType) {
        let explicit_type = self.resolve_explicit_type(var_decl.explicit_type.as_ref().unwrap());
        (&var_decl.name.token, explicit_type)
    }

    fn resolve_explicit_type(&self, explicit_type: &ExplicitType) -> NodeType {
        if let Some(node_type) =
            NodeType::deduce_from(explicit_type, &self.symbols, self.deps, &self.context)
        {
            trace!(target: "symbol_table", "Resolved explicit type {} to {}", explicit_type.span().lexeme(), node_type);
            node_type
        } else {
            NodeType::Ambiguous
        }
    }
}
