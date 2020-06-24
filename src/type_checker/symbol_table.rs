use super::TypeResolution;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::*;
use crate::source::*;
use log::trace;
use std::collections::HashSet;

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
        self.symbols
            .insert_type_metadata(new_symbol.clone(), metadata);

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

        let generics = self.insert_generics(&type_symbol, &decl.generics);
        self.symbols
            .get_type_metadata_mut(&type_symbol)
            .unwrap()
            .generics = generics;

        let mut type_metadata = self
            .symbols
            .get_type_metadata(&type_symbol)
            .unwrap()
            .clone();

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
                .map(|symbol| NodeType::Instance(symbol.clone(), GenericSpecialization::empty()))
                .collect();
            let instance_type = NodeType::Instance(
                type_symbol.clone(),
                GenericSpecialization::new(&type_metadata.generics, generic_types),
            );

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
                    specializations: HashSet::new(),
                },
            )
        }

        self.context.pop(); // Meta pop

        self.context.pop(); // Type pop

        self.symbols
            .type_metadata
            .insert(type_symbol, type_metadata);

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
            Some(p) if p.is_meta() => FunctionKind::MetaMethod(p.parent().unwrap().clone()),
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
            specializations: HashSet::new(),
        };
        self.insert_func_metadata(function_symbol.clone(), function_metadata);

        trace!(target: "symbol_table", "Finished building function {} -- {}", decl.name.token.lexeme(), new_type);

        function_symbol
    }

    fn insert_generics(&mut self, owner: &Symbol, generics: &[ResolvedToken]) -> Vec<Symbol> {
        let mut generic_symbols = Vec::new();
        for generic in generics {
            let generic_symbol = Symbol::new(self.context.last(), &generic.token);
            let generic_type = TypeMetadata::generic(owner, generic.token.lexeme());
            self.symbols
                .type_metadata
                .insert(generic_symbol.clone(), generic_type);

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
            TypeResolution::resolve(explicit_type, &self.symbols, self.deps, &self.context)
        {
            trace!(target: "symbol_table", "Resolved explicit type {} to {}", explicit_type.span().lexeme(), node_type);
            node_type
        } else {
            NodeType::Ambiguous
        }
    }
}
