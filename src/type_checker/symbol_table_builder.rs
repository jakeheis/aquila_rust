use super::{TypeResolution, TypeResolutionError};
use crate::diagnostic::*;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::*;
use log::trace;
use std::rc::Rc;

pub struct SymbolTableBuilder {
    lib: Rc<Lib>,
    symbols: SymbolTable,
    context: Vec<Symbol>,
    reporter: Rc<dyn Reporter>,
}

impl SymbolTableBuilder {
    pub fn build_symbols(lib: Lib, reporter: Rc<dyn Reporter>) -> Lib {
        let lib = Rc::new(lib);

        let mut builder = SymbolTableBuilder {
            lib: Rc::clone(&lib),
            symbols: SymbolTable::new(),
            context: vec![Symbol::lib_root(lib.as_ref())],
            reporter,
        };

        builder.build_type_headers(&lib.type_decls);
        builder.build_type_internals(&lib.type_decls);
        builder.build_functions(&lib.function_decls);

        for decl in &lib.trait_decls {
            builder.build_trait(decl);
        }

        for decl in &lib.conformance_decls {
            builder.build_conformance(decl);
        }

        let symbols = std::mem::replace(&mut builder.symbols, SymbolTable::new());
        std::mem::drop(builder);

        let mut lib = Rc::try_unwrap(lib).ok().unwrap();
        lib.symbols = symbols;

        lib
    }

    fn build_type_headers(&mut self, type_decls: &[TypeDecl]) {
        for type_decl in type_decls {
            self.build_type_header(type_decl);
        }
    }

    fn insert_func_metadata(&mut self, symbol: Symbol, metadata: FunctionMetadata) {
        self.symbols.insert_func_metadata(symbol, metadata)
    }

    fn build_type_header(&mut self, decl: &TypeDecl) {
        let new_symbol = Symbol::new(self.current_symbol(), &decl.name.token);

        let mut metadata = TypeMetadata::new(new_symbol.clone(), decl.is_public);

        self.context.push(new_symbol.clone());
        metadata.generics = self.insert_generics(&new_symbol, &decl.generics);
        self.context.pop();

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

        let mut type_metadata = self
            .symbols
            .get_type_metadata(&type_symbol)
            .unwrap()
            .clone();

        let mut any_private_fields = false;
        for field in &decl.fields {
            let (token, field_type) = self.var_decl_type(field, None);
            type_metadata.field_types.push(field_type.clone());

            let field_symbol = Symbol::new(self.current_symbol(), token);
            type_metadata.field_symbols.push(field_symbol.clone());

            type_metadata.field_visibilities.push(field.is_public);
            if !field.is_public {
                any_private_fields = true;
            }

            trace!(target: "symbol_table", "Inserting field {} (symbol = {})", token.lexeme(), field_symbol);
        }

        type_metadata.methods = self.build_functions(&decl.methods);

        self.context
            .push(Symbol::meta_symbol(self.current_symbol()));
        type_metadata.meta_methods = self.build_functions(&decl.meta_methods);

        let init_symbol = Symbol::init_symbol(self.current_symbol());
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
                    is_public: !any_private_fields,
                    include_caller: false,
                },
            )
        }

        self.context.pop(); // Meta pop

        self.context.pop(); // Type pop

        self.symbols.insert_type_metadata(type_symbol, type_metadata);

        trace!(target: "symbol_table", "Finished building type {}", decl.name.token.lexeme());
    }

    fn build_functions(&mut self, decls: &[FunctionDecl]) -> Vec<Symbol> {
        decls.iter().map(|decl| self.build_function(decl)).collect()
    }

    fn build_function(&mut self, decl: &FunctionDecl) -> Symbol {
        let function_symbol = Symbol::new(self.current_symbol(), &decl.name.token);

        trace!(target: "symbol_table", "Building function {} (symbol = {})", decl.name.token.lexeme(), function_symbol);

        self.context.push(function_symbol.clone());

        let generic_symbols = self.insert_generics(&function_symbol, &decl.generics);

        let mut param_types: Vec<NodeType> = Vec::new();
        let mut param_symbols: Vec<Symbol> = Vec::new();
        for param in &decl.parameters {
            let (token, node_type) = self.var_decl_type(param, Some(&function_symbol));
            param_types.push(node_type);
            param_symbols.push(Symbol::new(&function_symbol, token));
        }

        let return_type = decl
            .return_type
            .as_ref()
            .map(|r| self.resolve_type(r, Some(&function_symbol)))
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
            is_public: decl.is_public,
            include_caller: decl.include_caller
        };
        self.insert_func_metadata(function_symbol.clone(), function_metadata);

        trace!(target: "symbol_table", "Finished building function {} -- {}", decl.name.token.lexeme(), new_type);

        function_symbol
    }

    fn build_trait(&mut self, decl: &TraitDecl) {
        let trait_symbol = Symbol::new(self.current_symbol(), &decl.name);
        let requirements = self.build_functions(&decl.requirements);
        let metadata = TraitMetadata {
            symbol: trait_symbol.clone(),
            function_requirements: requirements,
        };
        self.symbols.insert_trait_metadata(trait_symbol, metadata);
    }

    fn build_conformance(&mut self, decl: &ConformanceDecl) {
        let type_symbol = Symbol::new(self.current_symbol(), &decl.target.token);
        self.context.push(type_symbol);
        self.build_functions(&decl.implementations);
        self.context.pop();
    }

    fn current_symbol(&self) -> &Symbol {
        self.context.last().unwrap()
    }

    fn insert_generics(&mut self, owner: &Symbol, generics: &[Token]) -> Vec<Symbol> {
        let mut generic_symbols = Vec::new();
        for generic in generics {
            let generic_symbol = Symbol::new(self.current_symbol(), &generic);
            let generic_type = TypeMetadata::generic(owner, generic.lexeme());
            self.symbols.insert_type_metadata(generic_symbol.clone(), generic_type);

            trace!(target: "symbol_table", "Inserting generic {} (symbol = {})", generic.lexeme(), generic_symbol);
            generic_symbols.push(generic_symbol);
        }
        generic_symbols
    }

    fn var_decl_type<'b>(
        &self,
        var_decl: &'b StructuralVariableDecl,
        enclosing_func: Option<&Symbol>,
    ) -> (&'b Token, NodeType) {
        let resolved_type = self.resolve_type(&var_decl.explicit_type, enclosing_func);
        (&var_decl.name, resolved_type)
    }

    fn resolve_type(
        &self,
        explicit_type: &ExplicitType,
        enclosing_func: Option<&Symbol>,
    ) -> NodeType {
        let resolver = TypeResolution::new(&self.lib, &self.symbols, &self.context, enclosing_func);
        match resolver.resolve(explicit_type) {
            Ok(resolved_type) => resolved_type,
            Err(error) => {
                let diag = match error {
                    TypeResolutionError::IncorrectlySpecialized(diag)
                    | TypeResolutionError::Inaccessible(diag) => diag,
                    TypeResolutionError::NotFound => {
                        Diagnostic::error(explicit_type, "Type not found")
                    }
                };
                self.reporter.report(diag);
                NodeType::Ambiguous
            }
        }
    }
}
