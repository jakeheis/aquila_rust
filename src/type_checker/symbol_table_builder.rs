use super::scope::{ContextTracker, ScopeType, SymbolResolution};
use crate::diagnostic::*;
use crate::library::*;
use crate::parsing::*;
use log::trace;
use std::rc::Rc;

pub struct SymbolTableBuilder<'a> {
    context: ContextTracker,
    symbols: SymbolTable,
    dependencies: &'a SymbolStore,
    reporter: Rc<dyn Reporter>,
}

impl<'a> SymbolTableBuilder<'a> {
    pub fn build_symbols(lib: &Lib, dependencies: &'a SymbolStore, reporter: Rc<dyn Reporter>) -> SymbolTable {
        let mut builder = SymbolTableBuilder {
            context: ContextTracker::new(lib.root_sym()),
            symbols: SymbolTable::new(&lib.name),
            dependencies,
            reporter,
        };

        builder.build_type_headers(&lib.type_decls);

        for decl in &lib.trait_decls {
            builder.build_trait_header(decl);
        }

        for decl in &lib.trait_decls {
            builder.build_trait_body(decl);
        }

        builder.build_type_internals(&lib.type_decls);
        builder.build_functions(&lib.function_decls, false);

        for decl in &lib.conformance_decls {
            builder.build_conformance(decl);
        }

        builder.symbols
    }

    fn build_type_headers(&mut self, type_decls: &[TypeDecl]) {
        for type_decl in type_decls {
            self.build_type_header(type_decl);
        }
    }

    fn build_type_header(&mut self, decl: &TypeDecl) {
        let new_symbol = self.current_symbol().child_token(&decl.name.token);

        let mut metadata = TypeMetadata::new(new_symbol.clone(), decl.is_public);

        metadata.generics = decl.generics.iter().map(|g| g.lexeme().to_owned()).collect();

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

        let mut type_metadata = self
            .symbols
            .get_type_metadata(&type_symbol)
            .expect(&format!("metadata for symbol {}", type_symbol))
            .clone();

        self.context
            .push_scope(type_symbol.clone(), ScopeType::InsideType);

        let mut any_private_fields = false;
        for field in &decl.fields {
            type_metadata.fields.push(VarMetadata {
                name: field.name.lexeme().to_owned(),
                var_type: self.resolve_type(&field.explicit_type),
                public: field.is_public,
            });

            if !field.is_public {
                any_private_fields = true;
            }
        }

        type_metadata.methods = self.build_functions(&decl.methods, false);

        self.context.push_meta_scope();
        type_metadata.meta_methods = self.build_functions(&decl.meta_methods, false);

        let init_symbol = self.current_symbol().init_symbol();
        type_metadata
            .meta_methods
            .push(init_symbol.name().to_owned());
        self.symbols.insert_func_metadata(
            init_symbol.clone(),
            FunctionMetadata {
                symbol: init_symbol,
                kind: FunctionKind::MetaMethod(type_symbol.clone()),
                generics: Vec::new(),
                parameters: type_metadata.fields.clone(),
                return_type: type_metadata.unspecialized_type(),
                is_public: !any_private_fields,
                generic_restrictions: Vec::new(),
                include_caller: false,
            },
        );

        self.context.pop_scope(); // Meta pop

        let deinit_symbol = self.current_symbol().deinit_symbol();
        type_metadata.methods.push(deinit_symbol.name().to_owned());
        self.symbols.insert_func_metadata(
            deinit_symbol.clone(),
            FunctionMetadata {
                symbol: deinit_symbol,
                kind: FunctionKind::Method(type_symbol.clone()),
                generics: Vec::new(),
                parameters: Vec::new(),
                return_type: NodeType::Void,
                is_public: true,
                generic_restrictions: Vec::new(),
                include_caller: false,
            },
        );

        self.context.pop_scope(); // Type pop

        self.symbols
            .insert_type_metadata(type_symbol, type_metadata);

        trace!(target: "symbol_table", "Finished building type {}", decl.name.token.lexeme());
    }

    fn build_functions(&mut self, decls: &[FunctionDecl], force_public: bool) -> Vec<String> {
        decls
            .iter()
            .map(|decl| self.build_function(decl, force_public))
            .collect()
    }

    fn build_function(&mut self, decl: &FunctionDecl, force_public: bool) -> String {
        let function_symbol = self.current_symbol().child_token(&decl.name.token);
        decl.name.set_symbol(function_symbol.clone());

        trace!(target: "symbol_table", "Building function {} (symbol = {})", decl.name.token.lexeme(), function_symbol);

        let mut function_metadata = FunctionMetadata {
            symbol: function_symbol.clone(),
            kind: FunctionKind::TopLevel,
            generics: decl.generics.iter().map(|g| g.lexeme().to_owned()).collect(),
            parameters: Vec::new(),
            return_type: NodeType::Ambiguous,
            include_caller: decl.include_caller,
            generic_restrictions: Vec::new(),
            is_public: force_public || decl.is_public,
        };
        self.symbols
            .insert_func_metadata(function_symbol.clone(), function_metadata.clone());

        self.context
            .push_scope(function_symbol.clone(), ScopeType::InsideFunction);

        for param in &decl.parameters {
            let node_type = self.resolve_type(&param.explicit_type);
            function_metadata.parameters.push(VarMetadata {
                name: param.name.lexeme().to_owned(),
                var_type: node_type,
                public: false,
            });
        }

        function_metadata.return_type = decl
            .return_type
            .as_ref()
            .map(|r| self.resolve_type(r))
            .unwrap_or(NodeType::Void);

        for restriction in &decl.generic_restrctions {
            let generic_name = restriction.generic.token.lexeme();
            let trait_name = restriction.trait_name.token.lexeme();

            let generic_symbol = if function_metadata.generics.iter().any(|g| g == generic_name) {
                function_metadata.symbol.child(generic_name)
            } else if let Some(g) = self.symbol_resolver().resolve_generic(generic_name) {
                g
            } else {
                self.reporter.report(Diagnostic::error(
                    &restriction.generic,
                    "Unrecognized generic type",
                ));
                continue
            };

            let this_lib_sym = self.symbols.lib.child(trait_name);
            let trait_metadata = if let Some(t) = self.symbols.get_trait_metadata(&this_lib_sym) {
                t
            } else if let Some(t) = self.dependencies.trait_metadata(trait_name) {
                t
            } else {
                self.reporter.report(Diagnostic::error(
                    &restriction.trait_name,
                    "Unrecognized trait",
                ));
                continue
            };
            
            function_metadata.generic_restrictions.push((generic_symbol, trait_metadata.symbol.clone()));
        }

        self.context.pop_scope();

        function_metadata.kind = match self.context.current_scope().scope_type {
            ScopeType::InsideMetatype => {
                FunctionKind::MetaMethod(self.current_symbol().owner_symbol().unwrap())
            }
            ScopeType::InsideType | ScopeType::InsideTrait => {
                FunctionKind::Method(self.current_symbol().clone())
            }
            ScopeType::TopLevel => FunctionKind::TopLevel,
            ScopeType::InsideFunction => panic!(),
        };

        self.symbols
            .insert_func_metadata(function_symbol.clone(), function_metadata);

        trace!(target: "symbol_table", "Finished building function {}", decl.name.token.lexeme());

        function_symbol.name().to_owned()
    }

    fn build_trait_header(&mut self, decl: &TraitDecl) {
        trace!("Building trait header {}", decl.name.lexeme());

        let trait_symbol = self.current_symbol().child_token(&decl.name);

        let metadata = TraitMetadata {
            symbol: trait_symbol.clone(),
            function_requirements: Vec::new(),
        };
        self.symbols.insert_trait_metadata(trait_symbol, metadata);
    }

    fn build_trait_body(&mut self, decl: &TraitDecl) {
        trace!("Building trait body {}", decl.name.lexeme());

        let trait_symbol = self.current_symbol().child_token(&decl.name);
        self.context
            .push_scope(trait_symbol.clone(), ScopeType::InsideTrait);
        let requirements = self.build_functions(&decl.requirements, true);
        self.context.pop_scope();

        let metadata = TraitMetadata {
            symbol: trait_symbol.clone(),
            function_requirements: requirements,
        };
        self.symbols.insert_trait_metadata(trait_symbol, metadata);

        trace!("Finished trait body {}", decl.name.lexeme());
    }

    fn build_conformance(&mut self, decl: &ConformanceDecl) {
        let type_symbol = self.current_symbol().child_token(&decl.target.token);
        let mut type_metadata = self
            .symbols
            .get_type_metadata(&type_symbol)
            .unwrap()
            .clone();

        self.context
            .push_scope(type_symbol.clone(), ScopeType::InsideType);
        let impls = self.build_functions(&decl.implementations, true);
        self.context.pop_scope();

        type_metadata.methods.extend(impls);

        let lib_sym = self.symbols.lib.child(decl.trait_name.lexeme());
        let trait_metadata = if let Some(t) = self.symbols.get_trait_metadata(&lib_sym) {
            t
        } else if let Some(t) = self.dependencies.trait_metadata(decl.trait_name.lexeme()) {
            t
        } else {
            self.reporter.report(Diagnostic::error(&decl.trait_name, "Trait not found"));
            return;
        };

        type_metadata.trait_impls.push(trait_metadata.symbol.clone());

        self.symbols
            .insert_type_metadata(type_symbol, type_metadata);
    }

    fn current_symbol(&self) -> &Symbol {
        self.context.current_symbol()
    }

    fn resolve_type(
        &self,
        explicit_type: &ExplicitType,
    ) -> NodeType {
        match self.symbol_resolver().resolve_explicit_type_or_err(explicit_type) {
            Ok(resolved_type) => resolved_type,
            Err(diag) => {
                self.reporter.report(diag);
                NodeType::Ambiguous
            }
        }
    }

    fn symbol_resolver(&self) -> SymbolResolution {
        self.context.symbol_resolver(&self.symbols, &self.dependencies)
    }
}
