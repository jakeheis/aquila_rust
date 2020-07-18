use super::{TypeResolution, TypeResolutionError, ContextTracker, ScopeType};
use crate::diagnostic::*;
use crate::lexing::Token;
use crate::library::*;
use crate::parsing::*;
use log::trace;
use std::rc::Rc;

pub struct SymbolTableBuilder {
    lib: Rc<Lib>,
    symbols: SymbolTable,
    context: ContextTracker,
    reporter: Rc<dyn Reporter>,
}

impl SymbolTableBuilder {
    pub fn build_symbols(lib: Lib, reporter: Rc<dyn Reporter>) -> Lib {
        let lib = Rc::new(lib);

        let mut builder = SymbolTableBuilder {
            lib: Rc::clone(&lib),
            symbols: SymbolTable::new(),
            context: ContextTracker::new(Rc::clone(&lib)),
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

    fn build_type_header(&mut self, decl: &TypeDecl) {
        let new_symbol = self.current_symbol().child_token(&decl.name.token);

        let mut metadata = TypeMetadata::new(new_symbol.clone(), decl.is_public);

        self.context.push_scope(new_symbol.clone(), ScopeType::InsideType);
        metadata.generics = self.insert_placeholder_generics(&new_symbol, &decl.generics);
        self.context.pop_scope();

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

        self.context.push_scope(type_symbol.clone(), ScopeType::InsideType);

        let (generics, _) = self.insert_generics(&type_symbol, &decl.generics, &[]);

        type_metadata.generics = generics;

        let mut any_private_fields = false;
        for field in &decl.fields {            
            type_metadata.fields.push(VarMetadata {
                name: field.name.lexeme().to_owned(),
                var_type: self.resolve_type(&field.explicit_type, None),
                public: field.is_public
            });

            if !field.is_public {
                any_private_fields = true;
            }
        }
        
        type_metadata.methods = self.build_functions(&decl.methods, false);

        self.context.push_scope_meta();
        type_metadata.meta_methods = self.build_functions(&decl.meta_methods, false);

        let init_symbol = self.current_symbol().init_symbol();
        type_metadata.meta_methods.push(init_symbol.name().to_owned());
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
        decls.iter().map(|decl| self.build_function(decl, force_public)).collect()
    }

    fn build_function(&mut self, decl: &FunctionDecl, force_public: bool) -> String {
        let function_symbol = self.current_symbol().child_token(&decl.name.token);

        trace!(target: "symbol_table", "Building function {} (symbol = {})", decl.name.token.lexeme(), function_symbol);

        self.context.push_scope(function_symbol.clone(), ScopeType::InsideFunction);

        let (generics, generic_restrictions) = self.insert_generics(&function_symbol, &decl.generics, &decl.generic_restrctions);

        let mut params: Vec<VarMetadata> = Vec::new();
        for param in &decl.parameters {
            let node_type = self.resolve_type(&param.explicit_type, Some(&function_symbol));
            params.push(VarMetadata {
                name: param.name.lexeme().to_owned(),
                var_type: node_type,
                public: false
            });
        }

        let return_type = decl
            .return_type
            .as_ref()
            .map(|r| self.resolve_type(r, Some(&function_symbol)))
            .unwrap_or(NodeType::Void);

        self.context.pop_scope();

        let function_kind = match self.context.current_scope().scope_type {
            ScopeType::InsideMetatype => FunctionKind::MetaMethod(self.current_symbol().owner_symbol().unwrap()),
            ScopeType::InsideType | ScopeType::InsideTrait => FunctionKind::Method(self.current_symbol().clone()),
            ScopeType::TopLevel => FunctionKind::TopLevel,
            ScopeType::InsideFunction => panic!()
        };

        let function_metadata = FunctionMetadata {
            symbol: function_symbol.clone(),
            kind: function_kind,
            generics,
            parameters: params,
            return_type: return_type,
            include_caller: decl.include_caller,
            generic_restrictions,
            is_public: force_public || decl.is_public,
        };
        self.symbols.insert_func_metadata(function_symbol.clone(), function_metadata);

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
        self.context.push_scope(trait_symbol.clone(), ScopeType::InsideTrait);
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
        let mut type_metadata = self.symbols.get_type_metadata(&type_symbol).unwrap().clone();

        self.context.push_scope(type_symbol.clone(), ScopeType::InsideType);
        let impls = self.build_functions(&decl.implementations, true);
        self.context.pop_scope();

        type_metadata.methods.extend(impls);

        self.symbols.insert_type_metadata(type_symbol, type_metadata);
    }

    fn current_symbol(&self) -> &Symbol {
        self.context.current_symbol()
    }

    fn insert_placeholder_generics(&mut self, owner: &Symbol, generics: &[Token]) -> Vec<String> {
        let mut generic_names = Vec::new();
        for generic in generics {
            let generic_symbol = self.current_symbol().child_token(&generic);
            let generic_type = TypeMetadata::generic(owner, generic.lexeme());
            self.symbols.insert_type_metadata(generic_symbol.clone(), generic_type);
            generic_names.push(generic.lexeme().to_owned());
        }
        generic_names
    }

    fn insert_generics(&mut self, owner: &Symbol, generics: &[Token], restrictions: &[GenericRestriction]) -> (Vec<String>, Vec<(Symbol, Symbol)>) {
        let mut generic_names = Vec::new();

        for generic in generics {
            let generic_symbol = self.current_symbol().child_token(&generic);
            let mut generic_type = TypeMetadata::generic(owner, generic.lexeme());

            for restriction in restrictions {
                if restriction.generic.token.lexeme() == generic.lexeme() {
                    restriction.generic.set_symbol(generic_symbol.clone());
                    let trait_metadata = self.resolve_trait(&restriction.trait_name.token.lexeme());

                    if let Some(trait_metadata) = trait_metadata {
                        let trait_metadata = trait_metadata.clone();

                        restriction.trait_name.set_symbol(trait_metadata.symbol.clone());

                        // TODO: doesn't support meta requirements
                        for requirement in &trait_metadata.function_requirements {
                            let requirement = trait_metadata.symbol.child(&requirement);
                            let mut req_metadata = if let Some(func) = self.symbols.get_func_metadata(&requirement) {
                                func
                            } else {
                                self.lib.function_metadata(&requirement).unwrap()
                            }.clone();

                            let symbol = generic_symbol.child(requirement.name());
                            req_metadata.symbol = symbol.clone();
                            req_metadata.kind = FunctionKind::Method(generic_symbol.clone());
                            self.symbols.insert_func_metadata(symbol.clone(), req_metadata);
                            generic_type.methods.push(symbol.name().to_owned());
                        }
    
                        generic_type.add_trait_impl(trait_metadata.symbol.clone());
                    } else {
                        self.reporter.report(Diagnostic::error(&restriction.trait_name, "Unrecognized trait"));
                    }
                }
            }

            self.symbols
                .insert_type_metadata(generic_symbol.clone(), generic_type);

            trace!(target: "symbol_table", "Inserting generic {} (symbol = {})", generic.lexeme(), generic_symbol);
            generic_names.push(generic.lexeme().to_owned());
        }

        let mut restriction_symbols: Vec<(Symbol, Symbol)> = Vec::new();
        for restriction in restrictions {
            if let Some(generic_symbol) = restriction.generic.get_symbol() {
                if let Some(trait_symbol) = restriction.trait_name.get_symbol() {
                    restriction_symbols.push((generic_symbol, trait_symbol));
                }
            } else {
                self.reporter.report(Diagnostic::error(&restriction.generic, "Unrecognized generic type"));
            }
        }

        (generic_names, restriction_symbols)
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

    fn resolve_trait(&self, trait_name: &str) -> Option<&TraitMetadata> {
        let same_lib_symbol = Symbol::lib_root(&self.lib.name).child(trait_name);
        
        if let Some(metadata) = self.symbols.get_trait_metadata(&same_lib_symbol) {
            Some(metadata)
        } else if let Some(trait_meta) = self.lib.trait_metadata(trait_name) {
            Some(trait_meta)
        } else {
            None
        }
    }
}
