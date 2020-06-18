use super::node_type::*;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use std::collections::HashMap;
use std::rc::Rc;

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

    // pub fn

    // pub fn is_generic(&self) -> bool {
    //     let last = self.last_component();
    //     let first_char: char = last.as_bytes()[0] as char;
    //     if let '0'..='9' = first_char {
    //         true
    //     } else {
    //         false
    //     }
    // }

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
    type_map: HashMap<Symbol, NodeType>,
    function_metadata: HashMap<Symbol, FunctionMetadata>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            type_map: HashMap::new(),
            function_metadata: HashMap::new(),
        }
    }

    pub fn insert(&mut self, symbol: Symbol, node_type: NodeType) {
        self.type_map.insert(symbol, node_type);
    }

    pub fn insert_func_metadata(&mut self, symbol: Symbol, metadata: FunctionMetadata) {
        self.function_metadata.insert(symbol, metadata);
    }

    pub fn get_type(&self, symbol: &Symbol) -> Option<&NodeType> {
        self.type_map.get(symbol)
    }
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "SymbolTable:")?;
        for (symbol, node_type) in self.type_map.iter() {
            writeln!(f, "  {} -> {}", symbol, node_type)?;
        }
        writeln!(f, "Function metadata:")?;
        for (symbol, meta) in self.function_metadata.iter() {
            writeln!(f, "  {} -> {}", symbol, meta)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct FunctionMetadata {
    symbol: Symbol,
    generics: Vec<Symbol>,
    parameters: Vec<NodeType>,
    return_type: NodeType,
}

impl std::fmt::Display for FunctionMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let generics: Vec<String> = self.generics.iter().map(|g| g.last_component().to_string()).collect();
        let generic_porition = if generics.is_empty() {
            String::new()
        } else {
            format!("|{}|", generics.join(","))
        };
        let parameters: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        let parameters = parameters.join(",");
        write!(f, "Function(def {}{}({}): {})", self.symbol.mangled(), generic_porition, parameters, self.return_type)
    }
}

pub struct SymbolTableBuilder {
    lib: Rc<Lib>,
    context: Vec<Symbol>,
}

impl SymbolTableBuilder {
    pub fn build_symbols(lib: IncompleteLib) -> Lib {
        let lib = lib.add_symbols(SymbolTable::new());
        let lib = Rc::new(lib);

        let mut builder = SymbolTableBuilder {
            lib: Rc::clone(&lib),
            context: Vec::new(),
        };

        builder.build_type_headers(&lib.type_decls);
        builder.build_type_internals(&lib.type_decls);
        builder.build_functions(&lib.function_decls);

        std::mem::drop(builder);

        Rc::try_unwrap(lib).ok().unwrap()
    }

    fn build_type_headers(&mut self, type_decls: &[Stmt]) {
        for type_decl in type_decls {
            self.build_type_header(type_decl);
        }
    }

    fn insert(&mut self, symbol: Symbol, node_type: NodeType) {
        self.lib.symbols.borrow_mut().insert(symbol, node_type);
    }

    fn insert_func_metadata(&mut self, symbol: Symbol, metadata: FunctionMetadata) {
        self.lib.symbols.borrow_mut().insert_func_metadata(symbol, metadata)
    }

    fn contains_symbol(&mut self, symbol: &Symbol) -> bool {
        self.lib.resolve_symbol(symbol).is_some()
    }

    fn build_type_header(&mut self, decl: &Stmt) {
        guard!(StmtKind::TypeDecl[name, _fields, _method, _meta_methods] = &decl.kind);

        let new_symbol = Symbol::new(self.context.last(), &name.token);
        let new_type = NodeType::Metatype(new_symbol.clone());

        self.insert(new_symbol.clone(), new_type.clone());

        name.set_symbol(new_symbol);
        name.set_type(new_type);
    }

    fn build_type_internals(&mut self, type_decls: &[Stmt]) {
        for type_decl in type_decls {
            self.build_type_internal(type_decl);
        }
    }

    fn build_type_internal(&mut self, decl: &Stmt) {
        guard!(StmtKind::TypeDecl[name, fields, methods, meta_methods] = &decl.kind);

        self.context.push(name.get_symbol().unwrap().clone());

        let mut field_types: Vec<NodeType> = Vec::new();
        for field in fields {
            let (token, field_type) = self.var_decl_type(field);
            field_types.push(field_type.clone());
            self.insert(Symbol::new(self.context.last(), token), field_type);
        }

        self.build_functions(&methods);

        self.context.push(Symbol::meta_symbol(self.context.last()));
        self.build_functions(&meta_methods);

        let init_symbol = Symbol::init_symbol(self.context.last());
        if !self.contains_symbol(&init_symbol) {
            let instance_type = NodeType::Type(name.get_symbol().unwrap().clone());
            let init_type = NodeType::Function(field_types, Vec::new(), Box::new(instance_type));
            self.insert(init_symbol, init_type.clone());
        }

        self.context.pop(); // Meta pop

        self.context.pop(); // Type pop
    }

    fn build_functions(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.build_function(stmt);
        }
    }

    fn build_function(&mut self, stmt: &Stmt) {
        let (name, generics, params, return_type) = match &stmt.kind {
            StmtKind::FunctionDecl(name, generics, params, return_type, ..) => (name, generics, params, return_type),
            StmtKind::Builtin(internal) => {
                guard!(StmtKind::FunctionDecl[name, generics, params, return_type, _body, _meta] = &internal.kind);
                (name, generics, params, return_type)
            }
            _ => unreachable!(),
        };

        let function_symbol = Symbol::new(self.context.last(), &name.token);

        self.context.push(function_symbol.clone());

        let mut generic_symbols = Vec::new();
        for (index, generic) in generics.iter().enumerate() {
            let generic_symbol = Symbol::new(self.context.last(), &generic.token);
            self.insert(generic_symbol.clone(), NodeType::Metatype(generic_symbol.clone()));
            generic_symbols.push(generic_symbol);
        }

        let param_types: Vec<_> = params.iter().map(|p| self.var_decl_type(p).1).collect();

        let return_type = return_type
            .as_ref()
            .map(|r| self.resolve_explicit_type(r))
            .unwrap_or(NodeType::Void);

        self.context.pop();

        let new_type = NodeType::Function(param_types.clone(), generic_symbols.clone(), Box::new(return_type.clone()));

        let function_metadata = FunctionMetadata {
            symbol: function_symbol.clone(),
            generics: generic_symbols,
            parameters: param_types.clone(),
            return_type: return_type,
        };
        self.insert_func_metadata(function_symbol.clone(), function_metadata);

        self.insert(function_symbol, new_type.clone());
        name.set_type(new_type.clone());
    }

    fn var_decl_type<'a>(&self, var_decl: &'a Stmt) -> (&'a Token, NodeType) {
        guard!(StmtKind::VariableDecl[name, explicit_type, _value] = &var_decl.kind);
        let explicit_type = self.resolve_explicit_type(explicit_type.as_ref().unwrap());
        (&name.token, explicit_type)
    }

    fn resolve_explicit_type(&self, explicit_type: &ExplicitType) -> NodeType {
        if let Some(node_type) = explicit_type.resolve(&self.lib, &self.context) {
            node_type
        } else {
            // Isn't a real type; make a fake type and type checker will catch the error
            NodeType::Type(Symbol::new_str(None, "_fake_type"))
        }
    }

    /*
    fn symbol_for_type_token(&self, token: &Token) -> Symbol {
        for parent in self.context.iter().rev() {
            let non_top_level_symbol = Symbol::new(Some(parent), token);
            if let Some(NodeType::Metatype(_)) = self.table.get_type(&non_top_level_symbol) {
                return non_top_level_symbol;
            }
        }

        let top_level_symbol = Symbol::new(None, token);

        if let Some(NodeType::Metatype(_)) = self.table.get_type(&top_level_symbol) {
            top_level_symbol
        } else if let Some(NodeType::Metatype(_)) = self.lib.resolve_symbol(&top_level_symbol) {
            top_level_symbol
        } else {
            // Isn't a real type; make a fake type and type checker will catch the error
            Symbol::new(None, token)
        }
    }

    fn resolve_explicit_type_expr(&self, type_expr: &Expr) -> NodeType {
        guard!(ExprKind::ExplicitType[category] = &type_expr.kind);

        let node_type = match category {
            ExplicitTypeCategory::Simple(token) => {
                if let Some(primitive) = NodeType::primitive(&token.token) {
                    primitive
                } else {
                    NodeType::Type(self.symbol_for_type_token(&token.token))
                }
            }
            ExplicitTypeCategory::Pointer(to) => {
                let inner = self.resolve_explicit_type_expr(to);
                NodeType::Pointer(Box::new(inner))
            }
            ExplicitTypeCategory::Array(of, count_token) => {
                let inner = self.resolve_explicit_type_expr(of);
                match count_token.lexeme().parse::<usize>() {
                    Ok(count) => {
                        let size = ArraySize::Known(count);
                        NodeType::Array(Box::new(inner), size)
                    }
                    Err(..) => NodeType::Array(Box::new(inner), ArraySize::Unknown),
                }
            }
        };

        type_expr.set_type(node_type).ok().unwrap()
    }*/
}
