use super::type_checker::NodeType;
use crate::guard;
use crate::lexing::*;
use crate::library::*;
use crate::parsing::*;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

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
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            type_map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, symbol: Symbol, node_type: NodeType) {
        self.type_map.insert(symbol, node_type);
    }

    pub fn get_type(&self, symbol: &Symbol) -> Option<&NodeType> {
        self.type_map.get(symbol)
    }
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "SymbolTable({:#?})", self.type_map)
    }
}

pub struct SymbolTableBuilder {
    table: SymbolTable,
    lib: Rc<IncompleteLib>,
    context: Vec<Symbol>,
    visit_methods: bool,
}

impl SymbolTableBuilder {
    pub fn build_symbols(lib: IncompleteLib) -> Lib {
        let lib = Rc::new(lib);

        let mut builder = SymbolTableBuilder {
            table: SymbolTable::new(),
            lib: Rc::clone(&lib),
            context: Vec::new(),
            visit_methods: false,
        };

        // Add all top level types before visiting any function and trying to figure out return type
        builder.build_list(&lib.type_decls);

        builder.visit_methods = true;
        builder.build_list(&lib.type_decls);
        builder.build_list(&lib.function_decls);

        let table = std::mem::replace(&mut builder.table, SymbolTable::new());
        std::mem::drop(builder);

        let lib = Rc::try_unwrap(lib).ok().unwrap();
        lib.add_symbols(table)
    }

    fn build_list(&mut self, stmts: &[Stmt]) -> Vec<NodeType> {
        stmts.iter().map(|s| s.accept(self)).collect()
    }

    fn resolve_symbol(&self, symbol: &Symbol) -> Option<&NodeType> {
        if let Some(found) = self.table.get_type(symbol) {
            Some(found)
        } else if let Some(found) = self.lib.resolve_symbol(symbol) {
            Some(found)
        } else {
            None
        }
    }

    fn resolve_type_expr(&self, type_expr: &Expr) -> NodeType {
        guard!(ExprKind::ExplicitType[type_token, modifier] = &type_expr.kind);

        let main = if let Some(primitive) = NodeType::primitive(type_token) {
            primitive
        } else if let Some(NodeType::Metatype(symbol)) =
            self.resolve_symbol(&Symbol::new(None, type_token))
        {
            NodeType::Type(symbol.clone())
        } else if let Some(NodeType::Metatype(symbol)) =
            self.resolve_symbol(&Symbol::new(self.context.last(), type_token))
        {
            NodeType::Type(symbol.clone())
        } else {
            // Isn't a real type; make a fake type and type checker will catch the error
            NodeType::Type(Symbol::new(None, type_token))
        };

        if modifier.is_some() {
            NodeType::Pointer(Box::new(main))
        } else {
            main
        }
    }
}

impl StmtVisitor for SymbolTableBuilder {
    type StmtResult = NodeType;

    fn visit_type_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        fields: &[Stmt],
        methods: &[Stmt],
        meta_methods: &[Stmt],
    ) -> Self::StmtResult {
        let new_symbol = Symbol::new(self.context.last(), &name.token);
        let new_type = NodeType::Metatype(new_symbol.clone());

        if self.visit_methods {
            self.context.push(new_symbol.clone());
            let field_types = self.build_list(&fields);

            self.build_list(&methods);

            self.context.push(Symbol::meta_symbol(self.context.last()));
            self.build_list(&meta_methods);

            let init_symbol = Symbol::init_symbol(self.context.last());
            if self.table.get_type(&init_symbol).is_none() {
                let init_type =
                    NodeType::Function(field_types, Box::new(NodeType::Type(new_symbol)));
                self.table
                    .insert(Symbol::init_symbol(self.context.last()), init_type.clone());
            }

            self.context.pop();

            self.context.pop();
        } else {
            self.table.insert(new_symbol.clone(), new_type.clone());
            self.context.push(new_symbol.clone());
            for field in fields {
                guard!(StmtKind::VariableDecl[name, _e1, _e2] = &field.kind);
                let field_type = field.accept(self);
                self.table
                    .insert(Symbol::new(self.context.last(), &name.token), field_type);
            }
            self.context.pop();
        }

        new_type
    }

    fn visit_function_decl(
        &mut self,
        _stmt: &Stmt,
        name: &TypedToken,
        params: &[Stmt],
        return_type: &Option<Expr>,
        _body: &[Stmt],
        _is_meta: bool,
    ) -> Self::StmtResult {
        let new_symbol = Symbol::new(self.context.last(), &name.token);

        let param_types: Vec<NodeType> = params.iter().map(|p| p.accept(self)).collect();

        let return_type = return_type
            .as_ref()
            .map(|r| self.resolve_type_expr(r))
            .unwrap_or(NodeType::Void);
        let new_type = NodeType::Function(param_types, Box::new(return_type));

        self.table.insert(new_symbol, new_type.clone());
        name.set_type(new_type.clone());

        new_type
    }

    fn visit_variable_decl(
        &mut self,
        _stmt: &Stmt,
        _name: &TypedToken,
        explicit_type: &Option<Expr>,
        _value: &Option<Expr>,
    ) -> Self::StmtResult {
        self.resolve_type_expr(explicit_type.as_ref().unwrap())
    }

    fn visit_if_stmt(
        &mut self,
        _stmt: &Stmt,
        _condition: &Expr,
        _body: &[Stmt],
        _else_body: &[Stmt],
    ) -> Self::StmtResult {
        NodeType::Void
    }

    fn visit_return_stmt(&mut self, _stmt: &Stmt, _expr: &Option<Expr>) -> Self::StmtResult {
        NodeType::Void
    }

    fn visit_print_stmt(&mut self, _stmt: &Stmt, _expr: &Option<Expr>, _print_type: &RefCell<Option<NodeType>>) -> Self::StmtResult {
        NodeType::Void
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, _expr: &Expr) -> Self::StmtResult {
        NodeType::Void
    }

    fn visit_builtin_stmt(&mut self, _stmt: &Stmt, inner: &Box<Stmt>) -> Self::StmtResult {
        inner.accept(self)
    }
}
