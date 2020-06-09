use super::type_checker::NodeType;
use crate::lexing::*;
use crate::parsing::*;
use std::collections::HashMap;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Symbol {
    pub id: String,
    pub user_def_name: String,
}

impl Symbol {
    pub fn new(parent: Option<&Symbol>, name: &Token) -> Self {
        Symbol::new_str(parent, name.lexeme())
    }

    pub fn new_str(parent: Option<&Symbol>, name: &str) -> Self {
        let id = parent.map(|p| p.id.clone() + "$").unwrap_or("".to_string()) + name;
        Symbol {
            id,
            user_def_name: name.to_string(),
        }
    }

    pub fn mangled(&self) -> String {
        self.id.replace("$", "__")
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Symbol({})", self.id)
    }
}

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

    pub fn symbol_named(&self, name: &str) -> Option<&Symbol> {
        self.type_map
            .keys()
            .find(|symbol| symbol.user_def_name == name)
    }

    pub fn symbol_and_type_named(&self, name: &str) -> Option<(&Symbol, &NodeType)> {
        let symbol = self
            .type_map
            .keys()
            .find(|symbol| symbol.user_def_name == name);
        symbol.map(|s| (s, self.get_type(s).unwrap()))
    }
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "SymbolTable({:#?})", self.type_map)
    }
}

pub struct SymbolTableBuilder {
    table: SymbolTable,
    context: Vec<Symbol>,
}

impl SymbolTableBuilder {
    pub fn build(program: &ParsedProgram) -> SymbolTable {
        let mut builder = SymbolTableBuilder {
            table: SymbolTable::new(),
            context: Vec::new(),
        };
        builder.build_list(&program.statements);
        builder.table
    }

    fn build_list(&mut self, stmts: &[Stmt]) {
        stmts.iter().for_each(|s| {
            s.accept(self);
        });
    }
}

impl StmtVisitor for SymbolTableBuilder {
    type StmtResult = NodeType;

    fn visit_type_decl(
        &mut self,
        _stmt: &Stmt,
        name: &Token,
        fields: &[Stmt],
        methods: &[Stmt],
    ) -> Self::StmtResult {
        let new_symbol = Symbol::new(self.context.last(), name);
        let new_type = NodeType::Metatype(name.lexeme().to_string());
        self.table.insert(new_symbol.clone(), new_type.clone());

        self.context.push(new_symbol);
        self.build_list(&fields);
        self.build_list(&methods);
        self.context.pop();

        new_type
    }

    fn visit_function_decl(
        &mut self,
        _stmt: &Stmt,
        name: &Token,
        params: &[Stmt],
        return_type: &Option<Token>,
        _body: &[Stmt],
    ) -> Self::StmtResult {
        let new_symbol = Symbol::new(self.context.last(), name);

        let mut throwaway = SymbolTable::new();
        std::mem::swap(&mut self.table, &mut throwaway);
        let param_types: Vec<NodeType> = params.iter().map(|p| p.accept(self)).collect();
        std::mem::swap(&mut self.table, &mut throwaway);

        let return_type = NodeType::from_opt(return_type);
        let new_type = NodeType::Function(param_types, Box::new(return_type));

        self.table.insert(new_symbol, new_type.clone());

        new_type
    }

    fn visit_variable_decl(
        &mut self,
        _stmt: &Stmt,
        name: &Token,
        kind: &Option<Token>,
        _value: &Option<Expr>,
    ) -> Self::StmtResult {
        if let Some(new_type) = kind.as_ref().map(|k| NodeType::from(k)) {
            let new_symbol = Symbol::new(self.context.last(), name);

            self.table.insert(new_symbol, new_type.clone());
            new_type
        } else {
            NodeType::Void
        }
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

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, _expr: &Expr) -> Self::StmtResult {
        NodeType::Void
    }
}
