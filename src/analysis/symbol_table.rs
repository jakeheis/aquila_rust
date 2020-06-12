use super::type_checker::NodeType;
use crate::lexing::*;
use crate::parsing::*;
use std::collections::HashMap;
use crate::guard;
use crate::source::*;

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

    pub fn mangled(&self) -> String {
        self.id.replace("$", "__")
    }

    pub fn owns(&self, other: &Symbol) -> bool {
        let name = other.id.rsplit("$").nth(0).unwrap();
        let expected = self.id.clone() + "$" + &name;
        other.id == expected
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
    context: Vec<Symbol>,
    visit_methods: bool,
}

impl SymbolTableBuilder {
    pub fn build(program: &ParsedProgram) -> SymbolTable {
        let mut builder = SymbolTableBuilder {
            table: program.stdlib.as_ref().map(|s| s.symbols.clone()).unwrap_or(SymbolTable::new()),
            context: Vec::new(),
            visit_methods: false,
        };

        // Add all top level types before visiting any function and trying to figure out return type
        builder.build_list(&program.type_decls);

        builder.visit_methods = true;
        builder.build_list(&program.type_decls);
        builder.build_list(&program.function_decls);

        builder.table
    }

    fn build_list(&mut self, stmts: &[Stmt]) -> Vec<NodeType> {
        stmts.iter().map(|s| {
            s.accept(self)
        }).collect()
    }

    fn resolve_type_expr(&self, type_expr: &Expr) -> NodeType {
        guard!(ExprKind::ExplicitType[name, modifier] = &type_expr.kind);

        let main = self.resolve_type(name);
        if modifier.is_some() {
            NodeType::Pointer(Box::new(main))
        } else {
            main
        }
    }

    fn resolve_type(&self, type_token: &Token) -> NodeType {
        if let Some(primitive) = NodeType::primitive(type_token) {
            primitive
        } else if let Some(NodeType::Metatype(symbol)) =
            self.table.get_type(&Symbol::new(None, type_token))
        {
            NodeType::Type(symbol.clone())
        } else if let Some(NodeType::Metatype(symbol)) = self
            .table
            .get_type(&Symbol::new(self.context.last(), type_token))
        {
            NodeType::Type(symbol.clone())
        } else {
            // Isn't a real type; make a fake type and type checker will catch the error
            NodeType::Type(Symbol::new(None, type_token))
        }
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
        let new_type = NodeType::Metatype(new_symbol.clone());

        if self.visit_methods {
            self.context.push(new_symbol.clone());
            let field_types = self.build_list(&fields);

            // Init method
            let init_type = NodeType::Function(
                field_types, 
                Box::new(NodeType::Type(new_symbol))
            );
            self.table.insert(Symbol::new_str(self.context.last(), "init"), init_type.clone());

            self.build_list(&methods);
            
            self.context.pop();
        } else {
            self.table.insert(new_symbol.clone(), new_type.clone());
        }

        new_type
    }

    fn visit_function_decl(
        &mut self,
        stmt: &Stmt,
        name: &Token,
        params: &[Stmt],
        return_type: &Option<Expr>,
        _body: &[Stmt],
    ) -> Self::StmtResult {
        let new_symbol = Symbol::new(self.context.last(), name);

        let param_types: Vec<NodeType> = params.iter().map(|p| p.accept(self)).collect();

        let return_type = return_type
            .as_ref()
            .map(|r| self.resolve_type_expr(r))
            .unwrap_or(NodeType::Void);
        let new_type = NodeType::Function(param_types, Box::new(return_type));

        self.table.insert(new_symbol, new_type.clone());
        stmt.stmt_type.replace(Some(new_type.clone()));

        new_type
    }

    fn visit_variable_decl(
        &mut self,
        _stmt: &Stmt,
        _name: &Token,
        kind: &Option<Expr>,
        _value: &Option<Expr>,
    ) -> Self::StmtResult {
        self.resolve_type_expr(kind.as_ref().unwrap())
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

    fn visit_print_stmt(&mut self, _stmt: &Stmt, _expr: &Option<Expr>) -> Self::StmtResult {
        NodeType::Void
    }

    fn visit_expression_stmt(&mut self, _stmt: &Stmt, _expr: &Expr) -> Self::StmtResult {
        NodeType::Void
    }

    fn visit_builtin_stmt(&mut self, _stmt: &Stmt, inner: &Box<Stmt>) -> Self::StmtResult {
        inner.accept(self)
    }

}
