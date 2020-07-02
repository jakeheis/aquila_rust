use super::ir::*;
use super::irwriter::IRWriter;
use crate::library::*;

struct Builtin {
    name: &'static str,
    implicit_calls: Vec<Symbol>,
    write: Option<fn(&mut IRWriter, &Symbol) -> ()>,
    special_call: Option<fn(&Symbol, &[IRExpr], &GenericSpecialization) -> IRExpr>,
}

impl Builtin {
    fn all() -> Vec<Builtin> {
        vec![Builtin::fatal_error(), Builtin::size(), Builtin::ptr_offset(), Builtin::read_line()]
    }

    fn named(symbol: &Symbol) -> Option<Builtin> {
        let slice: &str = &symbol.id;
        match slice {
            "stdlib$fatal_error" => Some(Builtin::fatal_error()),
            "stdlib$Memory$Meta$size" => Some(Builtin::size()),
            "stdlib$ptr_offset" => Some(Builtin::ptr_offset()),
            "stdlib$_read_line" => Some(Builtin::read_line()),
            _ => None
        }
    }

    fn fatal_error() -> Self {
        Builtin {
            name: "fatal_error",
            implicit_calls: vec![Symbol::new_str(&Symbol::stdlib_root(), "fatal_error_location")],
            write: None,
            special_call: Some(fatal_error_call),
        }
    }

    fn size() -> Self {
        Builtin {
            name: "size",
            implicit_calls: Vec::new(),
            write: None,
            special_call: Some(size_call)
        }
    }

    fn ptr_offset() -> Self {
        Builtin {
            name: "ptr_offset",
            implicit_calls: Vec::new(),
            write: Some(write_ptr_offset),
            special_call: None
        }
    }

    fn read_line() -> Self {
        Builtin {
            name: "_read_line",
            implicit_calls: Vec::new(),
            write: Some(write_read_line),
            special_call: None
        }
    }

    fn symbol(&self) -> Symbol {
        Symbol::new_str(&Symbol::stdlib_root(), self.name)
    }
}

pub fn is_direct_c_binding(symbol: &Symbol) -> bool {
    if symbol.lib_component() != "stdlib" {
        return false;
    }
    match symbol.last_component() {
        "strlen" | "memcpy" | "malloc" | "sizeof" | "realloc" | "exit" => true,
        _ => false,
    }
}

pub fn record_implicit_calls(lib: &mut Lib) {
    for builtin in Builtin::all() {
        for call in &builtin.implicit_calls {
            let builtin_symbol = builtin.symbol();
            lib.specialization_tracker.add_call(builtin_symbol, call.clone(), GenericSpecialization::empty());
        }
    }
}

pub fn write_special_call(symbol: &Symbol, args: &[IRExpr], spec: &GenericSpecialization) -> Option<IRExpr> {
    if let Some(builtin) = Builtin::named(&symbol) {
        if let Some(call) = &builtin.special_call {
            return Some(call(symbol, args, spec));
        }
    }
    None
}

pub fn write_special_function(writer: &mut IRWriter, metadata: &FunctionMetadata, spec: &GenericSpecialization) {
    if is_direct_c_binding(&metadata.symbol) {
        return;
    }

    if let Some(builtin) = Builtin::named(&metadata.symbol) {
        if let Some(write) = builtin.write {
            writer.start_block();
            write(writer, &metadata.symbol);
            writer.end_decl_func(metadata, spec);
        }
    } else {
        panic!("Builtin not handled: {}", metadata.symbol)
    }
}

// Write impls

fn write_ptr_offset(writer: &mut IRWriter, func_symbol: &Symbol) {
    let pointer_param = Symbol::new_str(func_symbol, "pointer");
    let distance_param = Symbol::new_str(func_symbol, "distance");

    let pointer = IRVariable::new(
        &pointer_param.mangled(),
        NodeType::pointer_to(NodeType::Any),
    );
    let distance = IRVariable::new(&distance_param.mangled(), NodeType::Int);

    let pointer = IRExpr::variable(&pointer);
    let casted = IRExpr {
        kind: IRExprKind::Cast(Box::new(pointer)),
        expr_type: NodeType::pointer_to(NodeType::Byte),
    };
    let distance = IRExpr::variable(&distance);
    let addition = IRExpr {
        kind: IRExprKind::Binary(Box::new(casted), String::from("+"), Box::new(distance)),
        expr_type: NodeType::pointer_to(NodeType::Byte),
    };
    writer.return_value(Some(addition));
}

fn write_read_line(writer: &mut IRWriter, _func_symbol: &Symbol) {
    let line = IRVariable::new("line", NodeType::pointer_to(NodeType::Byte));
    let size = IRVariable::new("size", NodeType::Int);

    writer.declare_var(&line);
    writer.assign_var(
        &line,
        IRExpr {
            kind: IRExprKind::Literal(String::from("NULL")),
            expr_type: NodeType::pointer_to(NodeType::Byte),
        },
    );

    writer.declare_var(&size);

    let args = vec![
        IRExpr::address_of(&line),
        IRExpr::address_of(&size),
        IRExpr {
            kind: IRExprKind::Literal(String::from("stdin")),
            expr_type: NodeType::Void,
        },
    ];

    let call = IRExpr::call("getline", args, NodeType::Void);
    writer.expr(call);

    writer.return_var(&line);
}

// Call impls

fn fatal_error_call(_symbol: &Symbol, args: &[IRExpr], _spec: &GenericSpecialization) -> IRExpr {
    let message = args[0].clone();
    let location = IRExpr::string_literal("\"<unknown>\"");
    let callee = Symbol::new_str(&Symbol::stdlib_root(), "fatal_error_location");
    IRExpr::call(&callee.mangled(), vec![message, location], NodeType::Void)
}

fn size_call(symbol: &Symbol, _args: &[IRExpr], spec: &GenericSpecialization) -> IRExpr {
    let mem_type = Symbol::new_str(symbol, "T");
    let mem_type = spec.type_for(&mem_type).unwrap();

    let arg = IRExpr {
        kind: IRExprKind::ExplicitType,
        expr_type: mem_type.clone()
    };
    IRExpr::call("sizeof", vec![arg], NodeType::Double)
}
