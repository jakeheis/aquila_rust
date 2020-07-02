use super::ir::*;
use super::irwriter::IRWriter;
use crate::library::*;

pub fn record_implicit_calls(lib: &mut Lib) {
    let fatal_error_location = Symbol::new_str(&Symbol::stdlib_root(), "fatal_error_location");
    let main = Symbol::main_symbol(lib);
    lib.specialization_tracker.add_call(main, fatal_error_location, GenericSpecialization::empty());
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

pub fn write_special_call(symbol: &Symbol, args: &[IRExpr], spec: &GenericSpecialization) -> Option<IRExpr> {
    if symbol.lib_component() != "stdlib" {
        return None;
    }

    match symbol.last_component() {
        "size" => {
            let mem_type = Symbol::new_str(symbol, "T");
            let mem_type = spec.type_for(&mem_type).unwrap();

            let arg = IRExpr {
                kind: IRExprKind::ExplicitType,
                expr_type: mem_type.clone()
            };
            let call = IRExpr::call("sizeof", vec![arg], NodeType::Double);
            Some(call)
        }
        "fatal_error" => {
            let message = args[0].clone();
            let location = IRExpr::string_literal("\"<unknown>\"");
            let callee = Symbol::new_str(&Symbol::stdlib_root(), "fatal_error_location");
            let call = IRExpr::call(&callee.mangled(), vec![message, location], NodeType::Void);
            Some(call)
        }
        _ => None,
    }
}

pub fn write_special_function(writer: &mut IRWriter, metadata: &FunctionMetadata, spec: &GenericSpecialization) {
    // If direct c binding, don't write anything
    if is_direct_c_binding(&metadata.symbol) {
        return;
    }

    let write_func: fn(&mut IRWriter, &Symbol) -> () = match metadata.symbol.last_component() {
        "ptr_offset" => {
            write_ptr_offset
        },
        "_read_line" => {
            write_read_line
        },
        "size" | "fatal_error" => {
            // Don't write function; modify call sites
            return;
        }
        _ => panic!("Builtin not handled: {}", metadata.symbol),
    };

    writer.start_block();
    write_func(writer, &metadata.symbol);
    writer.end_decl_func(metadata, spec);
}

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
