use super::ir::*;
use super::irwriter::IRWriter;
use crate::library::*;

struct Builtin {
    write: Option<fn(&mut IRWriter, &Symbol) -> ()>,
    special_call: Option<fn(&Symbol, &GenericSpecialization, Vec<IRExpr>) -> IRExpr>,
}

impl Builtin {
    fn named(symbol: &Symbol) -> Option<Builtin> {
        match symbol.id.as_str() {
            "stdlib$Memory$Meta$size" => Some(Builtin::size()),
            "stdlib$ptr_offset" => Some(Builtin::ptr_offset()),
            "stdlib$_read_line" => Some(Builtin::read_line()),
            "stdlib$print" => Some(Builtin::print()),
            _ => None,
        }
    }

    fn size() -> Self {
        Builtin {
            write: None,
            special_call: Some(size_call),
        }
    }

    fn ptr_offset() -> Self {
        Builtin {
            write: Some(write_ptr_offset),
            special_call: None,
        }
    }

    fn read_line() -> Self {
        Builtin {
            write: Some(write_read_line),
            special_call: None,
        }
    }

    fn print() -> Self {
        Builtin {
            write: None,
            special_call: Some(print_call),
        }
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

pub fn can_write_special_call(symbol: &Symbol) -> bool {
    if let Some(builtin) = Builtin::named(&symbol) {
        if builtin.special_call.is_some() {
            return true;
        }
    }
    false
}

pub fn write_special_call(
    symbol: &Symbol,
    spec: &GenericSpecialization,
    args: Vec<IRExpr>,
) -> IRExpr {
    if let Some(builtin) = Builtin::named(&symbol) {
        if let Some(call) = &builtin.special_call {
            return call(symbol, spec, args);
        }
    }
    panic!()
}

pub fn write_special_function(
    writer: &mut IRWriter,
    metadata: &FunctionMetadata,
) {
    if is_direct_c_binding(&metadata.symbol) {
        return;
    }

    if let Some(builtin) = Builtin::named(&metadata.symbol) {
        if let Some(write) = builtin.write {
            writer.start_block();
            write(writer, &metadata.symbol);
            writer.end_decl_func(metadata);
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

    let casted = IRExpr::cast(
        IRExpr::variable(&pointer),
        NodeType::pointer_to(NodeType::Byte),
    );
    let addition = IRExpr::binary(
        casted,
        IRBinaryOperator::Plus,
        IRExpr::variable(&distance),
        NodeType::pointer_to(NodeType::Byte),
    );
    writer.return_value(addition);
}

fn write_read_line(writer: &mut IRWriter, _func_symbol: &Symbol) {
    let line = IRVariable::new("line", NodeType::pointer_to(NodeType::Byte));
    let size = IRVariable::new("size", NodeType::Int);

    writer.declare_var(&line);
    writer.assign_var(
        &line,
        IRExpr::literal("NULL", NodeType::pointer_to(NodeType::Byte)),
    );

    writer.declare_var(&size);

    let args = vec![
        IRExpr::address_of(&line),
        IRExpr::address_of(&size),
        IRExpr::literal("stdin", NodeType::Void),
    ];

    let call = IRExpr::call_nongenric("getline", args, NodeType::Void);
    writer.expr(call);

    writer.return_var(&line);
}

// Call impls

fn size_call(symbol: &Symbol, spec: &GenericSpecialization, _args: Vec<IRExpr>) -> IRExpr {
    let mem_type = Symbol::new_str(symbol, "T");
    let expr_type = spec.type_for(&mem_type).unwrap().clone();
    let arg = IRExpr {
        kind: IRExprKind::ExplicitType,
        expr_type: expr_type,
    };
    IRExpr::call_nongenric("sizeof", vec![arg], NodeType::Double)
}

fn print_call(_symbol: &Symbol, _spec: &GenericSpecialization, mut args: Vec<IRExpr>) -> IRExpr {
    let node_type = args[0].expr_type.clone();
    if let NodeType::Instance(type_symbol, spec) = &node_type {
        let full_symbol = Symbol::new_str(&type_symbol, "write");
        let arg = IRExpr {
            kind: IRExprKind::Unary(IRUnaryOperator::Reference, Box::new(args.remove(0))),
            expr_type: node_type.clone(),
        };
        IRExpr::call_generic(full_symbol, spec.clone(), vec![arg], NodeType::Void)
    } else {
        let format_specificer = match node_type {
            NodeType::Int | NodeType::Bool => "%i",
            NodeType::Double => "%f",
            node_type if node_type.is_pointer_to(NodeType::Byte) => "%s",
            _ => unreachable!(),
        };

        let format_line = format!("\"{}\\n\"", format_specificer);
        let format_expr = IRExpr {
            kind: IRExprKind::Literal(format_line),
            expr_type: NodeType::pointer_to(NodeType::Byte),
        };
        IRExpr::call_nongenric(
            "printf",
            vec![format_expr, args.remove(0)],
            NodeType::Void,
        )
    }
}
