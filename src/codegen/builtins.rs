use super::ir::*;
use super::irwriter::IRWriter;
use crate::library::*;

struct Builtin {
    write: Option<fn(&mut IRWriter, &Symbol) -> ()>,
    special_call:
        Option<fn(&mut IRWriter, &Symbol, &GenericSpecialization, Vec<IRExpr>) -> IRExpr>,
}

impl Builtin {
    fn named(symbol: &Symbol) -> Option<Builtin> {
        match symbol.unique_id() {
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
    let bindings = [
        "strlen", "memcpy", "malloc", "sizeof", "realloc", "exit", "free",
    ];
    for binding in &bindings {
        if symbol == &Symbol::stdlib(binding) {
            return true;
        }
    }
    false
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
    writer: &mut IRWriter,
    func_symbol: &Symbol,
    spec: &GenericSpecialization,
    args: Vec<IRExpr>,
) -> IRExpr {
    if let Some(builtin) = Builtin::named(&func_symbol) {
        if let Some(call) = &builtin.special_call {
            return call(writer, func_symbol, spec, args);
        }
    }
    panic!()
}

pub fn write_special_function(writer: &mut IRWriter, metadata: &FunctionMetadata) {
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
    let pointer = IRVariable::new_sym(
        &func_symbol.child("pointer"),
        NodeType::pointer_to(NodeType::Any),
    );
    let distance = IRVariable::new_sym(&func_symbol.child("distance"), NodeType::Int);

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

    let call = IRExpr::call_extern("getline", args, NodeType::Void);
    writer.expr(call);

    writer.return_var(&line);
}

pub fn write_type_init(writer: &mut IRWriter, type_metadata: &TypeMetadata) {
    let init_symbol = type_metadata.symbol.meta_symbol().init_symbol();
    let init_metadata = writer.all_symbols.function_metadata(&init_symbol).unwrap().clone();

    let new_item = IRVariable::new("new_item", init_metadata.return_type.clone());

    writer.start_block();
    writer.declare_var(&new_item);

    for field in &type_metadata.fields {
        let field_expr = IRExpr::field(
            &new_item,
            &type_metadata.symbol_for_field(&field).mangled(),
            field.var_type.clone(),
        );
        let param = IRVariable::new_sym(&init_symbol.child(&field.name), field.var_type.clone());
        writer.assign(field_expr, IRExpr::variable(&param));
    }
    writer.return_value(IRExpr::variable(&new_item));
    writer.end_decl_func(&init_metadata);
}

pub fn write_type_deinit(
    writer: &mut IRWriter,
    type_metadata: &TypeMetadata,
) {
    let deinit_symbol = Symbol::deinit_symbol(&type_metadata.symbol);
    let deinit_metadata = writer
        .all_symbols
        .function_metadata(&deinit_symbol)
        .unwrap()
        .clone();

    writer.start_block();

    let self_var = IRVariable::self_var(type_metadata);

    if type_metadata.conforms_to(&Symbol::stdlib("Freeable")) { 
        let free_sym = type_metadata.symbol.child("free");
        let free = IRExpr::call(
            free_sym.clone(),
            type_metadata.dummy_specialization(),
            vec![IRExpr::variable(&self_var)],
            NodeType::Void,
        );
        writer.expr(free);
    }

    for field in &type_metadata.fields {
        if let NodeType::Instance(type_sym, spec) = &field.var_type {
            let field_symbol = type_metadata.symbol_for_field(field);

            let field_expr =
                IRExpr::field_deref(&self_var, &field_symbol.mangled(), field.var_type.clone());
            let field_expr = IRExpr {
                kind: IRExprKind::Unary(IRUnaryOperator::Reference, Box::new(field_expr)),
                expr_type: NodeType::pointer_to(field.var_type.clone()),
            };

            let deinit_sym = Symbol::deinit_symbol(&type_sym);
            let deinit = IRExpr::call(
                deinit_sym.clone(),
                spec.clone(),
                vec![field_expr],
                NodeType::Void,
            );
            writer.expr(deinit);
        }
    }

    writer.end_decl_func(&deinit_metadata);
}

// Call impls

fn size_call(
    _writer: &mut IRWriter,
    func_symbol: &Symbol,
    spec: &GenericSpecialization,
    _args: Vec<IRExpr>,
) -> IRExpr {
    let mem_type = func_symbol.child("T");
    let expr_type = spec.type_for(&mem_type).unwrap().clone();
    let arg = IRExpr {
        kind: IRExprKind::ExplicitType,
        expr_type: expr_type,
    };
    IRExpr::call_extern("sizeof", vec![arg], NodeType::Double)
}

fn print_call(
    _writer: &mut IRWriter,
    _func_symbol: &Symbol,
    _spec: &GenericSpecialization,
    mut args: Vec<IRExpr>,
) -> IRExpr {
    let node_type = args[0].expr_type.clone();
    if let NodeType::Instance(..) = &node_type {
        let print_object = Symbol::stdlib("print_object");
        let spec =
            GenericSpecialization::new(&print_object, &["T".to_owned()], vec![node_type.clone()]);
        IRExpr::call(print_object, spec.clone(), args, NodeType::Void)
    } else {
        let format_specificer = match node_type {
            NodeType::Int | NodeType::Bool => "%i",
            NodeType::Double => "%f",
            node_type if node_type.is_pointer_to(NodeType::Byte) => "%s",
            _ => unreachable!(),
        };

        let format_line = format!("{}\\n", format_specificer);
        let format_expr = IRExpr::string_literal(&format_line);
        IRExpr::call_extern("printf", vec![format_expr, args.remove(0)], NodeType::Void)
    }
}
