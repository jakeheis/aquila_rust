use crate::library::*;
use super::irwriter::IRWriter;
use super::ir::*;

pub fn is_direct_c_binding(symbol: &Symbol) -> bool {
    if symbol.lib_component() != "stdlib" {
        return false;
    }
    match symbol.last_component() {
        "strlen" | "memcpy" | "malloc" | "sizeof" | "realloc" | "exit" => true,
        _ => false,
    }
}

pub fn write(symbol: &Symbol, writer: &mut IRWriter) {
    match symbol.last_component() {
        "ptr_offset" => write_ptr_offset(writer, symbol),
        "_read_line" => write_read_line(writer),
        name => panic!("Haven't implemented builtin {}", name),
    }
}

pub fn add_builtin_symbols(_lib: &Lib) {
    // let mut symbols = lib.symbols.borrow_mut();
    // symbols.insert(Symbol::new_str(None, "iterate"), NodeType::FlexibleFunction(iterate_check));
}

// pub fn iterate_check(args: &[NodeType]) -> bool {
//     if args.len() == 1 {
//         if let NodeType::Array(..) = args[0] {
//             return true;
//         }
//     }
//     false
// }

fn write_ptr_offset(writer: &mut IRWriter, func_symbol: &Symbol) {
    let pointer_param = Symbol::new_str(func_symbol, "pointer");
    let distance_param = Symbol::new_str(func_symbol, "distance");

    let pointer = IRVariable::new(&pointer_param.mangled(), NodeType::pointer_to(NodeType::Any));
    let distance = IRVariable::new(&distance_param.mangled(), NodeType::Int);

    let pointer = IRExpr::variable(&pointer);
    let casted = IRExpr {
        kind: IRExprKind::Cast(Box::new(pointer)),
        expr_type: NodeType::pointer_to(NodeType::Byte)
    };
    let distance = IRExpr::variable(&distance);
    let addition = IRExpr {
        kind: IRExprKind::Binary(Box::new(casted), String::from("+"), Box::new(distance)),
        expr_type: NodeType::pointer_to(NodeType::Byte)
    };
    writer.return_value(Some(addition));
}

fn write_read_line(writer: &mut IRWriter) {
    let line = IRVariable::new("line", NodeType::pointer_to(NodeType::Byte));
    let size = IRVariable::new("size", NodeType::Int);
    
    writer.declare_var(&line);
    writer.assign_var(&line, IRExpr {
        kind: IRExprKind::Literal(String::from("NULL")),
        expr_type: NodeType::pointer_to(NodeType::Byte)
    });

    writer.declare_var(&size);

    let args = vec![
        IRExpr::address_of(&line),
        IRExpr::address_of(&size),
        IRExpr {
            kind: IRExprKind::Literal(String::from("stdin")),
            expr_type: NodeType::Void,
        }
    ];

    let call = IRExpr {
        kind: IRExprKind::Call(String::from("getline"), args),
        expr_type: NodeType::Void
    };
    writer.expr(call);

    writer.return_var(&line);
}
