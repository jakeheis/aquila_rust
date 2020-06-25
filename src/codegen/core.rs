use super::c_writer::CWriter;
use crate::library::*;

pub fn is_direct_c_binding(symbol: &Symbol) -> bool {
    if symbol.lib_component() != "stdlib" {
        return false;
    }
    match symbol.last_component() {
        "strlen" | "memcpy" | "malloc" | "sizeof" | "realloc" | "exit" => true,
        _ => false,
    }
}

pub fn write(symbol: &Symbol, writer: &mut CWriter) {
    match symbol.last_component() {
        "ptr_offset" => write_ptr_offset(writer),
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

fn write_ptr_offset(writer: &mut CWriter) {
    let line = String::from("(char*)stdlib__ptr_offset__pointer + stdlib__ptr_offset__distance");
    writer.write_return(Some(line));
}

fn write_read_line(writer: &mut CWriter) {
    writer.decl_var(
        &NodeType::pointer_to(NodeType::Byte),
        "line",
        Some(String::from("NULL")),
    );
    writer.decl_var(&NodeType::Int, "size", None);
    writer.writeln("getline(&line, &size, stdin);");
    writer.write_return(Some(String::from("line")));
}
