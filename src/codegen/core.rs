use super::c_writer::CWriter;
use crate::analysis::*;

pub fn should_write_builtin(symbol: &Symbol) -> bool {
    let id_ref: &str = &symbol.id;
    match id_ref {
        "strlen" => false,
        _ => true,
    }
}

pub fn write(symbol: &Symbol, writer: &mut CWriter) {
    let id_ref: &str = &symbol.id;
    match id_ref {
        "allocate" => write_allocate(writer),
        name => panic!("Haven't implemented builtin {}", name),
    }
}

fn write_allocate(writer: &mut CWriter) {
    let line = String::from("malloc(allocate__count)");
    writer.write_return(Some(line));
}
