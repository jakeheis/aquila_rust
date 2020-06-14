use super::c_writer::CWriter;
use crate::analysis::*;

pub fn should_write_builtin(symbol: &Symbol) -> bool {
    let id_ref: &str = &symbol.id;
    match id_ref {
        "strlen" | "memcpy" | "malloc" => false,
        _ => true,
    }
}

pub fn write(symbol: &Symbol, writer: &mut CWriter) {
    let id_ref: &str = &symbol.id;
    match id_ref {
        "ptr_offset" => write_ptr_offset(writer),
        name => panic!("Haven't implemented builtin {}", name),
    }
}

fn write_ptr_offset(writer: &mut CWriter) {
    let line = String::from("ptr_offset__pointer + ptr_offset__distance");
    writer.write_return(Some(line));
}
