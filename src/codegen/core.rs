use super::c_writer::CWriter;
use crate::analysis::*;

pub fn write(symbol: &Symbol, writer: &mut CWriter) {
    let id_ref: &str = &symbol.id; 
    match id_ref {
        "allocate" => write_allocate(writer),
        _ => panic!()
    }
}

fn write_allocate(writer: &mut CWriter) {
    let line = String::from("malloc(allocate__count)");
    writer.write_return(Some(line));
}
