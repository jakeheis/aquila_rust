use std::fs;
use std::path::PathBuf;

mod common;
use common::*;

#[test]
fn gauntlet() -> Result<(), &'static str> {
    let i = fs::read_dir("/Users/jakeheiser/Desktop/Projects/Rust/aquila/tests/aquila").unwrap();
    for file in i {
        test_file(file.unwrap().path())?;
    }
    Ok(())
}

fn test_file(file: PathBuf) -> Result<(), &'static str> {
    let (reporter, mut diagnostics) = TestReporter::new();

    let source = aquila::source::file(file.to_str().unwrap());
    match aquila::run_with_reporter(source, reporter) {
        Ok(_) => {
            assert_eq!(diagnostics.unwrap().len(), 0);
            Ok(())
        },
        Err(message) => {
            println!("Diagnostics:");
            for diag in diagnostics.unwrap() {
                println!("{}", diag);
            }
            Err(message)
        }
    }
}