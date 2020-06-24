use std::fs;
use std::path::PathBuf;
use std::process::Command;

mod common;
use common::*;

#[test]
fn gauntlet() -> Result<(), &'static str> {
    let i = fs::read_dir("/Users/jakeheiser/Desktop/Projects/Rust/aquila/tests/aquila").unwrap();
    for file in i {
        println!(
            "Testing file: {}",
            file.as_ref().ok().unwrap().path().to_str().unwrap()
        );
        test_file(file.unwrap().path())?;
    }
    Ok(())
}

fn test_file(file: PathBuf) -> Result<(), &'static str> {
    let (reporter, mut diagnostics) = TestReporter::new();

    let read = fs::read_to_string(file.clone()).unwrap();
    let lines: Vec<_> = read.lines().collect();
    if lines[0] == "// skip" {
        println!("Skipping");
        return Ok(());
    }

    let mut expected_output: Vec<String> = Vec::new();
    for line in lines {
        if line.starts_with("/// ") {
            let output = &line[4..];
            expected_output.push(String::from(output));
        }
    }

    let source = aquila::source::file(file.to_str().unwrap());
    match aquila::run_with_reporter(source, reporter) {
        Ok(_) => assert_eq!(diagnostics.unwrap().len(), 0),
        Err(message) => {
            println!("Diagnostics:");
            for diag in diagnostics.unwrap() {
                println!("{}", diag);
            }
            return Err(message);
        }
    }

    if !expected_output.is_empty() {
        let output = Command::new("/Users/jakeheiser/Desktop/Projects/Rust/aquila/build/program")
            .output()
            .unwrap();

        let got_lines = String::from_utf8(output.stdout).unwrap();

        if !output.status.success() {
            println!("Output:\n{}", got_lines);
            return Err("Run failed");
        }

        for (index, (got, expected)) in got_lines.lines().zip(expected_output).enumerate() {
            if got != expected {
                println!("Line {}: expected '{}', got '{}'", index + 1, expected, got);
                return Err("Invalid output");
            }
        }

        Ok(())
    } else {
        Ok(())
    }
}
