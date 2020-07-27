use aquila::diagnostic::*;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

mod common;
use common::*;

#[test]
fn array() -> Result<(), &'static str> {
    test_file("array")
}

#[test]
fn generics() -> Result<(), &'static str> {
    test_file("generics")
}

#[test]
fn math() -> Result<(), &'static str> {
    test_file("math")
}

#[test]
fn optional() -> Result<(), &'static str> {
    test_file("optional")
}

#[test]
fn resolution() -> Result<(), &'static str> {
    test_file("resolution")
}

#[test]
fn string() -> Result<(), &'static str> {
    test_file("string")
}

#[test]
fn traits() -> Result<(), &'static str> {
    test_file("traits")
}

#[test]
fn types() -> Result<(), &'static str> {
    test_file("type")
}

#[test]
fn vector() -> Result<(), &'static str> {
    test_file("vector")
}

// #[test]
// fn gauntlet() -> Result<(), &'static str> {
//     let i = fs::read_dir("/Users/jakeheiser/Desktop/Projects/Rust/aquila/tests/aquila").unwrap();
//     for file in i {
//         println!(
//             "Testing file: {}",
//             file.as_ref().ok().unwrap().path().to_str().unwrap()
//         );
//         test_file(file.unwrap().path())?;
//     }
//     Ok(())
// }

fn test_file(file_root: &str) -> Result<(), &'static str> {
    let path = format!(
        "/Users/jakeheiser/aquila/tests/aquila/{}.aq",
        file_root
    );
    let path = PathBuf::from(&path);

    let read = fs::read_to_string(path).unwrap();
    let lines: Vec<_> = read.lines().collect();

    if lines[0] == "/// skip" {
        println!("Skipping");
        return Ok(());
    }

    let mut main: Vec<String> = Vec::new();
    let mut expected_lines: Vec<String> = Vec::new();
    let mut fail_zones: Vec<Vec<String>> = Vec::new();
    let mut in_failable_zone = false;

    for line in lines {
        if line.starts_with("/// ") {
            let chopped_line = &line[4..];
            if chopped_line.starts_with("->") {
                if in_failable_zone {
                    panic!()
                }
                let expected = &line[7..];
                expected_lines.push(String::from(expected));
            } else if chopped_line.starts_with("error") {
                if in_failable_zone {
                    panic!()
                }
                in_failable_zone = true;
                fail_zones.push(Vec::new());
            } else if chopped_line.starts_with("end") {
                if !in_failable_zone {
                    panic!()
                }
                in_failable_zone = false;
            } else {
                panic!("Unknown control sequence: {}", chopped_line)
            }
        } else {
            let line = String::from(line);
            if in_failable_zone {
                fail_zones.last_mut().unwrap().push(line);
            } else {
                main.push(line);
            }
        }
    }

    fs::create_dir_all("/tmp/aquila").unwrap();

    let main_file_path = format!("/tmp/aquila/{}_main.aq", file_root);
    let mut main_file = File::create(main_file_path.clone()).unwrap();
    for line in &main {
        writeln!(main_file, "{}", line).unwrap();
    }
    expect_success(Path::new(&main_file_path).to_path_buf(), expected_lines)?;

    for (index, fail_zone) in fail_zones.iter().enumerate() {
        let file_path = format!("/tmp/aquila/{}_fail_{}.aq", file_root, index);
        let mut file = File::create(file_path.clone()).unwrap();
        for line in &main {
            writeln!(file, "{}", line).unwrap();
        }
        for line in fail_zone {
            writeln!(file, "{}", line).unwrap();
        }
        expect_failure(Path::new(&file_path).to_path_buf(), index)?;
    }

    Ok(())
}

fn expect_success(file: PathBuf, expected_output: Vec<String>) -> Result<(), &'static str> {
    let (output, diagnostics) = run_file(file);

    if !diagnostics.is_empty() {
        println!("Diagnostics:");
        for diag in diagnostics {
            println!("{}", diag);
        }
        return Err("Expected no diagnostics");
    }

    if output.lines().count() != expected_output.len() {
        println!("Got: {:?}", output.lines().collect::<Vec<_>>());
        println!("Expected: {:?}", expected_output);
        return Err("Wrong number of output lines");
    }

    for (index, (got, expected)) in output.lines().zip(expected_output).enumerate() {
        if got != expected {
            println!(
                "Expected line {}: expected '{}', got '{}'",
                index + 1,
                expected,
                got
            );
            return Err("Invalid output");
        }
    }

    Ok(())
}

fn expect_failure(file: PathBuf, index: usize) -> Result<(), &'static str> {
    let (_, diagnostics) = run_file(file);

    if diagnostics.is_empty() {
        println!("Expected diagnostics in failure zone {}", index + 1);
        Err("Expected diagnostics")
    } else {
        Ok(())
    }
}

fn run_file(file: PathBuf) -> (String, Vec<Diagnostic>) {
    let (reporter, mut diagnostics) = TestReporter::new();

    let source = aquila::source::file(file.to_str().unwrap());
    match aquila::run_with_reporter(source, reporter, true) {
        Ok(_) => {
            assert_eq!(diagnostics.unwrap().len(), 0);

            let output =
                Command::new("/Users/jakeheiser/aquila/build/program")
                    .output()
                    .unwrap();

            let got_lines = String::from_utf8(output.stdout).unwrap();

            if !output.status.success() {
                println!("Output:\n{}", got_lines);
                panic!()
            }

            (got_lines, Vec::new())
        }
        Err(_) => (String::new(), diagnostics.unwrap()),
    }
}
