use aquila;
use env_logger;

fn main() -> Result<(), &'static str> {
    env_logger::init();

    let args: Vec<_> = std::env::args().collect();
    let file_name = if args.len() > 1 {
        &args[1]
    } else {
        "/Users/jakeheiser/Desktop/Projects/Rust/aquila/test.aq"
    };
    let source = aquila::file(file_name);
    aquila::run(source)
}
