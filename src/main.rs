use aquila;
use env_logger;

fn main() -> Result<(), &'static str> {
    env_logger::init();

    let source = aquila::file("test.aq");
    aquila::run(source)
}
