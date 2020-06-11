use aquila;

fn main() -> Result<(), &'static str> {
    let source = aquila::file("test.aq");
    aquila::run(source)
}
