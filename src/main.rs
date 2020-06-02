use aquila;

fn main() {
    let source = aquila::Source::text("4 < 5 && 1 + 6 > 2 == true");
    aquila::run(source);
}
