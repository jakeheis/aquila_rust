use aquila;

fn main() {
    let source = aquila::Source::text("4 + 5 * 7 + 6 + 7 * 1");
    aquila::run(source);
}
