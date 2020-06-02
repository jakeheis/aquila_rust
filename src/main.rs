use aquila;

fn main() {
    let source = aquila::text("4 < 5 && 1 + 6 > 2 == true");
    aquila::run(source);
}
