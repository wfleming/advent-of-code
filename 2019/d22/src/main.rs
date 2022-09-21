mod cards;

use std::fs;
use std::env::args;

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");

    let input_contents = fs::read_to_string(input_path).expect("File not readable");
    let shuffle_lines: Vec<&str> = input_contents.lines().collect();
    let d0 = cards::new_deck(10007);
    let p1_d = cards::execute_shuffle(d0, shuffle_lines);
    let p1_pos = p1_d.iter().position(|&c| c == 2019).expect("deck should certainly still contain 2019");
    println!("p1: card 2019 is at position {}", p1_pos);
}
