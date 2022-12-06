use std::collections::BTreeSet;
use std::env::args;
use std::fs;

// returns the index of the last character of the first n-length sequence of unique chars
fn unique_seq(datastream: &str, n: usize) -> usize {
    for i in 0..=(datastream.len() - n) {
        let chars: BTreeSet<char> = datastream[i..=(i + n - 1)].chars().collect();

        if chars.len() == n {
            return i + n - 1;
        }
    }

    panic!("Found no header");
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let datastream = fs::read_to_string(input_path)
        .expect("read the input")
        .trim()
        .to_string();

    let p1 = unique_seq(&datastream, 4);
    println!("p1: packet header ends at {}th char", p1 + 1);

    let p2 = unique_seq(&datastream, 14);
    println!("p2: message header ends at {}th char", p2 + 1);
}
