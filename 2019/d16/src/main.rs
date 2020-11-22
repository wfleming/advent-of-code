mod fft;

use std::env::args;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn read_signal() -> fft::Signal {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let file = File::open(input).expect("couldn't open file");
    let mut buffered = BufReader::new(file);
    let mut line = String::new();
    buffered.read_line(&mut line).expect("no line?");

    let starting_signal: fft::Signal = line
        .trim()
        .chars()
        .map(|c| (c.to_digit(10).unwrap() as i32))
        .collect();

    starting_signal
}

fn main() {
    let signal = read_signal();

    // p1
    let p100 = fft::apply_phase_n(signal, 100);
    let first8 = p100
        .iter()
        .take(8)
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join("");
    println!("p1: after 100 phases, first 8 digits are {}", first8);
}
