use std::boxed::Box;
use std::collections::HashSet;
use std::env::args;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::vec::Vec;

fn read_deltas(path: String) -> Vec<i32> {
    // yes, unwrap is a no-no, I know, don't care here
    let mut deltas = Vec::new();
    let file = File::open(path).unwrap();
    let buffered = BufReader::new(file);

    for line in buffered.lines() {
        let x = str::parse::<i32>(&line.unwrap()).unwrap();
        deltas.push(x);
    }

    return deltas;
}

fn find_repeat(deltas: Vec<i32>) -> i32 {
    let mut seen: HashSet<i32> = HashSet::new();
    let mut next: i32 = 0;
    let mut delta_idx = 0;

    while !seen.contains(&next) {
        seen.insert(next);
        next = next + deltas[delta_idx];
        delta_idx += 1;
        if delta_idx >= deltas.len() {
            delta_idx = 0;
        }
    }

    return next;
}

fn main() {
    println!("Hello, world!");

    let mut argv = args();
    argv.next();
    let in_file = argv.next();
    match in_file {
        None => {
            println!("provide a file to read input from");
        },
        Some(path) => {
            println!("going to read from {}", path);
            let deltas: Vec<i32> = read_deltas(path);
            let deltas_box = Box::new(deltas);

            let tot: i32 = (*deltas_box).iter().sum();
            println!("p1: total = {}", tot);

            let repeat = find_repeat(*deltas_box);
            println!("p2: repeat = {}", repeat);
        }
    }
}
