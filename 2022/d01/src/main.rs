use std::env::args;
use std::io::{self,BufRead};
use std::fs;

fn parse_elves(input_path: &str) -> Vec<u32> {
    let f = fs::File::open(input_path).expect("Input path should exist");
    let br = io::BufReader::new(f);

    let mut e = 0;
    let mut elves = vec![];

    for lr in br.lines() {
        let l = lr.expect("Should not have errors reading lines");
        if l.len() == 0 {
            elves.push(e);
            e = 0;
        } else {
            let c = l.parse::<u32>().expect("Line should be number");
            e += c;
        }
    }
    elves.push(e);

    return elves;
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let mut elves = parse_elves(&input_path);

    elves.sort_by(|a, b| b.partial_cmp(a).unwrap());
    println!("p1: elf with most calories has {}", elves[0]);
    println!("p2: top three elves have {}", elves[0] + elves[1] + elves[2]);
}
