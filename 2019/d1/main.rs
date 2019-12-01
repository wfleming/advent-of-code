use std::env::args;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::vec::Vec;

fn read_masses(path: String) -> Vec<i32> {
    let mut ns = Vec::new();
    let file = File::open(path).expect("couldn't open file");
    let buffered = BufReader::new(file);

    for line in buffered.lines() {
        let x: i32 = line.expect("no line?").parse().expect("not an int?");
        ns.push(x);
    }

    ns
}

fn fuel_for_mass(mass: i32) -> i32 {
    (mass / 3) - 2
}

fn fuel_for_mass_recurse(mass: i32) -> i32 {
    let f = fuel_for_mass(mass);

    if f <= 0 {
        0
    } else {
        f + fuel_for_mass_recurse(f)
    }
}

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let masses = read_masses(input);

    let fuel_amts = masses.iter()
        .map(|m| fuel_for_mass(*m))
        .collect::<Vec<i32>>();
    let fuel_tot: i32 = fuel_amts.iter().sum();

    println!("p1: fuel needed = {}", fuel_tot);

    let fuel_tot_2: i32 = masses.iter()
        .map(|m| fuel_for_mass_recurse(*m))
        .sum();
    println!("p2: fuel needed = {}", fuel_tot_2);
}
