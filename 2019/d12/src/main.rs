mod moon;
mod parser;
mod simulation;

use parser::*;
use simulation::*;
use std::env::args;

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let moons = parse_file(&input);

    // p1
    let mut sim1 = Simulation::new(moons.clone());
    sim1.step_n(1000);
    println!(
        "p1: after 1000 steps, total energy = {}",
        sim1.total_energy()
    );

    // p2
    let mut sim2 = Simulation::new(moons.clone());
    let cycles = sim2.find_cycles();
    println!("p2: cycling took {} steps", cycles);
}
