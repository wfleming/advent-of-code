mod parser;
mod reaction;

use std::env::args;
use parser::*;
use reaction::*;

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let reactions = parse_file(&input);

    // p1
    let factory = Factory::new(reactions);
    let reaction = factory.find_reaction("FUEL").unwrap();
    let production = factory.produce_reaction(reaction);
    let ore_amt = production.consumed.iter().find(|(_amt, mat)| mat == "ORE").unwrap().0;
    println!("p1: to create 1 FUEL you need {:?}", ore_amt);
    // first run - got 15332478, that's too high
    // I needed to share tracking of leftovers with every iteration
    // correct: 301997

    // p2
    let ore_per_trillion = factory.fuel_from_ore(ONE_TRILLION);
    println!("p2: from {} ORE, we'll get {} FUEL", ONE_TRILLION, ore_per_trillion);
}
