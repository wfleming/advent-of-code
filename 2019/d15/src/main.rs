mod mapper;
mod oxy_fill;

use intcode::{machine::Machine, tape};
use mapper::*;
use oxy_fill::*;
use std::env::args;

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let tape = tape::from_file(&input);

    // p1
    let mut machine1 = Machine::new(tape.clone());
    let mut map = discover_map(&mut machine1);
    println!("p1 - found the oxy");
    println!("{}", map);
    println!("p1 - finding the optimal path");
    map.droid = (0, 0); // it should already be there, but let's be sure
    let path = a_star(map.clone());
    println!("p1 - it takes {} steps to get to the oxy", path.len());

    // p2
    let mut p2_map = OxyMap::from_map(&map);
    println!(
        "p2 - it took {} minutes to saturate area with oxygen",
        p2_map.fill()
    );
}
