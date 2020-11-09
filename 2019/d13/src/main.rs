mod game;

use game::*;
use intcode::{machine::Machine, tape};
use std::env::args;

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let tape = tape::from_file(&input);

    // p1
    let mut machine1 = Machine::new(tape.clone());
    let mut game1 = Game::new();
    game1.run(&mut machine1);
    let blocks_count = game1.tiles.iter().filter(
        |(_key, tile_type)| **tile_type == Entity::Block
        ).count();
    println!("p1: there are {} blocks", blocks_count);
}
