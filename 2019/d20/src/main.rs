mod maze;

use maze::Maze;
use std::env::args;

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");

    let m1 = Maze::from_file(&input);
    let p1 = maze::find_path(&m1);
    println!("p1: {} steps", p1.len() - 1);

    let p2 = maze::find_path_p2(&m1);
    println!("p2: {} steps", p2.len() - 1);
}
