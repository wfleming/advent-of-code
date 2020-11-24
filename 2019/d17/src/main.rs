mod camera;

// use intcode::num_bigint::BigInt;
// use intcode::num_traits::cast::FromPrimitive;
use intcode::{machine::Machine, tape};
use std::env::args;

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let tape = tape::from_file(&input);

    // p1
    let mut machine1 = Machine::new(tape.clone());
    let v = camera::View::read_machine(&mut machine1);
    println!("p1: camera's calibration sum is {}", v.calibrate());
}
