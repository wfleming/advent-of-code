mod camera;
mod droid;

use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;
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

    // p2
    let mut tape2 = tape.clone();
    tape2[0] = BigInt::from_i32(2).unwrap(); // tell it to run the robot
    let mut machine2 = Machine::new(tape2);
    droid::input_program(&mut machine2);
    droid::run_program(&mut machine2, v.points.len());
    let result = &machine2.outputs[machine2.outputs.len() - 1];
    println!("p2: result is {}", result);
}
