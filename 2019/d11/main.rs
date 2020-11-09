mod robot;

use std::env::args;
use intcode::{machine::Machine, tape};
// use intcode::num_traits::cast::FromPrimitive;

use crate::robot::Robot;

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let tape = tape::from_file(&input);

    // p1
    let mut machine = Machine::new(tape.clone());
    let mut robot = Robot::new();
    robot.run(&mut machine);
    println!("p2: panels painted = {:?}", robot.panels_count());
    println!("p2: outputs = {:?}", machine.outputs.len());
}

#[cfg(test)]
mod test {
    // use super::*;

    #[test]
    fn test_p1() {
        // TODO?
    }
}
