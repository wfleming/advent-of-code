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
    println!("p1: panels painted = {:?}", robot.panels_count());
    println!("p1: outputs = {:?}", machine.outputs.len());

    // p2
    let mut machine = Machine::new(tape.clone());
    let mut robot = Robot::new();
    // set starting panel to white
    robot.paint_panel(robot.pos.clone(), 1);
    robot.run(&mut machine);
    println!("p2: printing the hull below");
    println!("{}", robot.hull_str());
}

#[cfg(test)]
mod test {
    // use super::*;

    #[test]
    fn test_p1() {
        // TODO?
    }
}
