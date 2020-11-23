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
    let mut machine = Machine::new(tape.clone());
    machine.push_input(BigInt::from_i32(1).expect("1 should parse"));
    machine.run_until_exit();
    println!("p1: output = {:?}", machine.outputs_to_s());

    // p2
    let mut machine = Machine::new(tape.clone());
    machine.push_input(BigInt::from_i32(2).expect("2 should parse"));
    machine.run_until_exit();
    println!("p2: output = {:?}", machine.outputs_to_s());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bigint_math() {
        let x = BigInt::from_i32(100).unwrap();
        let y = x + 1;
        assert_eq!(BigInt::from_i32(101).unwrap(), y);
    }

    #[test]
    fn test_p1_quine() {
        let tape = tape::from_str("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99");

        let mut machine = Machine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(tape, machine.outputs);
    }

    #[test]
    fn test_p1_a() {
        let tape = tape::from_str("1102,34915192,34915192,7,4,7,99,0");

        let mut machine = Machine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(16, machine.outputs[0].to_str_radix(10).len());
    }

    #[test]
    fn test_p1_b() {
        let tape = tape::from_str("104,1125899906842624,99");

        let mut machine = Machine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(tape[1], machine.outputs[0]);
    }
}
