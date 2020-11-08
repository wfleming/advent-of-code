use num_traits::cast::FromPrimitive;
use num_traits::cast::ToPrimitive;
use num_traits::{Zero, One};
use num_bigint::{BigInt};
use std::vec::Vec;
use std::collections::HashMap;

#[derive(Debug)]
#[derive(Clone)]
pub struct Machine {
    tape: HashMap<BigInt, BigInt>,
    pos: BigInt,
    inputs: Vec<BigInt>,
    pub outputs: Vec<BigInt>,
    relative_base: BigInt,
}

impl Machine {
    pub fn new(tape: Vec<BigInt>) -> Machine {
        // convert the vec into a hashmap to support sparse tapes using bigints
        let mut tape_map: HashMap<BigInt, BigInt> = HashMap::new();

        for (pos, v) in tape.iter().enumerate() {
            tape_map.insert(
                BigInt::from_usize(pos).expect("should be parseable"),
                v.clone(),
            );
        }

        Machine {
            tape: tape_map,
            pos: Zero::zero(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            relative_base: Zero::zero(),
        }
    }

    pub fn push_input(&mut self, v: BigInt) {
        self.inputs.push(v);
    }

    pub fn run_until_exit(&mut self) {
        while !self.is_exited() {
            self.step();
        }
    }

    pub fn run_until_exit_or_input_wait(&mut self) {
        loop {
            if self.is_exited() {
                break;
            }
            if self.cur_opcode() == 3 && self.inputs.len() == 0 {
                break;
            }
            self.step();
        }
    }

    pub fn outputs_to_s(&self) -> Vec<String> {
        self.outputs.iter().map(|x| x.to_str_radix(10)).collect()
    }


    fn step(&mut self) {
        // println!(
        //     "DEBUG: step pos={} rel_base={} opcode={} from tape_at_pos={} tape_at_100={} tape_at_101={}",
        //     self.pos, self.relative_base, self.cur_opcode(), self.tape_at(self.pos.clone()), self.tape_at(BigInt::from_i32(100).unwrap()), self.tape_at(BigInt::from_i32(101).unwrap())
        // );
        match self.cur_opcode() {
            1 => self.add(),
            2 => self.mult(),
            3 => self.read_input(),
            4 => self.write_output(),
            5 => self.jmp_if_true(),
            6 => self.jmp_if_false(),
            7 => self.less_than(),
            8 => self.equals(),
            9 => self.mod_relative_base(),
            99 => (), // halt
            _ => panic!("bad machine state, invalid opcode from '{:?}'", self.tape.entry(self.pos.clone())),
        };
    }

    fn add(&mut self) {
        let l = self.nth_param(1);
        let r = self.nth_param(2);
        let dest_pos = self.nth_literal_param(3);
        // println!("DEBUG: add {}, {} -> [{}]", l, r, dest_pos);
        self.tape.insert(dest_pos, l + r);
        self.pos += 4;
    }

    fn mult(&mut self) {
        let l = self.nth_param(1);
        let r = self.nth_param(2);
        let dest_pos = self.nth_literal_param(3);
        // println!("DEBUG: mult {}, {} -> [{}]", l, r, dest_pos);
        self.tape.insert(dest_pos, l * r);
        self.pos += 4;
    }

    fn read_input(&mut self) {
        // println!("DEBUG: machine reading inputs. input queue={:?}", self.inputs);
        let dest_pos = self.nth_literal_param(1);
        self.tape.insert(dest_pos, self.inputs.remove(0));
        // println!("DEBUG: machine reading inputs. got dest_pos={}, input={}, full tape={:?}", dest_pos, self.tape[dest_pos], self.tape);
        self.pos += 2;
    }

    fn write_output(&mut self) {
        let o = self.nth_param(1);
        // println!("DEBUG: output {}", o);
        self.outputs.push(o);
        self.pos += 2;
    }

    fn jmp_if_true(&mut self) {
        if self.nth_param(1) != Zero::zero() {
            self.pos = self.nth_param(2);
        } else {
            self.pos += 3;
        }
    }

    fn jmp_if_false(&mut self) {
        if self.nth_param(1) == Zero::zero() {
            self.pos = self.nth_param(2);
        } else {
            self.pos += 3;
        }
    }

    fn less_than(&mut self) {
        let dest_pos = self.nth_literal_param(3);
        if self.nth_param(1) < self.nth_param(2) {
            self.tape.insert(dest_pos, One::one());
        } else {
            self.tape.insert(dest_pos, Zero::zero());
        }
        self.pos += 4;
    }

    fn equals(&mut self) {
        let dest_pos = self.nth_literal_param(3);
        if self.nth_param(1) == self.nth_param(2) {
            self.tape.insert(dest_pos, One::one());
        } else {
            self.tape.insert(dest_pos, Zero::zero());
        }
        self.pos += 4;
    }

    fn mod_relative_base(&mut self) {
        self.relative_base += self.nth_param(1);
        self.pos += 2;
    }

    fn nth_literal_param(&self, n: u32) -> BigInt {
        let val = self.tape_at(self.pos.clone() + n);
        match self.nth_param_mode(n) {
            0 => val,
            1 => panic!("invalid use of mode 1"),
            2 => self.relative_base.clone() + val,
            _ => panic!("invalid mode"),
        }
    }

    fn nth_param(&self, n: u32) -> BigInt {
        let val = self.tape_at(self.pos.clone() + n);

        match self.nth_param_mode(n) {
            0 => self.tape_at(val),
            1 => val,
            2 => self.tape_at(self.relative_base.clone() + val),
            _ => panic!("invalid mode"),
        }
    }

    fn is_exited(&self) -> bool {
          99 == self.cur_opcode()
    }

    fn tape_at(&self, pos: BigInt) -> BigInt {
        self.tape.get(&pos).unwrap_or(&Zero::zero()).clone()
    }

    fn nth_param_mode(&self, n: u32) -> u32 {
        assert!(n >= 1 && n <=3);
        let div = BigInt::from_i32((10 as i32).pow(n + 1)).expect("div a bigint");
        let ten = BigInt::from_i32(10).expect("parse 10");
        let v_big = (self.tape_at(self.pos.clone()) / div) % ten;
        v_big.to_u32().expect("should be small enough")
    }

    fn cur_opcode(&self) -> i32 {
        // each digit in the tape entry is (comma sep):
        // p3mode,p2mode,p1mode,0,opcode
        let one_hundred = BigInt::from_i32(100).expect("100");
        let tape_v: BigInt = self.tape_at(self.pos.clone());
        let op_big: BigInt = tape_v % one_hundred;
        op_big.to_i32().expect("should be small enough")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tape::from_str;

    #[test]
    fn test_bigint_math() {
        let x = BigInt::from_i32(100).unwrap();
        let y = x + 1;
        assert_eq!(BigInt::from_i32(101).unwrap(), y);
    }

    #[test]
    fn test_d9_p1_quine() {
        let tape = from_str("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99");

        let mut machine = Machine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(tape, machine.outputs);
    }

    #[test]
    fn test_d9_p1_a() {
        let tape = from_str("1102,34915192,34915192,7,4,7,99,0");

        let mut machine = Machine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(16, machine.outputs[0].to_str_radix(10).len());
    }

    #[test]
    fn test_d9_p1_b() {
        let tape = from_str("104,1125899906842624,99");

        let mut machine = Machine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(tape[1], machine.outputs[0]);
    }
}
