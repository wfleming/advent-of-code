use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;
use intcode::num_traits::cast::ToPrimitive;
use intcode::num_traits::Zero;
use intcode::{machine::Machine};
use std::collections::HashMap;

pub type Point = (BigInt, BigInt);

pub enum Dir {
    Up,
    Down,
    Left,
    Right
}

pub struct Robot {
    hull: HashMap<Point, i32>,
    dir: Dir,
    pos: Point,
}

impl Robot {
    pub fn new() -> Robot {
        Robot {
            dir: Dir::Up,
            hull: HashMap::new(),
            pos: (Zero::zero(), Zero::zero()),
        }
    }

    pub fn run(&mut self, machine: &mut Machine) {
        let mut output_idx: usize = 0;

        while !machine.is_exited() {
            // give input of current panel color if asked
            if machine.is_waiting_for_input() {
                machine.push_input(BigInt::from_i32(self.current_color()).unwrap());
            }

            machine.step();

            // read output
            while output_idx < machine.outputs.len() {
                let out_val = machine.outputs.get(output_idx).unwrap().to_i32().expect("should be 0 or 1");

                // color output
                if output_idx % 2 == 0 { // color output
                    self.hull.insert(self.pos.clone(), out_val);
                } else { // turn output
                    self.dir = self.next_dir(out_val);
                    self.pos = self.next_pos();

                    // provide next input of new current position color
                    // machine.push_input(BigInt::from_i32(self.current_color()).unwrap());
                }

                output_idx += 1;
            }
        }
    }

    pub fn panels_count(&self) -> usize {
        self.hull.len()
    }

    fn current_color(&self) -> i32 {
        // default color is 0 (black)
        *self.hull.get(&self.pos).unwrap_or(&0)
    }

    fn next_pos(&self) -> Point {
        match self.dir {
            Dir::Up => (self.pos.0.clone(), self.pos.1.clone() + 1),
            Dir::Down => (self.pos.0.clone(), self.pos.1.clone() - 1),
            Dir::Left => (self.pos.0.clone() - 1, self.pos.1.clone()),
            Dir::Right => (self.pos.0.clone() + 1, self.pos.1.clone()),
        }
    }

    fn next_dir(&self, turn: i32) -> Dir {
        match turn {
            // 90 deg left
            0 => match self.dir {
                Dir::Up => Dir::Left,
                Dir::Left => Dir::Down,
                Dir::Down => Dir::Right,
                Dir::Right => Dir::Up,
            },
            // 90 deg right
            1 => match self.dir {
                Dir::Up => Dir::Right,
                Dir::Right => Dir::Down,
                Dir::Down => Dir::Left,
                Dir::Left => Dir::Up,
            },
            _ => panic!("unexpected output for turn: {:?}", turn),
        }
    }
}

