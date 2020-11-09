use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;
use intcode::num_traits::cast::ToPrimitive;
use intcode::num_traits::{Zero,One};
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
        while !machine.is_exited() {
            machine.run_until_exit_or_input_wait();

            // give input of current panel color
            machine.push_input(BigInt::from_i32(self.current_color()).unwrap());

            // if there's output, the bot already painted
            if machine.outputs.len() >= 2 && !machine.is_exited() {
                // use the output to set hull & change dir/pos
                let color = machine.outputs.get(machine.outputs.len() - 2).unwrap();
                let turn = machine.outputs.get(machine.outputs.len() - 1).unwrap();

                self.hull.insert(self.pos.clone(), color.to_i32().expect("should be 0 or 1"));
                self.dir = self.next_dir(turn.to_i32().expect("should be 0 or 1"));
                self.pos = self.next_pos();
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

