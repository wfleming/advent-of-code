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
    pub hull: HashMap<Point, i32>,
    pub dir: Dir,
    pub pos: Point,
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
                }

                output_idx += 1;
            }
        }
    }

    pub fn paint_panel(&mut self, pos: Point, color: i32) {
      self.hull.insert(pos, color);
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

    // map the hull hashmap to a string. ' ' is black, # is white
    pub fn hull_str(&self) -> String {
        let mut min_x = i32::MAX;
        let mut max_x = i32::MIN;
        let mut min_y = i32::MAX;
        let mut max_y = i32::MIN;

        // get the bounds of of the coordinates
        for point in self.hull.keys() {
            let x = point.0.to_i32().expect("pretty sure coordinates fit in i32 for this one");
            let y = point.1.to_i32().expect("pretty sure coordinates fit in i32 for this one");

            if x < min_x { min_x = x }
            if x > max_x { max_x = x }
            if y < min_y { min_y = y }
            if y > max_y { max_y = y }
        }

        println!("DEBUG: min_x = {}, max_x = {}, min_y = {}, max_y = {}", min_x, max_x, min_y, max_y);

        let mut hull_str = String::new();
        hull_str.reserve(((max_x - min_x) * (max_y - min_y)) as usize);

        // go row-by-row
        for y in min_y..(max_y + 1) {
            // col-by-col, get color at position & append to string
            for x in min_x..(max_x + 1) {
                let p = (BigInt::from_i32(x).unwrap(), BigInt::from_i32(y).unwrap());
                let c = *self.hull.get(&p).unwrap_or(&0);

                match c {
                    0 => hull_str.push_str(" "),
                    1 => hull_str.push_str("#"),
                    _ => panic!("{:?} is not a valid color on the hull", c),
                }
            }

            // at end of row, newline
            hull_str.push_str("\n")
        }

        hull_str
    }
}

