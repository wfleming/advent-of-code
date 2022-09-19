use std::collections::HashSet;
use intcode::{machine::Machine, tape::Tape};
use intcode::num_traits::ToPrimitive;
use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;
// use std::fmt;

#[derive(Debug)]
pub struct TractorBeam {
    tape: Tape,
    pub points: HashSet<(i64,i64)>,
}

impl TractorBeam {
    pub fn new(tape: Tape) -> TractorBeam {
        TractorBeam{tape: tape, points: HashSet::new()}
    }

    pub fn scan_area(&mut self, max_x: i64, max_y: i64) {
        for x in 0..(max_x + 1) {
            for y in 0..(max_y + 1) {
                // println!("scanning {}, {}", x, y);
                let mut m = Machine::new(self.tape.clone());
                m.push_input(BigInt::from_i64(x).unwrap());
                m.push_input(BigInt::from_i64(y).unwrap());
                m.run_until_exit();

                if m.outputs[0].to_i32() == Some(1) {
                    self.points.insert((x,y));
                }
            }
        }
    }
}
