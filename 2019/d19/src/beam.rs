use std::collections::BTreeMap;
use intcode::{machine::Machine, tape::Tape};
use intcode::num_traits::ToPrimitive;
use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;

pub struct TractorBeam {
    tape: Tape,
    pub points: BTreeMap<(u32,u32), bool>,
}

const SHIP_SIZE: u32 = 100;

impl TractorBeam {
    pub fn new(tape: Tape) -> TractorBeam {
        TractorBeam{tape: tape, points: BTreeMap::new()}
    }

    pub fn check_pt(&mut self, pt: &(u32, u32)) -> bool {
        if !self.points.contains_key(pt) {
            let mut m = Machine::new(self.tape.clone());
            m.push_input(BigInt::from_u32(pt.0).unwrap());
            m.push_input(BigInt::from_u32(pt.1).unwrap());
            m.run_until_exit();

            self.points.insert(pt.clone(), m.outputs[0].to_u32() == Some(1));
        }

        self.points[pt]
    }

    pub fn is_pt_ship(&mut self, pt: &(u32,u32)) -> bool {
        for xd in 0..SHIP_SIZE {
            for yd in 0..SHIP_SIZE {
              if !self.check_pt(&(pt.0 + xd, pt.1 + yd)) {
                  return false;
              }
            }
        }

        true
    }

    // returns top-left pt of ship if found
    pub fn find_ship(&mut self) -> Option<(u32, u32)> {
        for y in SHIP_SIZE..(SHIP_SIZE * 20) {
            // these constants based on my p1 output & ratios emitted, plus some leeway
            let min_x: u32 = (y as f32 * 0.7).floor() as u32;
            let max_x: u32 = (y as f32 * 0.9).floor() as u32;
            if max_x - min_x < SHIP_SIZE {
                continue;
            }
            for x in min_x..max_x {
                if self.is_pt_ship(&(x,y)) {
                    return Some((x,y));
                }
            }
        }

        None
    }
}
