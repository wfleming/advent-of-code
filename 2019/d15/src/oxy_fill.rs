use crate::mapper::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum InteriorState {
    Oxygen,
    NotOxygen,
}

#[derive(Debug)]
pub struct OxyMap {
    points: HashMap<Point, InteriorState>, // points droid can access - includes current droid pos & oxy point (if known)
}

fn neighbors(p: &Point) -> Vec<Point> {
    vec![
        (p.0, p.1 - 1),
        (p.0, p.1 + 1),
        (p.0 - 1, p.1),
        (p.0 + 1, p.1),
    ]
}

impl OxyMap {
    pub fn from_map(map: &Map) -> OxyMap {
        let pts: HashMap<Point, InteriorState> = map
            .interior
            .iter()
            .map(|pt| {
                if *pt == map.oxy.unwrap() {
                    (pt.clone(), InteriorState::Oxygen)
                } else {
                    (pt.clone(), InteriorState::NotOxygen)
                }
            })
            .collect();

        OxyMap { points: pts }
    }

    fn not_saturated(&self) -> bool {
        self.points
            .iter()
            .any(|(_pt, st)| st == &InteriorState::NotOxygen)
    }

    // returns minutes it takes for area to be full of oxygen
    pub fn fill(&mut self) -> u64 {
        let mut t = 0;

        while self.not_saturated() {
            t += 1;

            // do this in 2 passes to avoid jumping more than 1 spot in 1 tick
            let pts_to_fill: Vec<Point> = self
                .points
                .iter()
                .filter(|(pt, st)| {
                    st == &&InteriorState::NotOxygen
                        && neighbors(pt)
                            .iter()
                            .any(|pt2| self.points.get(pt2) == Some(&InteriorState::Oxygen))
                })
                .map(|(pt, _st)| pt.clone())
                .collect();

            pts_to_fill.iter().for_each(|pt| {
                self.points.insert(pt.clone(), InteriorState::Oxygen);
            });
        }

        t
    }
}
