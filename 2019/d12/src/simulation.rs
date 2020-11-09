use crate::moon::Moon;
use std::vec::Vec;

#[derive(Clone)]
#[derive(Debug)]
pub struct Simulation {
    moons: Vec<Moon>,
}

impl Simulation {
    pub fn new(moons: Vec<Moon>) -> Simulation {
        Simulation {
            moons: moons,
        }
    }

    pub fn step_n(&mut self, steps: u32) {
        for _ in 0..steps {
            self.step();
        }
    }

    pub fn step(&mut self) {
        self.apply_gravity();
        self.apply_velocity();
    }

    fn apply_gravity(&mut self) {
        // each unique pair of moons must be compared, but only once.
        for idx1 in 0..self.moons.len() {
            // for inner loop, only loop for moons after moon1, to avoid comparing (a,b) and then
            // later comparing (b, a)
            for idx2 in (idx1 + 1)..self.moons.len() {
                // TODO - I wrestled with the borrow checker here a while to clean this up, and got
                // stuck. This is ugly for now.

                if self.moons[idx1].pos.0 < self.moons[idx2].pos.0 {
                    self.moons[idx1].vel.0 += 1;
                    self.moons[idx2].vel.0 -= 1;
                } else if self.moons[idx2].pos.0 < self.moons[idx1].pos.0 {
                    self.moons[idx2].vel.0 += 1;
                    self.moons[idx1].vel.0 -= 1;
                }

                if self.moons[idx1].pos.1 < self.moons[idx2].pos.1 {
                    self.moons[idx1].vel.1 += 1;
                    self.moons[idx2].vel.1 -= 1;
                } else if self.moons[idx2].pos.1 < self.moons[idx1].pos.1 {
                    self.moons[idx2].vel.1 += 1;
                    self.moons[idx1].vel.1 -= 1;
                }

                if self.moons[idx1].pos.2 < self.moons[idx2].pos.2 {
                    self.moons[idx1].vel.2 += 1;
                    self.moons[idx2].vel.2 -= 1;
                } else if self.moons[idx2].pos.2 < self.moons[idx1].pos.2 {
                    self.moons[idx2].vel.2 += 1;
                    self.moons[idx1].vel.2 -= 1;
                }
            }
        }
    }

    fn apply_velocity(&mut self) {
        for mut moon in &mut self.moons {
            moon.pos = (
                moon.pos.0 + moon.vel.0,
                moon.pos.1 + moon.vel.1,
                moon.pos.2 + moon.vel.2
            )
        }
    }

    pub fn total_energy(&self) -> i32 {
        self.moons.iter().map(|moon| {
            let pot = moon.pos.0.abs() + moon.pos.1.abs() + moon.pos.2.abs();
            let kin = moon.vel.0.abs() + moon.vel.1.abs() + moon.vel.2.abs();
            (pot * kin)
        }).sum()
    }
}

// leftover from attempt at making the above less repetitive
// handle the gravity/velocity on 1 axis for 2 moons
// fn moon_gravity<F1, F2>(moon1: &mut Moon, moon2: &mut Moon, get: F1, set: F2)
//     where F1: Fn(&Moon) -> i32, F2: Fn(&mut Moon, i32) -> () {
//     if get(moon1) < get(moon2) {
//         set(moon1, get(moon1) + 1);
//         set(moon2, get(moon2) - 1);
//     } else if get(moon2) < get(moon1) {
//         set(moon2, get(moon2) + 1);
//         set(moon1, get(moon1) - 1);
//     }
// }

#[cfg(test)]
mod test {
    use super::*;

    fn example_1_initial_moons() -> Vec<Moon> {
        vec![
            Moon { pos: (-1, 0, 2), vel: (0, 0, 0) },
            Moon { pos: (2, -10, -7), vel: (0, 0, 0) },
            Moon { pos: (4, -8, 8), vel: (0, 0, 0) },
            Moon { pos: (3, 5, -1), vel: (0, 0, 0) },
        ]
    }

    #[test]
    fn test_example_1_1_step() {
        let mut sim = Simulation::new(example_1_initial_moons());
        sim.step();

        assert_eq!(
            sim.moons[0],
            Moon {
                pos: (2, -1, 1),
                vel: (3, -1, -1),
            }
        )
    }

    #[test]
    fn test_example_1_10_steps() {
        let mut sim = Simulation::new(example_1_initial_moons());
        sim.step_n(10);
        assert_eq!(sim.total_energy(), 179);
    }
}
