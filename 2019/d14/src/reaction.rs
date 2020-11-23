use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::vec::Vec;

pub const ONE_TRILLION: i64 = 1000000000000;

fn div_ceil(x: i64, y: i64) -> i64 {
    if x % y == 0 {
        x / y
    } else {
        (x / y) + 1
    }
}

pub type MaterialAmount = (i64, String);

fn mat_vec_from_map(map: HashMap<String, i64>) -> Vec<MaterialAmount> {
    let mut vec: Vec<MaterialAmount> = map.iter().map(|(k, v)| (*v, k.to_string())).collect();
    vec.sort_by(|x, y| x.1.partial_cmp(&y.1).unwrap());

    vec
}

#[derive(Debug, PartialEq)]
pub struct Reaction {
    pub inputs: Vec<MaterialAmount>,
    pub output: MaterialAmount,
}

impl fmt::Display for Reaction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut str = String::new();

        let input_strs: Vec<String> = self
            .inputs
            .iter()
            .map(|i| format!("{} {}", i.0, i.1))
            .collect();
        str.push_str(&input_strs.join(", "));
        str.push_str(&format!(" -> {} {}", self.output.0, self.output.1));

        write!(f, "{}", str)
    }
}

#[derive(Debug, PartialEq)]
pub struct Production {
    pub consumed: Vec<MaterialAmount>,
    pub produced: Vec<MaterialAmount>,
}

#[derive(Debug, PartialEq)]
pub struct Factory {
    reactions: Vec<Reaction>,
}

//  let scale = div_ceil(i.0, r.output.0);
impl Factory {
    pub fn new(reactions: Vec<Reaction>) -> Factory {
        Factory {
            reactions: reactions,
        }
    }

    pub fn find_reaction(&self, produced: &str) -> Option<&Reaction> {
        self.reactions.iter().find(|x| x.output.1 == produced)
    }

    // descends through a reaction to its atomic inputs
    // e.g. if you have 10 ORE -> 10 A, 1 ORE -> 1 B, 7 A, 1 B -> 1 C, for the last reaction
    // this method would return { consumed: [11 ORE, 1 B, 7A], produced: [3 A, 1 C] }
    pub fn produce_reaction(&self, reaction: &Reaction) -> Production {
        self.produce_reaction_scale(reaction, 1)
    }

    pub fn produce_reaction_scale(&self, reaction: &Reaction, scale: i64) -> Production {
        let mut consumed: HashMap<String, i64> = HashMap::new();
        let mut produced: HashMap<String, i64> = HashMap::new();

        self.produce_reaction_iter(reaction, scale, &mut consumed, &mut produced);

        Production {
            consumed: mat_vec_from_map(consumed),
            produced: mat_vec_from_map(produced),
        }
    }

    pub fn produce_reaction_iter(
        &self,
        reaction: &Reaction,
        scale: i64,
        consumed: &mut HashMap<String, i64>,
        produced: &mut HashMap<String, i64>,
    ) {
        // the output is always produced
        produced
            .entry(reaction.output.1.clone())
            .and_modify(|x| *x += scale * reaction.output.0)
            .or_insert(scale * reaction.output.0);

        // println!("DEBUG produce_reaction {:?} at scale {}", reaction, scale);
        reaction.inputs.iter().for_each(|i| {
            let mut needed_amt = i.0 * scale;

            // println!("  DEBUG looking at input {:?}. consumed={:?} produced={:?}", i, consumed, produced);

            // if we have some left over from a previous reaction, use it
            if produced.contains_key(&i.1) {
                let avail = produced[&i.1];
                let use_of_avail = cmp::min(needed_amt, avail);
                // println!("    DEBUG produced has {} leftover, we need {}", avail, needed_amt);
                consumed
                    .entry(i.1.clone())
                    .and_modify(|x| *x += use_of_avail)
                    .or_insert(use_of_avail);
                produced
                    .entry(i.1.clone())
                    .and_modify(|x| *x -= use_of_avail);
                needed_amt -= use_of_avail;
                // delete any produced keys if empty now
                produced.retain(|_k, v| *v != 0);
            }

            if needed_amt > 0 {
                let maybe_in_r = self.find_reaction(&i.1);

                if let Some(in_r) = maybe_in_r {
                    // scale the production of in_r. E.g. if we need 14 A & in_r produces 10 A, we
                    // need to do it twice
                    let iter_scale = div_ceil(needed_amt, in_r.output.0);
                    self.produce_reaction_iter(in_r, iter_scale, consumed, produced);
                    // println!("  DEBUG after inner produce. input_mat={} consumed={:?} produced={:?}", i.1, consumed, produced);

                    // we'll now consume what that just produced
                    let now_avail = produced[&i.1];
                    if now_avail < needed_amt {
                        panic!(
                            "shouldn't have produced less {} than needed. needed {}, have {}",
                            i.1, needed_amt, now_avail
                        );
                    }
                    let use_of_avail = cmp::min(needed_amt, now_avail);
                    consumed
                        .entry(i.1.clone())
                        .and_modify(|x| *x += use_of_avail)
                        .or_insert(use_of_avail);
                    produced
                        .entry(i.1.clone())
                        .and_modify(|x| *x -= use_of_avail);
                    // delete any produced keys if empty now
                    produced.retain(|_k, v| *v != 0);
                } else {
                    // this was an atomic input, so it was scale * whatever consumed
                    consumed
                        .entry(i.1.clone())
                        .and_modify(|x| *x += scale * i.0)
                        .or_insert(scale * i.0);
                }
            }
        });
    }

    // given a certain amount of ore, how much fuel can we produce?
    pub fn fuel_from_ore(&self, target_ore_amt: i64) -> i64 {
        let fuel_r = self.find_reaction("FUEL").unwrap();
        if fuel_r.output.0 != 1 {
            panic!(
                "I'm assuming the reaction produced 1 FUEL since the inputs I've seen always do"
            );
        }

        // get the lower bound for the search
        let produce_min_fuel = self.produce_reaction_scale(fuel_r, 1);
        let ore_used_min = produce_min_fuel
            .consumed
            .iter()
            .find(|(_amt, mat)| mat == "ORE")
            .unwrap()
            .0;
        let min_bound = target_ore_amt / ore_used_min;

        // get the upper bound for the search by producing min_bound and calculating that ratio
        let produce_min_bound = self.produce_reaction_scale(fuel_r, min_bound);
        let ore_used_min = produce_min_bound
            .consumed
            .iter()
            .find(|(_amt, mat)| mat == "ORE")
            .unwrap()
            .0;
        let max_bound = target_ore_amt / (ore_used_min / min_bound);
        let produced_too_much = self.produce_reaction_scale(fuel_r, max_bound);
        let ore_used_high = produced_too_much
            .consumed
            .iter()
            .find(|(_amt, mat)| mat == "ORE")
            .unwrap()
            .0;
        if ore_used_high <= target_ore_amt {
            panic!("our upper-bound estimate is still too low!");
        }

        self.bisect(target_ore_amt, fuel_r, min_bound, max_bound)
    }

    fn bisect(&self, target_ore_amt: i64, fuel_r: &Reaction, low: i64, high: i64) -> i64 {
        let mut low = low;
        let mut high = high;

        while low < high {
            let mid = low + (high - low) / 2;
            let p = self.produce_reaction_scale(fuel_r, mid);
            let ore_used = p
                .consumed
                .iter()
                .find(|(_amt, mat)| mat == "ORE")
                .unwrap()
                .0;

            if ore_used == target_ore_amt {
                return mid;
            } else if ore_used < target_ore_amt {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }

        // it *should* be low - 1, but that seems to be 1 off - unsure where my bug is. related to
        // integer division & rounding? Just test `low` directly, then return low - 1 if it's too
        // high
        let p = self.produce_reaction_scale(fuel_r, low);
        let ore_used = p
            .consumed
            .iter()
            .find(|(_amt, mat)| mat == "ORE")
            .unwrap()
            .0;
        if ore_used <= target_ore_amt {
            return low;
        } else {
            return low - 1;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::*;

    fn ex_input() -> String {
        String::from(
            "10 ORE => 10 A\n\
             1 ORE => 1 B\n\
             7 A, 1 B => 1 C\n\
             7 A, 1 C => 1 D\n\
             7 A, 1 D => 1 E\n\
             7 A, 1 E => 1 FUEL",
        )
    }

    fn ex_input_2() -> String {
        String::from(
            "9 ORE => 2 A\n\
             8 ORE => 3 B\n\
             7 ORE => 5 C\n\
             3 A, 4 B => 1 AB\n\
             5 B, 7 C => 1 BC\n\
             4 C, 1 A => 1 CA\n\
             2 AB, 3 BC, 4 CA => 1 FUEL\n",
        )
    }

    fn ex_input_3() -> String {
        String::from(
            "157 ORE => 5 NZVS\n\
             165 ORE => 6 DCFZ\n\
             44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
             12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
             179 ORE => 7 PSHF\n\
             177 ORE => 5 HKGWZ\n\
             7 DCFZ, 7 PSHF => 2 XJWVT\n\
             165 ORE => 2 GPVTF\n\
             3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT",
        )
    }

    #[test]
    fn test_div_ceil_1() {
        assert_eq!(div_ceil(10, 10), 1);
    }

    #[test]
    fn test_div_ceil_2() {
        assert_eq!(div_ceil(7, 10), 1);
    }

    #[test]
    fn test_div_ceil_3() {
        assert_eq!(div_ceil(11, 10), 2);
    }

    #[test]
    fn test_div_ceil_4() {
        assert_eq!(div_ceil(50, 10), 5);
    }

    #[test]
    fn test_div_ceil_5() {
        assert_eq!(div_ceil(42, 10), 5);
    }

    #[test]
    fn test_production_atomic() {
        let reactions = parse_str(&ex_input());
        let factory = Factory::new(reactions);
        let r = factory.find_reaction("A").unwrap();
        let production = factory.produce_reaction(r);

        assert_eq!(
            production,
            Production {
                consumed: vec![(10, "ORE".to_string())],
                produced: vec![(10, "A".to_string())],
            }
        )
    }

    #[test]
    fn test_production_aoc_ex1() {
        let reactions = parse_str(&ex_input());
        let factory = Factory::new(reactions);
        let r = factory.find_reaction("FUEL").unwrap();
        let production = factory.produce_reaction(r);

        let consumed_ore = production
            .consumed
            .iter()
            .find(|(_amt, mat)| mat == "ORE")
            .unwrap();
        assert_eq!(consumed_ore.0, 31);
    }

    #[test]
    fn test_production_aoc_ex2() {
        let reactions = parse_str(&ex_input_2());
        let factory = Factory::new(reactions);
        let r = factory.find_reaction("FUEL").unwrap();
        let production = factory.produce_reaction(r);

        let consumed_ore = production
            .consumed
            .iter()
            .find(|(_amt, mat)| mat == "ORE")
            .unwrap();
        assert_eq!(consumed_ore.0, 165);
    }

    #[test]
    fn test_production_aoc_ex3() {
        let reactions = parse_str(&ex_input_3());
        let factory = Factory::new(reactions);
        let r = factory.find_reaction("FUEL").unwrap();
        let production = factory.produce_reaction(r);

        let consumed_ore = production
            .consumed
            .iter()
            .find(|(_amt, mat)| mat == "ORE")
            .unwrap();
        assert_eq!(consumed_ore.0, 13312);
    }

    #[test]
    fn test_p2_aoc_ex3() {
        let reactions = parse_str(&ex_input_3());
        let factory = Factory::new(reactions);
        let possible_fuel = factory.fuel_from_ore(ONE_TRILLION);
        assert_eq!(possible_fuel, 82892753);
    }
}
