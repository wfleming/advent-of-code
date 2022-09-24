use std::env::args;
use std::fs;

// coordinate are y top-to-bottom, then x left-to-right
type State = Vec<Vec<bool>>;

fn parse(input: &str) -> State {
    input
        .lines()
        .map(|l| {
            l.trim()
                .chars()
                .map(|c| {
                    if c == '#' {
                        true
                    } else if c == '.' {
                        false
                    } else {
                        panic!("unexpected char in map")
                    }
                })
                .collect()
        })
        .filter(|l: &Vec<bool>| l.len() > 0)
        .collect()
}

// fn disp(s: &State) -> String {
//     s.iter()
//         .map(|l| {
//             l.iter()
//                 .map(|c| if *c { '#' } else { '.' })
//                 .collect::<String>()
//         })
//         .collect::<Vec<String>>()
//         .join("\n")
// }

mod p1 {
    use super::State;
    use std::collections::BTreeSet;

    pub fn biodiversity(state: &State) -> i32 {
        let mut bd = 0;

        for (y, l) in state.iter().enumerate() {
            for (x, c) in l.iter().enumerate() {
                let i = (y * 5) + x;
                if *c {
                    bd += (2 as i32).pow(i as u32)
                }
            }
        }

        bd
    }

    fn step_state(state: &State) -> State {
        state
            .iter()
            .enumerate()
            .map(|(y, l)| {
                l.iter()
                    .enumerate()
                    .map(|(x, c)| {
                        let up = if y > 0 { state[y - 1][x] } else { false };
                        let down = if y < state.len() - 1 {
                            state[y + 1][x]
                        } else {
                            false
                        };
                        let left = if x > 0 { l[x - 1] } else { false };
                        let right = if x < l.len() - 1 { l[x + 1] } else { false };
                        let nearby_bugs = vec![up, down, left, right]
                            .iter()
                            .filter(|c| **c == true)
                            .count();

                        if *c {
                            if nearby_bugs == 1 {
                                *c
                            } else {
                                false
                            }
                        } else {
                            if nearby_bugs >= 1 && nearby_bugs <= 2 {
                                true
                            } else {
                                *c
                            }
                        }
                    })
                    .collect()
            })
            .collect()
    }

    pub fn p1(state0: &State) -> i32 {
        // because biodiversity is unique for a given layout, storing the scores instead of the actual
        // states is probably faster.
        let mut diversities = BTreeSet::new();
        let mut s = state0.clone();

        loop {
            let bd = biodiversity(&s);

            if diversities.contains(&bd) {
                // println!("p1: saw {} unique states before answer", diversities.len());
                return bd;
            }

            diversities.insert(bd);
            s = step_state(&s);
        }
    }
}

mod p2 {
    use super::State;

    // p2 is a stack of levels. to the left is "down", to the right is "up"
    type State2 = Vec<State>;

    const LEVEL_DIM: usize = 5;

    fn empty_level() -> State {
        vec![vec![false; LEVEL_DIM]; LEVEL_DIM]
    }

    fn step_level(lvl_down: &State, lvl_cur: &State, lvl_up: &State) -> State {
        lvl_cur
            .iter()
            .enumerate()
            .map(|(y, l)| {
                l.iter()
                    .enumerate()
                    .map(|(x, c)| {
                        let neighbor_up = if y > 0 { lvl_cur[y - 1][x] } else { false };
                        let neighbor_down = if y < lvl_cur.len() - 1 {
                            lvl_cur[y + 1][x]
                        } else {
                            false
                        };
                        let neighbor_left = if x > 0 { l[x - 1] } else { false };
                        let neighbor_right = if x < l.len() - 1 { l[x + 1] } else { false };
                        let mut neighbors =
                            vec![neighbor_up, neighbor_down, neighbor_left, neighbor_right];
                        // now append the up/down neighbors if appropriate
                        if y == 1 && x == 2 {
                            // get all y=0 down level
                            neighbors.append(&mut lvl_down[0].clone());
                        } else if y == 2 && x == 1 {
                            // get all x=0 down level
                            let mut xs = lvl_down.iter().map(|r| r[0]).collect();
                            neighbors.append(&mut xs);
                        } else if y == 2 && x == 3 {
                            // get all x=4 down level
                            let mut xs = lvl_down.iter().map(|r| r[4]).collect();
                            neighbors.append(&mut xs);
                        } else if y == 3 && x == 2 {
                            // get all y=4 down level
                            neighbors.append(&mut lvl_down[4].clone());
                        }

                        // these aren't else-ifs because they're not exclusive: y=0,x=4 borders
                        // two outer-layer cells because it's at a corner
                        if y == 0 {
                            // get y = 1, x=2 up level
                            neighbors.push(lvl_up[1][2]);
                        }
                        if x == 0 {
                            // get y = 2, x=1 up level
                            neighbors.push(lvl_up[2][1]);
                        }
                        if x == 4 {
                            // get y = 2, x=3 up level
                            neighbors.push(lvl_up[2][3]);
                        }
                        if y == 4 {
                            // get y = 3, x=2 up level
                            neighbors.push(lvl_up[3][2]);
                        }

                        // DEBUG
                        // if x == 4 {
                        //     println!("DEBUG for y = {} x == 4", y);
                        //     println!("lvl_down=\n{}\n\ncur=\n{}\n\nlvl_up=\n{}\n", super::disp(lvl_down), super::disp(lvl_cur), super::disp(lvl_up));
                        //     println!("There are {} neighbors {:?}", neighbors.len(), neighbors);
                        // }
                        //END DEBUG

                        let nearby_bugs = neighbors.iter().filter(|x| **x).count();

                        if y == 2 && x == 2 {
                            // central square is where it recurses, so always treat it as
                            // empty regardless of anything else
                            false
                        } else if *c {
                            if nearby_bugs == 1 {
                                *c
                            } else {
                                false
                            }
                        } else {
                            if nearby_bugs >= 1 && nearby_bugs <= 2 {
                                true
                            } else {
                                *c
                            }
                        }
                    })
                    .collect()
            })
            .collect()
    }

    fn step_state(s: &State2) -> State2 {
        // each iteration we add 1 new level at each end
        let mut new_levels = Vec::new();
        let tmp_empty = empty_level();

        new_levels.push(step_level(&tmp_empty, &empty_level(), &s[0]));

        for (lvl_idx, lvl) in s.iter().enumerate() {
            let down = if lvl_idx > 0 {
                &s[lvl_idx - 1]
            } else {
                &tmp_empty
            };
            let up = if lvl_idx < s.len() - 1 {
                &s[lvl_idx + 1]
            } else {
                &tmp_empty
            };
            new_levels.push(step_level(down, &lvl, up));
        }
        new_levels.push(step_level(
            s.last().expect("s isn't empty"),
            &empty_level(),
            &tmp_empty,
        ));

        new_levels
    }

    pub fn count_bugs(levels: &State2) -> u32 {
        levels
            .iter()
            .map(|l| {
                l.iter()
                    .map(|r| r.iter().filter(|x| **x).count() as u32)
                    .sum::<u32>()
            })
            .sum()
    }

    pub fn p2(s0: &State, iters: u32) -> State2 {
        let mut levels = vec![s0.clone()];

        for _ in 0..iters {
            levels = step_state(&levels);
        }

        levels
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sample_biodiversity() {
        let i = ".....
.....
.....
#....
.#...";
        let bd = p1::biodiversity(&parse(i));
        assert_eq!(bd, 2129920);
    }

    const SAMPLE_START: &str = "
....#
#..#.
#..##
..#..
#....";

    #[test]
    fn test_sample_p1() {
        let bd = p1::p1(&parse(SAMPLE_START));
        assert_eq!(bd, 2129920);
    }

    #[test]
    fn test_sample_p2() {
        let levels = p2::p2(&parse(SAMPLE_START), 10);
        assert_eq!(p2::count_bugs(&levels), 99);
    }
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let state0 = parse(&fs::read_to_string(input_path).expect("File not readable"));

    let p1_ans = p1::p1(&state0);
    println!("p1: {}", p1_ans);

    let p2_ans = p2::count_bugs(&p2::p2(&state0, 200));
    println!("p2: {}", p2_ans);
}
