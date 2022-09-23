use std::env::args;
use std::fs;
use std::collections::BTreeSet;

type State = Vec<Vec<char>>;

fn parse(input: &str) -> State {
    input.lines().map(|l| l.trim().chars().collect()).filter(|l: &Vec<char>| l.len() > 0).collect()
}

fn biodiversity(state: &State) -> i32 {
    let mut bd = 0;

    for (y, l) in state.iter().enumerate() {
        for (x, c) in l.iter().enumerate() {
            let i = (y * 5) + x;
            if *c == '#' {
                bd += (2 as i32).pow(i as u32)
            }
        }
    }

    bd
}

fn step_state(state: &State) -> State {
    state.iter().enumerate().map(|(y, l)| {
        l.iter().enumerate().map(|(x, c)| {
            let up = if y > 0 { state[y - 1][x] } else { '.' };
            let down = if y < state.len() - 1 { state[y + 1][x] } else { '.' };
            let left = if x > 0 { l[x - 1] } else { '.' };
            let right = if x < l.len() - 1 { l[x + 1] } else { '.' };
            let nearby_bugs = vec![up, down, left, right].iter().filter(|c| **c == '#').count();

            if *c == '#' {
                if nearby_bugs == 1 { *c } else { '.' }
            } else if *c == '.' {
                if nearby_bugs >= 1 && nearby_bugs <= 2 { '#' } else { *c }
            } else {
                panic!("Unexpected char in state")
            }
        }).collect()
    }).collect()
}

fn p1(state0: &State) -> i32 {
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

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let state0 = parse(&fs::read_to_string(input_path).expect("File not readable"));

    let p1_ans = p1(&state0);
    println!("p1: {}", p1_ans);
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
        let bd = biodiversity(&parse(i));
        assert_eq!(bd, 2129920);
    }

    #[test]
    fn test_sample_p1() {
        let i = "
....#
#..#.
#..##
..#..
#....";
        let bd = p1(&parse(i));
        assert_eq!(bd, 2129920);
    }
}

