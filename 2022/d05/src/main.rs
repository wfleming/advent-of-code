use std::env::args;
use std::fs;
use std::io::{self, BufRead};

#[derive(PartialEq, Debug, Clone)]
struct Move {
    source: usize,
    target: usize,
    amount: usize,
}

impl Move {
    pub fn parse(s: &str) -> Move {
        let pieces: Vec<&str> = s.split(" ").collect();
        let a = pieces[1].parse::<usize>().expect("valid number");
        let s = pieces[3].parse::<usize>().expect("valid number");
        let t = pieces[5].parse::<usize>().expect("valid number");

        Move {
            source: s,
            target: t,
            amount: a,
        }
    }
}

#[derive(Clone, Debug)]
struct Stacks(Vec<Vec<char>>);  // stacks are bottom -> top

impl Stacks {
    pub fn apply_move(&mut self, m: &Move) {
        for _ in 1..=m.amount {
            self.move_top_crate(m.source, m.target);
        }
    }

    // source and target are stack "identifiers" (starts at 1), not indexes (starts at 0)
    pub fn move_top_crate(&mut self, source: usize, target: usize) {
        let c = self.0[source - 1].pop().expect("Don't try to move from empty stack");
        self.0[target - 1].push(c);
    }

    pub fn apply_move_2(&mut self, m: &Move) {
        let mut batch = Vec::new();
        for _ in 1..=m.amount {
            batch.push(self.0[m.source - 1].pop().expect("Don't try to move from empty stack"));
        }
        batch.reverse();
        self.0[m.target - 1].append(&mut batch);
    }
}

fn parse_input(input_path: &str) -> (Stacks, Vec<Move>) {
    let f = fs::File::open(input_path)
        .map_err(|e| format!("{}", e))
        .expect("should be able to read file");
    let br = io::BufReader::new(f);

    let mut stacks = Vec::new();
    let mut moves = Vec::new();

    for lr in br.lines() {
        let l = lr.expect("read line");
        if l.contains("[") {
            // part of the stacks
            let stack_count = (l.len() + 1) / 4;
            for stack_idx in 0..stack_count {
                if stacks.len() < stack_idx + 1 {
                    stacks.push(Vec::new());
                }
                let s_idx = 1 + (stack_idx * 4);
                if s_idx < l.len() && l.chars().nth(s_idx).unwrap() != ' ' {
                    stacks[stack_idx].push(l.chars().nth(s_idx).unwrap());
                }
            }
        } else if l.contains("move") {
            // parse a move
            moves.push(Move::parse(&l));
        }
    }

    // stacks were built top-down, but bottom-up will be easier to work with, so we reverse
    // them here
    for s in &mut stacks {
        s.reverse();
    }

    (Stacks(stacks), moves)
}


fn p1(stacks: Stacks, moves: &Vec<Move>) -> String {
    let mut stacks = stacks;
    for m in moves {
        stacks.apply_move(&m);
    }

    stacks.0.iter().map(|s| s.last().expect("no empty stack")).collect()
}

fn p2(stacks: Stacks, moves: &Vec<Move>) -> String {
    let mut stacks = stacks;
    for m in moves {
        stacks.apply_move_2(&m);
    }

    stacks.0.iter().map(|s| s.last().expect("no empty stack")).collect()
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let (stacks, moves) = parse_input(&input_path);

    println!("p1: top crates are {}", p1(stacks.clone(), &moves));
    println!("p2: top crates are {}", p2(stacks.clone(), &moves));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_move() {
        assert_eq!(
            Move::parse("move 3 from 8 to 9"),
            Move {
                source: 8,
                target: 9,
                amount: 3
            }
        );
    }
}
