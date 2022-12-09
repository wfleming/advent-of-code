use std::env::args;
use std::fs;
use std::ops::{Add, Sub};
use std::collections::BTreeSet;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Point {
    x: i32,
    y: i32,
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for Point {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

struct Move {
    dir: Point,
    count: u8
}

impl Move {
    pub fn parse(line: &str) -> Move {
        let pieces: Vec<&str> = line.split(" ").collect();
        let count = pieces[1].parse::<u8>().expect("should be a number");
        let dir = match pieces[0] {
            "U" => Point { x: 0, y: 1 },
            "D" => Point { x: 0, y: -1 },
            "L" => Point { x: -1, y: 0 },
            "R" => Point { x: 1, y: 0 },
            _ => panic!("unexpected char in move"),
        };

        Move { dir: dir, count: count }
    }

    pub fn parse_all(input: &str) -> Vec<Move> {
        input.lines().map(|l| Self::parse(l)).collect()
    }
}

struct Rope {
    h: Point,
    t: Point
}

impl Rope {
    pub fn new() -> Self {
        Rope { h: Point { x: 0, y: 0 }, t: Point { x: 0, y: 0 } }
    }

    pub fn apply_move(&mut self, m: &Move) {
        for _ in 0..m.count {
            self.step(&m.dir);
        }
    }

    pub fn step(&mut self, dir: &Point) {
        self.h = self.h + *dir;

        let mut diff = self.h - self.t;
        if diff.x.abs() > 1 || diff.y.abs() > 1 {
            if diff.x != 0 {
                diff.x = diff.x / diff.x.abs();
            }
            if diff.y != 0 {
                diff.y = diff.y / diff.y.abs();
            }
            self.t = self.t + diff;
        }
    }
}

fn p1(moves: &Vec<Move>) -> usize {
    let mut rope = Rope::new();
    let mut tail_pts: BTreeSet<Point> = BTreeSet::new();

    tail_pts.insert(rope.t);

    for m in moves {
        for _ in 0..m.count {
            rope.step(&m.dir);
            tail_pts.insert(rope.t);
        }
    }

    tail_pts.len()
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let input = fs::read_to_string(input_path)
        .expect("read the input")
        .trim()
        .to_string();
    let moves = Move::parse_all(&input);

    println!("p1: tail touches {} points", p1(&moves));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_p1_one_move() {
        let ms = Move::parse_all("R 4");
        assert_eq!(p1(&ms), 4);
    }

    #[test]
    fn test_p1_sample() {
        let ms = Move::parse_all("R 4\n\
            U 4\n\
            L 3\n\
            D 1\n\
            R 4\n\
            D 1\n\
            L 5\n\
            R 2");
        assert_eq!(p1(&ms), 13);
    }
}
