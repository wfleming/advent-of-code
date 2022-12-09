use std::collections::BTreeSet;
use std::env::args;
use std::fs;
use std::ops::{Add, Sub};

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
    count: u8,
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

        Move {
            dir: dir,
            count: count,
        }
    }

    pub fn parse_all(input: &str) -> Vec<Move> {
        input.lines().map(|l| Self::parse(l)).collect()
    }
}

struct Rope(Vec<Point>); // head is first knot in vec

impl Rope {
    pub fn new(knots: usize) -> Self {
        let knots = (0..knots).map(|_| Point { x: 0, y: 0 }).collect::<Vec<_>>();
        Rope(knots)
    }

    pub fn step(&mut self, dir: &Point) {
        self.0[0] = self.0[0] + *dir;

        for idx in 0..self.0.len() - 1 {
            let mut diff = self.0[idx] - self.0[idx + 1];
            if diff.x.abs() > 1 || diff.y.abs() > 1 {
                if diff.x != 0 {
                    diff.x = diff.x / diff.x.abs();
                }
                if diff.y != 0 {
                    diff.y = diff.y / diff.y.abs();
                }
                self.0[idx + 1] = self.0[idx + 1] + diff;
            }
        }
    }
}

fn count_tail_points(knots: usize, moves: &Vec<Move>) -> usize {
    let mut rope = Rope::new(knots);
    let mut tail_pts: BTreeSet<Point> = BTreeSet::new();

    tail_pts.insert(rope.0[knots - 1]);

    for m in moves {
        for _ in 0..m.count {
            rope.step(&m.dir);
            tail_pts.insert(rope.0[knots - 1]);
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

    println!("p1: tail touches {} points", count_tail_points(2, &moves));
    println!("p2: tail touches {} points", count_tail_points(10, &moves));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_p1_one_move() {
        let ms = Move::parse_all("R 4");
        assert_eq!(count_tail_points(2, &ms), 4);
    }

    #[test]
    fn test_p1_sample() {
        let ms = Move::parse_all(
            "R 4\n\
            U 4\n\
            L 3\n\
            D 1\n\
            R 4\n\
            D 1\n\
            L 5\n\
            R 2",
        );
        assert_eq!(count_tail_points(2, &ms), 13);
    }

    #[test]
    fn test_p2_sample() {
        let ms = Move::parse_all(
            "R 5\n\
            U 8\n\
            L 8\n\
            D 3\n\
            R 17\n\
            D 10\n\
            L 25\n\
            U 20",
        );
        assert_eq!(count_tail_points(10, &ms), 36);
    }
}
