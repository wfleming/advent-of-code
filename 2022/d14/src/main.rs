use std::env::args;
use std::collections::VecDeque;
use std::fs;
use std::cmp::{min,max};
use std::fmt;
use std::iter;

type Pt = (usize, usize);

const SAND_ENTRY: Pt = (500, 0);

#[derive(Copy, Clone, Debug, PartialEq)]
enum Mat {
    Rock,
    Sand,
    Air,
}

#[derive(Clone)]
struct Cave {
    data: Vec<VecDeque<Mat>>,
    x_min: usize,
    y_max: usize,
}

impl Cave {
    pub fn new() -> Cave {
        Cave { data: Vec::new(), x_min: 0, y_max: 0 }
    }

    pub fn parse(input: &str) -> Cave {
        let mut cave: Cave = Cave::new();

        for line in input.lines() {
            let mut pts = line.split(" -> ").map(|ps| {
                let mut pieces = ps.split(",").map(|s| s.parse::<usize>().unwrap());
                (pieces.next().unwrap(), pieces.next().unwrap())
            });

            let mut from = pts.next().unwrap();

            while let Some(to) = pts.next() {
                for x in min(from.0, to.0)..=max(from.0, to.0) {
                    for y in min(from.1, to.1)..=max(from.1, to.1) {
                        cave.set(&(x,y), Mat::Rock);
                    }
                }
                from = to;
            }
        }

        cave
    }


    pub fn get(&self, pt: &Pt) -> Mat {
        if !self.in_bounds(pt) {
            return Mat::Air
        }

        self.data[pt.1][pt.0 - self.x_min]
    }

    pub fn set(&mut self, pt: &Pt, mat: Mat) {
        // println!("DEBUG: Cave.set pt={:?} mat={:?} data={:?}", pt, mat, self.data);
        // resize & update if pt out of bounds
        if !self.in_bounds(pt) {
            if self.data.is_empty() {
                self.x_min = pt.0 + 1;
            }

            if pt.1 > self.y_max {
                let w = self.x_max() - self.x_min + 1;
                let mut row = VecDeque::with_capacity(w);
                row.resize(w, Mat::Air);
                for _ in 0..=(pt.1 - self.y_max) {
                    self.data.push(row.clone());
                }
                self.y_max = pt.1
            }

            if pt.0 < self.x_min {
                let n = self.x_min - pt.0;
                for row in &mut self.data {
                    row.resize(row.len() + n, Mat::Air);
                    row.rotate_right(n);
                }
                self.x_min = pt.0;
            } else if pt.0 > self.x_max() {
                let n = pt.0 - self.x_max();
                for row in &mut self.data {
                    row.resize(row.len() + n, Mat::Air);
                }
            }

        }

        // actually set the value
        // println!("    DEBUG: about to really set [{}][{}] in data {:?}", pt.1, pt.0 - self.x_min, self.data);
        self.data[pt.1][pt.0 - self.x_min] = mat;
    }

    fn x_max(&self) -> usize {
        if self.data.is_empty() {
            self.x_min
        } else {
            self.x_min + self.data[0].len() - 1
        }
    }

    fn in_bounds(&self, pt: &Pt) -> bool {
        pt.0 >= self.x_min && pt.0 <= self.x_max() && pt.1 <= self.y_max
    }

    fn iter(&self) -> CaveIter {
        CaveIter { cave: self, x: self.x_min, y: 0 }
    }

    // drop a grain of sand, inserting it into self in it's final resting place.
    // returns the final resting place, or None if it ends up going out of bounds.
    pub fn drop_sand(&mut self) -> Option<Pt> {
        let mut pt = SAND_ENTRY.clone();
        while let pt2 = self.next_sand_pt(&pt) {
            match pt2 {
                None => {
                    self.set(&pt, Mat::Sand);
                    return Some(pt);
                }
                Some(pt2_p) => {
                    if !self.in_bounds(&pt2_p) {
                        return None;
                    } else {
                        pt = pt2_p;
                    }
                }
            }
        }

        None
    }

    fn next_sand_pt(&self, pt: &Pt) -> Option<Pt> {
        let down = (pt.0, pt.1 + 1);
        let left = (pt.0 - 1, pt.1 + 1);
        let right = (pt.0 + 1, pt.1 + 1);

        if self.get(&down) == Mat::Air {
            Some(down)
        } else if self.get(&left) == Mat::Air {
            Some(left)
        } else if self.get(&right) == Mat::Air {
            Some(right)
        } else {
            None
        }
    }
}

impl fmt::Display for Cave {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();

        s.push_str(&format!("from ({}, {}) to ({}, {})\n", self.x_min, 0, self.x_max(), self.y_max));
        for (pt, m) in self.iter() {
            if pt == SAND_ENTRY {
                s.push('+');
            } else {
                match m {
                    Mat::Rock => s.push('#'),
                    Mat::Sand => s.push('o'),
                    Mat::Air => s.push('.'),
                }
            }
            if pt.0 == self.x_max() {
                s.push('\n');
            }
        }

        write!(f, "{}", s)
    }
}

struct CaveIter<'a> {
    cave: &'a Cave,
    x: usize,
    y: usize,
}

impl<'a> iter::Iterator for CaveIter<'a> {
    type Item = (Pt, Mat);

    fn next(&mut self) -> Option<Self::Item> {
        if self.x > self.cave.x_max() || self.y > self.cave.y_max {
            return None
        }

        let pt = (self.x, self.y);
        let m = self.cave.get(&pt);

        if self.x == self.cave.x_max() {
            self.x = self.cave.x_min;
            self.y += 1;
        } else {
            self.x += 1;
        }

        Some((pt, m))
    }
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let mut cave = Cave::parse(&input);

    // for _ in 0..30 { cave.drop_sand(); }
    while cave.drop_sand() != None {}

    println!("{}", &cave);

    let sand_count = cave.iter().fold(0, |acc, (pt, m)| if m == Mat::Sand { acc + 1 } else { acc });
    println!("p1: there are {} units of sand", sand_count);
}
