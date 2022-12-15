use std::cmp::min;
use std::collections::BTreeMap;
use std::env::args;
use std::fmt;
use std::fs;
use std::iter;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Pt {
    x: i32,
    y: i32,
}

impl Pt {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x: x, y: y }
    }

    pub fn dist(&self, other: &Self) -> i32 {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Thing {
    Sensor(Pt), // the contained pt is the closest beacon, not the pt the sensor itself is at
    Beacon,
    NoBeacon,
    Unknown,
}

#[derive(Clone)]
struct Cave {
    data: BTreeMap<Pt, Thing>,
    sensors: Vec<(Pt, i32)>, // sensors & their range
    x_min: i32,
    x_max: i32,
    y_min: i32,
    y_max: i32,
}

impl Cave {
    pub fn new() -> Cave {
        Cave {
            data: BTreeMap::new(),
            sensors: Vec::new(),
            x_min: 0,
            x_max: 0,
            y_min: 0,
            y_max: 0,
        }
    }

    pub fn parse(input: &str) -> Cave {
        let mut cave: Cave = Cave::new();

        for line in input.lines() {
            let sx0 = line.find("x=").unwrap() + 2;
            let sx1 = sx0 + line[sx0..].find(',').unwrap();
            let sx = line[sx0..sx1].parse::<i32>().unwrap();
            let sy0 = sx1 + line[sx1..].find("y=").unwrap() + 2;
            let sy1 = sy0 + line[sy0..].find(':').unwrap();
            let sy = line[sy0..sy1].parse::<i32>().unwrap();

            let bx0 = sy1 + line[sy1..].find("x=").unwrap() + 2;
            let bx1 = bx0 + line[bx0..].find(',').unwrap();
            let bx = line[bx0..bx1].parse::<i32>().unwrap();
            let by0 = bx1 + line[bx1..].find("y=").unwrap() + 2;
            let by = line[by0..].parse::<i32>().unwrap();

            let sensor_pt = Pt::new(sx, sy);
            let beacon_pt = Pt::new(bx, by);
            cave.set(&sensor_pt, Thing::Sensor(beacon_pt));
            cave.set(&beacon_pt, Thing::Beacon);
            cave.sensors.push((sensor_pt, sensor_pt.dist(&beacon_pt)));
        }

        cave
    }

    pub fn get(&self, pt: &Pt) -> &Thing {
        self.data.get(pt).unwrap_or(&Thing::Unknown)
    }

    pub fn set(&mut self, pt: &Pt, thing: Thing) {
        if pt.x < self.x_min {
            self.x_min = pt.x;
        }
        if pt.x > self.x_max {
            self.x_max = pt.x;
        }
        if pt.y < self.y_min {
            self.y_min = pt.y;
        }
        if pt.y > self.y_max {
            self.y_max = pt.y;
        }

        self.data.insert(*pt, thing);
    }

    fn iter(&self) -> CaveIter {
        CaveIter {
            cave: self,
            x: self.x_min,
            y: self.y_min,
        }
    }
}

impl fmt::Display for Cave {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = String::new();

        s.push_str(&format!(
            "from ({}, {}) to ({}, {})\n",
            self.x_min, self.y_min, self.x_max, self.y_max
        ));
        for (pt, m) in self.iter() {
            match m {
                Thing::Sensor(_) => s.push('S'),
                Thing::Beacon => s.push('B'),
                Thing::NoBeacon => s.push('x'),
                Thing::Unknown => s.push('.'),
            }
            if pt.x == self.x_max {
                s.push('\n');
            }
        }

        write!(f, "{}", s)
    }
}

struct CaveIter<'a> {
    cave: &'a Cave,
    x: i32,
    y: i32,
}

impl<'a> iter::Iterator for CaveIter<'a> {
    type Item = (Pt, Thing);

    fn next(&mut self) -> Option<Self::Item> {
        if self.y > self.cave.y_max {
            return None;
        }

        let pt = Pt::new(self.x, self.y);
        let t = self.cave.get(&pt);

        if self.x == self.cave.x_max {
            self.x = self.cave.x_min;
            self.y += 1;
        } else {
            self.x += 1;
        }

        Some((pt, *t))
    }
}

fn blocked_pts_in_row(cave: &mut Cave, y_target: i32) -> i32 {
    let intersecting_sensor_pts: Vec<(Pt, i32)> = cave
        .sensors
        .iter()
        .filter_map(|(pt, range)| {
            if *range >= (pt.y - y_target).abs() {
                Some((*pt, *range))
            } else {
                None
            }
        })
        .collect();

    intersecting_sensor_pts
        .iter()
        .fold(0, |acc, (s_pt, range)| {
            let y_delta = (s_pt.y - y_target).abs();
            let x_range = range - y_delta;

            acc + ((s_pt.x - x_range)..=(s_pt.x + x_range)).fold(0, |row_acc, x| {
                match cave.get(&Pt::new(x, y_target)) {
                    Thing::Unknown => {
                        cave.set(&Pt::new(x, y_target), Thing::NoBeacon);
                        row_acc + 1
                    }
                    _ => row_acc,
                }
            })
        })
}

fn p1(cave: &mut Cave, y_target: i32) {
    let blocked_pts = blocked_pts_in_row(cave, y_target);
    println!(
        "p1: at y == {}, {} positions cannot contain a beacon",
        y_target, blocked_pts
    );
}

fn p2(cave: &mut Cave, max_xy: i32) {
    let x_max = min(cave.x_max, max_xy);
    let y_max = min(cave.y_max, max_xy);

    for x in 0..=x_max {
        for y in 0..=y_max {
            let pt = Pt::new(x, y);
            match cave.get(&pt) {
                Thing::Unknown => {
                    let blocked = cave
                        .sensors
                        .iter()
                        .any(|(s_pt, range)| range >= &s_pt.dist(&pt));
                    if !blocked {
                        println!("p2: found unique position at {:?}", &pt);
                        println!("p2: tuning frequency is {}", (pt.x * 4000000) + pt.y);
                        return;
                    }
                }
                _ => continue,
            }
        }
    }
    panic!("p2: did not find a unique possible position");
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let p1_y = args
        .next()
        .expect("provide p1 y val")
        .parse::<i32>()
        .expect("should be a number");
    let mut cave = Cave::parse(&input);

    p1(&mut cave, p1_y);

    let p2_max = p1_y * 2;
    p2(&mut cave, p2_max);
}
