use intcode::machine::Machine;
use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;
use intcode::num_traits::cast::ToPrimitive;
use intcode::num_traits::One;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::ops::Range;

// for coordinate system: x goes left -> right, y goes top -> bottom (it makes display easier)
pub type Point = (i64, i64);

fn distance(p1: Point, p2: Point) -> i64 {
    (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs()
}

#[derive(Clone, Copy, Debug)]
pub enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    pub fn to_input(&self) -> BigInt {
        match self {
            Self::North => One::one(),
            Self::South => BigInt::from_i64(2).unwrap(),
            Self::West => BigInt::from_i64(3).unwrap(),
            Self::East => BigInt::from_i64(4).unwrap(),
        }
    }

    pub fn next_point(&self, from: Point) -> Point {
        match self {
            Self::North => (from.0, from.1 - 1),
            Self::South => (from.0, from.1 + 1),
            Self::West => (from.0 - 1, from.1),
            Self::East => (from.0 + 1, from.1),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Map {
    walls: HashSet<Point>,        // points that are a wall
    pub interior: HashSet<Point>, // points droid can access - includes current droid pos & oxy point (if known)
    pub droid: Point,             // current droid pos
    pub oxy: Option<Point>,       // the discovered position of the oxygen system
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut map_str = String::new();
        let (x_range, y_range) = self.bounds();

        // println!("DEBUG Display::fmt - bounds are x={:?} y={:?} for map {:?}", x_range, y_range, self);
        // go from min Y to max Y, build each row
        for y in y_range.clone() {
            let mut line = String::new();

            for x in x_range.clone() {
                if self.walls.contains(&(x, y)) {
                    line.push('#');
                } else if self.oxy == Some((x, y)) {
                    line.push('O');
                } else if self.droid == (x, y) {
                    line.push('D');
                } else if (x, y) == (0, 0) {
                    line.push('x'); // mark the starting point for visual examination
                } else if self.interior.contains(&(x, y)) {
                    line.push('.');
                } else {
                    line.push(' ');
                }
            }
            line.push('\n');
            map_str.push_str(&line);
        }

        write!(f, "{}", map_str)
    }
}

impl Map {
    pub fn bounds(&self) -> (Range<i64>, Range<i64>) {
        let mut min_x = i64::MAX;
        let mut max_x = i64::MIN;
        let mut min_y = i64::MAX;
        let mut max_y = i64::MIN;

        self.walls.union(&self.interior).for_each(|(x, y)| {
            if *x < min_x {
                min_x = *x
            }
            if *x > max_x {
                max_x = *x
            }
            if *y < min_y {
                min_y = *y
            }
            if *y > max_y {
                max_y = *y
            }
        });

        (min_x..(max_x + 1), min_y..(max_y + 1))
    }

    fn known_point(&self, point: Point) -> bool {
        self.walls.contains(&point) || self.interior.contains(&point)
    }

    // directions we can try (either because the next pos is interior or unknown)
    pub fn available_steps(&self) -> Vec<Direction> {
        let mut v: Vec<Direction> = Vec::new();

        let p = Direction::North.next_point(self.droid);
        if !self.walls.contains(&p) {
            v.push(Direction::North);
        }

        let p = Direction::South.next_point(self.droid);
        if !self.walls.contains(&p) {
            v.push(Direction::South);
        }

        let p = Direction::West.next_point(self.droid);
        if !self.walls.contains(&p) {
            v.push(Direction::West);
        }

        let p = Direction::East.next_point(self.droid);
        if !self.walls.contains(&p) {
            v.push(Direction::East);
        }

        v
    }
}

fn unexplored_dirs(map: &Map) -> Vec<Direction> {
    map.available_steps()
        .iter()
        .filter(|d| !map.known_point(d.next_point(map.droid)))
        .map(|d| d.clone())
        .collect()
}

pub fn discover_map(machine: &mut Machine) -> Map {
    let mut map = Map {
        walls: HashSet::new(),
        interior: [(0, 0)].iter().cloned().collect(),
        droid: (0, 0),
        oxy: None,
    };

    walk_bfs(machine, &mut map, Direction::North);

    if map.oxy.is_none() {
        panic!("we missed the oxygen, but should have seen everything");
    }

    map
}

fn walk_bfs(machine: &mut Machine, map: &mut Map, current_heading: Direction) {
    let mut output_idx: usize = machine.outputs.len();

    machine.push_input(current_heading.to_input());

    while !machine.is_waiting_for_input() {
        if machine.is_exited() {
            panic!("machine shouldn't have exited!");
        }

        machine.step();

        // read output
        while output_idx < machine.outputs.len() {
            match machine.outputs.get(output_idx).unwrap().to_i64().unwrap() {
                0 => {
                    // hit a wall - add wall point & turn heading
                    map.walls.insert(current_heading.next_point(map.droid));
                }
                1 => {
                    // changed pos - change droid & add interior point
                    map.droid = current_heading.next_point(map.droid);
                    map.interior.insert(map.droid);
                }
                2 => {
                    // changed pos & found oxygen - set all of those
                    map.droid = current_heading.next_point(map.droid);
                    map.interior.insert(map.droid);
                    map.oxy = Some(map.droid);
                }
                _ => panic!("invalid output"),
            }
            output_idx += 1;

            // println!("------DEBUG-------\nhead={:?} droid={:?}\nmap={:?}\n{}\n----------END DEBUG---------\n\n", current_heading, map.droid, map, map);
        }
    }

    if machine.is_waiting_for_input() {
        // to do BFS, we fork the machine for each possible direction we can take
        // (that we haven't already tried to visit). After each descent, we reset the map's
        // droid point to what it was at the time we forked.
        // this whole thing is awkard - I'm paying for some modeling mistakes I don't want to
        // spend the time to fix.
        let dirs = unexplored_dirs(map);
        let checkpoint_droid = map.droid;

        if dirs.is_empty() {
            return;
        }

        dirs.iter().for_each(|d| {
            let mut machine_forked = machine.clone();
            walk_bfs(&mut machine_forked, map, *d);
            map.droid = checkpoint_droid.clone();
        });
    }
}

pub fn a_star(starting_map: Map) -> Vec<Direction> {
    let mut open_set: Vec<Map> = Vec::new();
    open_set.push(starting_map.clone());

    // g score of a map state is len of steps to get there from origin.
    // to aid inspection, we store the actual directions, not just a count
    // that way we don't really need came_from
    // since we're only changing the droid point of each map, we use that as the key so I don't
    // need to implement Hash myself
    let mut g_score: HashMap<Point, Vec<Direction>> = HashMap::new();
    g_score.insert(starting_map.droid.clone(), Vec::new());

    // f score of a map state is dist(map.droid, map.oxy)

    while open_set.len() > 0 {
        // keep the states sorted by f score, then get the first one
        open_set.sort_by_key(|m| distance(m.droid, m.oxy.unwrap()));
        let current = open_set.remove(0);

        if current.droid == current.oxy.unwrap() {
            // this is the goal!
            return g_score[&current.droid].clone();
        }

        let steps_to_current = g_score.get(&current.droid).unwrap().clone();

        current.available_steps().iter().for_each(|d| {
            let mut neighbor = current.clone();
            neighbor.droid = d.next_point(current.droid);
            let mut g_for_neighbor = steps_to_current.clone();
            g_for_neighbor.push(d.clone());

            let neighbor_in_open_set = open_set.iter().any(|m| m.droid == neighbor.droid);
            if g_score.contains_key(&neighbor.droid) {
                let old_neighor_g_score = &g_score[&neighbor.droid];
                // if we found a better way to get here, overwrite g & consider adding to open set
                // if we saw this state before & this isn't a better way of getting there, don't
                // consider adding to open set again. This prevents walking in circles or pacing
                if g_for_neighbor.len() < old_neighor_g_score.len() {
                    g_score.insert(neighbor.droid, g_for_neighbor);
                    if !neighbor_in_open_set {
                        open_set.push(neighbor);
                    }
                }
            } else {
                // we haven't see this state yet. insert the g score
                g_score.insert(neighbor.droid, g_for_neighbor);
                if !neighbor_in_open_set {
                    open_set.push(neighbor);
                }
            }
        });
    }

    panic!("A* failed");
}
