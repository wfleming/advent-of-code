use std::cmp::Ordering;
use std::collections::{BTreeMap, BinaryHeap};
use std::env::args;
use std::fs;

type Pos = (usize, usize); // x, y

struct Map(Vec<Vec<char>>);

impl Map {
    pub fn parse(input: &str) -> (Map, Pos, Pos) {
        let mut start_pos = (0, 0);
        let mut goal_pos = (0, 0);

        let map = input
            .lines()
            .enumerate()
            .map(|(y, l)| {
                l.chars()
                    .enumerate()
                    .map(|(x, c)| match c {
                        'S' => {
                            start_pos = (x, y);
                            'a'
                        }
                        'E' => {
                            goal_pos = (x, y);
                            'z'
                        }
                        _ => c,
                    })
                    .collect::<Vec<char>>()
            })
            .collect::<Vec<Vec<char>>>();

        (Map(map), start_pos, goal_pos)
    }

    pub fn neighbor_positions(&self, p: &Pos) -> Vec<Pos> {
        let mut n = vec![];

        if p.0 != 0 {
            n.push((p.0 - 1, p.1));
        }
        if p.0 < self.0[0].len() - 1 {
            n.push((p.0 + 1, p.1));
        }
        if p.1 != 0 {
            n.push((p.0, p.1 - 1));
        }
        if p.1 < self.0.len() - 1 {
            n.push((p.0, p.1 + 1));
        }

        n
    }

    pub fn at(&self, p: &Pos) -> char {
        self.0[p.1][p.0]
    }

    pub fn can_move(&self, from: &Pos, to: &Pos) -> bool {
        let f_ord = self.at(from) as u8;
        let t_ord = self.at(to) as u8;

        f_ord >= t_ord || t_ord - f_ord <= 1
    }
}

#[derive(PartialEq, Eq)]
struct HeapWrapper<T: Eq + PartialEq> {
    pub pt: T,
    pub cost: u16,
}

impl<T: Eq> PartialOrd for HeapWrapper<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Eq> Ord for HeapWrapper<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

fn djikstra(map: &Map, start: Pos, goal: Pos) -> Option<u16> {
    let mut dist: BTreeMap<Pos, u16> = BTreeMap::new();
    let mut prev: BTreeMap<Pos, Pos> = BTreeMap::new();
    let mut queue: BinaryHeap<HeapWrapper<Pos>> = BinaryHeap::new();

    dist.insert(start, 0);
    queue.push(HeapWrapper { pt: start, cost: 0 });

    while !queue.is_empty() {
        let p = queue.pop().unwrap().pt;

        for p2 in map
            .neighbor_positions(&p)
            .iter()
            .filter(|n| map.can_move(&p, n))
        {
            let d = dist[&p] + 1;

            if p2 == &goal {
                return Some(d);
            }

            if !dist.contains_key(p2) || d < dist[p2] {
                dist.insert(*p2, d);
                prev.insert(*p2, p);
                queue.push(HeapWrapper { pt: *p2, cost: d });
            }
        }
    }

    None
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let (map, start_pos, goal_pos) = Map::parse(&input);

    let p1_steps = djikstra(&map, start_pos, goal_pos).expect("found goal");
    println!("p1: can reach goal in {} steps", p1_steps);

    let potential_hike_starts = map
        .0
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .filter(|(_x, c)| **c == 'a')
                .map(|(x, _c)| (x, y))
                .collect::<Vec<Pos>>()
        })
        .collect::<Vec<Pos>>();
    let shortest_hike_length = potential_hike_starts
        .iter()
        .filter_map(|p| djikstra(&map, *p, goal_pos))
        .min()
        .expect("There are at least some hikes");
    println!("p2: shortest hike is {} steps", shortest_hike_length);
}
