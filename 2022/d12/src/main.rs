use std::collections::{BTreeMap, VecDeque};
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

fn djikstra(map: &Map, start: Pos) -> BTreeMap<Pos, u16> {
    let mut dist: BTreeMap<Pos, u16> = BTreeMap::new();
    let mut prev: BTreeMap<Pos, Pos> = BTreeMap::new();
    let mut queue: VecDeque<Pos> = VecDeque::new();

    dist.insert(start, 0);
    queue.push_back(start);

    while !queue.is_empty() {
        let p = queue.pop_front().unwrap();

        for p2 in map
            .neighbor_positions(&p)
            .iter()
            .filter(|n| map.can_move(n, &p))
        {
            let d = dist[&p] + 1;

            if !dist.contains_key(p2) || d < dist[p2] {
                dist.insert(*p2, d);
                prev.insert(*p2, p);
                queue.push_back(*p2);
            }
        }
    }

    dist
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let (map, start_pos, goal_pos) = Map::parse(&input);

    let dist = djikstra(&map, goal_pos);
    println!("p1: can reach goal in {} steps", dist[&start_pos]);

    let shortest_hike_length = map
        .0
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter()
                .enumerate()
                .flat_map(|(x,c)| {
                    if *c == 'a' {
                        dist.get(&(x,y))
                    } else {
                        None
                    }
                })
                .collect::<Vec<&u16>>()
        })
        .min()
        .expect("There are at least some hikes");
    println!("p2: shortest hike is {} steps", shortest_hike_length);
}
