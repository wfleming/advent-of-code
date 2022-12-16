use std::cmp::Ordering;
use std::env::args;
use std::collections::{HashMap, HashSet, BinaryHeap};
use std::fs;
use std::rc::Rc;
use std::hash::{Hash, Hasher};
use std::fmt;

const TIME_LIMIT: u32 = 30;

#[derive(Debug, Default, PartialEq, Eq)]
struct Graph {
    neighbors: HashMap<String, Vec<String>>,
    openeable_valves: HashSet<String>,
    pressure_rates: HashMap<String, u32>,
    walk_lengths: HashMap<String, Vec<(String, u32)>>, // how long to walk from key -> val.0
                                                          // without opening anything
}

impl Graph {
    fn parse(input: &str) -> Self {
        let mut g: Graph = Default::default();

        for line in input.lines() {
            let name_idx0 = line.find(' ').unwrap() + 1;
            let name = line[name_idx0..name_idx0 + 2].to_string();

            let rate_idx0 = line.find("rate=").unwrap() + 5;
            let rate_idx1 = rate_idx0 + line[rate_idx0..].find(';').unwrap();
            let rate = line[rate_idx0..rate_idx1].parse::<u32>().unwrap();

            g.pressure_rates.insert(name.clone(), rate);
            if rate > 0 {
                g.openeable_valves.insert(name.clone());
            }
            match line.find("lead to valves ") {
                Some(idx) => {
                    let valves_idx = idx + 15;
                    let valves: Vec<String> = line[valves_idx..].split(", ").map(|x| x.to_string()).collect();
                    g.neighbors.insert(name, valves);
                },
                None => {
                    let valve_idx = line.find("leads to valve ").unwrap() + 15;
                    let valves = vec![line[valve_idx..].to_string()];
                    g.neighbors.insert(name, valves);
                }
            }
        }

        g.populate_walk_lengths();
        g
    }

    fn populate_walk_lengths(&mut self) {
        for (valve, neighbors) in self.neighbors.iter() {
            let mut all_conns: HashMap<String, u32> = HashMap::new();
            let mut queue: Vec<(String, u32)> = neighbors.iter().map(|n| (n.clone(), 1)).collect();

            while let Some((v, steps)) = queue.pop() {
                if &v != valve && steps < *all_conns.get(&v).unwrap_or(&u32::MAX) {
                    all_conns.insert(v.clone(), steps);

                    for next_v in &self.neighbors[&v] {
                        queue.push((next_v.clone(), steps + 1));
                    }
                }
            }

            self.walk_lengths.insert(valve.clone(), all_conns.iter().map(|(v,s)| (v.clone(), *s)).collect());
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct SearchState {
    graph: Rc<Graph>,
    cur_valve: String,
    valves_open: Vec<String>, // HashSet would have made sense, but is not itself hashable, so this
                              // is a bit simpler
    pressure_released: u32,   // so far
    tot_time: u32,
}

impl Hash for SearchState {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.cur_valve.hash(state);
        self.valves_open.hash(state);
        self.pressure_released.hash(state);
        self.tot_time.hash(state);
    }
}

impl fmt::Display for SearchState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<cur={} open={:?} pressure_released={} tot_time={}>", self.cur_valve, self.valves_open, self.pressure_released, self.tot_time)
    }
}

// // impl Ord for SearchState {
// //     fn cmp(&self, other: &Self) -> Ordering {
// //         self.pressure_released_by_end.cmp(&other.pressure_released_by_end)
// //     }
// // }

// // impl PartialOrd for SearchState {
// //     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
// //         Some(self.cmp(other))
// //     }
// // }

impl SearchState {
    fn start_state(v: String, g: Rc<Graph>) -> Self {
        Self { graph: g, cur_valve: v.to_string(), valves_open: Vec::new(), pressure_released: 0, tot_time: 0 }
    }

    fn pressure_per_min(&self) -> u32 {
        self.valves_open.iter().fold(0, |accum, v| accum + self.graph.pressure_rates[v])
    }

    fn is_terminal(&self) -> bool {
        self.tot_time == TIME_LIMIT || self.valves_open.len() == self.graph.openeable_valves.len()
    }

    fn terminal_pressure_released(&self) -> u32 {
        let time_left = TIME_LIMIT - self.tot_time;
        self.pressure_released + (self.pressure_per_min() * time_left)
    }

    // return vec of (state, time_to_reach_state)
    // we only care about states where we actually will open the valve
    fn next_states(&self) -> Vec<(SearchState, u32)> {
        let pressure_per_min = self.pressure_per_min();
        self.graph.walk_lengths[&self.cur_valve].iter().filter_map(|(next_valve, steps_taken)| {
            let can_open = self.graph.pressure_rates[next_valve] > 0 && !self.valves_open.contains(&next_valve);
            let next_tot_time = self.tot_time + steps_taken + 1;

            if can_open && next_tot_time <= TIME_LIMIT {
                let next_pressure_released = self.pressure_released + (pressure_per_min * (steps_taken + 1));
                let mut next_valves_open = self.valves_open.clone();
                next_valves_open.push(next_valve.clone());

                Some((SearchState { graph: self.graph.clone(), cur_valve: next_valve.clone(), valves_open: next_valves_open, pressure_released: next_pressure_released, tot_time: next_tot_time }, steps_taken + 1))
            } else {
                None
            }
        }).collect()
    }
}

fn debug_path(p: &Vec<SearchState>) -> String {
    let mut string = String::new();

    for idx in 0..p.len() {
        if idx != 0 && !p[idx - 1].valves_open.contains(&p[idx].cur_valve) && p[idx].valves_open.contains(&p[idx].cur_valve) {
            string.push_str(&format!("[{}]", &p[idx].cur_valve));
        } else {
            string.push_str(&p[idx].cur_valve);
        }

        if idx < p.len() - 1 {
            string.push_str(" -> ");
        }
    }

    string
}

fn find_path_dfs(path: &Vec<SearchState>) -> Vec<SearchState> {
    let next_states = path[path.len() - 1].next_states();
    if path[path.len() - 1].is_terminal() || next_states.is_empty() {
        return path.clone();
    }

    let mut next_paths: Vec<Vec<SearchState>> = next_states.iter().map(|(s, _t)| {
        let mut p = path.clone();
        p.push(s.clone());
        find_path_dfs(&p)
    }).collect();
    next_paths.sort_by_key(|p| p[p.len() - 1].terminal_pressure_released());
    next_paths.reverse();
    next_paths[0].clone()
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let graph = Graph::parse(&input);

    let path = find_path_dfs(&vec![SearchState::start_state("AA".to_string(), Rc::new(graph))]);
    println!("p1: best path: {}, terminal pressure released {}", debug_path(&path), path[path.len() - 1].terminal_pressure_released());
}
