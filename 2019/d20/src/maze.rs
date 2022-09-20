use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::BinaryHeap;
use std::fs;

type Point = (i32, i32);

pub struct Maze {
    pub cells: Vec<Vec<char>>,
    pub edges: BTreeMap<Point, BTreeSet<Point>>,
    pub entrance: Point,
    pub exit: Point,
}

const DELTAS: &'static [Point] = &[(-1, 0), (1, 0), (0, -1), (0, 1)];

impl Maze {
    pub fn from_file(path: &str) -> Maze {
        let contents = fs::read_to_string(path).expect("File not readable");

        Maze::from_str(&contents)
    }

    pub fn from_str(input: &str) -> Maze {
        let mut lines: Vec<Vec<char>> = input.lines().map(|l| l.chars().collect()).collect();
        lines.reverse();

        let mut maze = Maze {
            cells: lines,
            edges: BTreeMap::new(),
            entrance: (0, 0),
            exit: (0, 0),
        };

        for (y, line) in maze.cells.iter().enumerate() {
            for (x, c) in line.iter().enumerate() {
                if *c != '.' {
                    continue;
                } // jump ahead if this isn't a floor

                let pt = (x as i32, y as i32);
                let neighbors: BTreeSet<Point> = DELTAS
                    .iter()
                    .filter_map(|d| maze.resolve_neighbor(&pt, d))
                    .collect();
                maze.edges.insert(pt, neighbors);

                // check for entrance/exit
                for d in DELTAS {
                    let n1 = (x as i32 + d.0, y as i32 + d.1);
                    let n2 = (n1.0 + d.0, n1.1 + d.1);

                    if maze.cell_at(&n1) == 'A' && maze.cell_at(&n2) == 'A' {
                        maze.entrance = (x as i32, y as i32);
                    } else if maze.cell_at(&n1) == 'Z' && maze.cell_at(&n2) == 'Z' {
                        maze.exit = (x as i32, y as i32);
                    }
                }
            }
        }

        maze
    }

    pub fn cell_at(&self, pos: &(i32, i32)) -> char {
        if pos.0 < 0 || pos.1 < 0 {
            return '#';
        } else if pos.1 >= self.cells.len() as i32
            || pos.0 >= self.cells[pos.1 as usize].len() as i32
        {
            return '#';
        }

        self.cells[pos.1 as usize][pos.0 as usize]
    }

    fn find_portal_exit(&self, p1: char, p2: char, ignore: &Point) -> Point {
        for (y, line) in self.cells.iter().enumerate() {
            for (x, _c) in line.iter().enumerate() {
                let pt = (x as i32, y as i32);
                if pt == *ignore || self.cell_at(&pt) != '.' {
                    continue;
                }

                // portal names are always up-down or left-right: the chars aren't directional
                // labels. I.e. `.AB` doesn't mean the other side is `.BA`. However, I'm pretty
                // sure no input has conflicting mirrored names (if there's an XY there's no YX),
                // and just checking both is easier than tracking all the directions, so I'm doing
                // that.

                for d in DELTAS {
                    let n1 = (pt.0 + d.0, pt.1 + d.1);
                    let n2 = (n1.0 + d.0, n1.1 + d.1);
                    if (self.cell_at(&n1) == p1 && self.cell_at(&n2) == p2)
                        || (self.cell_at(&n1) == p2 && self.cell_at(&n2) == p1)
                    {
                        return pt;
                    }
                }
            }
        }

        panic!("Didn't find portal exit");
    }

    fn resolve_neighbor(&self, pos: &Point, delta: &(i32, i32)) -> Option<Point> {
        let n = (pos.0 + delta.0, pos.1 + delta.1);
        let c = self.cell_at(&n);
        if c == '.' {
            return Some((pos.0 + delta.0, pos.1 + delta.1));
        } else if c.is_alphabetic() {
            let portal_out = self.cell_at(&(n.0 + delta.0, n.1 + delta.1));
            if (c == 'A' || c == 'Z') && c == portal_out {
                // nop: entrance and exit are identified separately in from_str
                return None;
            }

            return Some(self.find_portal_exit(c, portal_out, &pos));
        }

        None
    }
}

#[derive(Eq)]
struct HeapWrapper {
    pub pt: Point,
    pub cost: i32,
}

impl PartialEq for HeapWrapper {
    fn eq(&self, other: &Self) -> bool {
        self.pt == other.pt
    }
}

impl PartialOrd for HeapWrapper {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for HeapWrapper {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

pub fn find_path(maze: &Maze) -> Vec<Point> {
    let mut dist: BTreeMap<Point, i32> = BTreeMap::new();
    let mut prev: BTreeMap<Point, Point> = BTreeMap::new();
    let mut queue: BinaryHeap<HeapWrapper> = BinaryHeap::new();

    dist.insert(maze.entrance, 0);
    queue.push(HeapWrapper {
        pt: maze.entrance,
        cost: 0,
    });

    while !queue.is_empty() {
        let p = queue.pop().expect("queue is not empty").pt;

        if p == maze.exit {
            let mut path = vec![p];
            while path[0] != maze.entrance {
                path.insert(0, prev[&path[0]]);
            }
            return path;
        }

        for n in &maze.edges[&p] {
            let d = dist[&p] + 1;
            if !dist.contains_key(&n) || d < dist[&n] {
                dist.insert(*n, d);
                prev.insert(*n, p);
                queue.push(HeapWrapper { pt: *n, cost: d });
            }
        }
    }

    panic!("Failed to find path");
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_INPUT1: &str = "         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z";

    const SAMPLE_INPUT2: &str = "                   A
                   A
  #################.#############
  #.#...#...................#.#.#
  #.#.#.###.###.###.#########.#.#
  #.#.#.......#...#.....#.#.#...#
  #.#########.###.#####.#.#.###.#
  #.............#.#.....#.......#
  ###.###########.###.#####.#.#.#
  #.....#        A   C    #.#.#.#
  #######        S   P    #####.#
  #.#...#                 #......VT
  #.#.#.#                 #.#####
  #...#.#               YN....#.#
  #.###.#                 #####.#
DI....#.#                 #.....#
  #####.#                 #.###.#
ZZ......#               QG....#..AS
  ###.###                 #######
JO..#.#.#                 #.....#
  #.#.#.#                 ###.#.#
  #...#..DI             BU....#..LF
  #####.#                 #.#####
YN......#               VT..#....QG
  #.###.#                 #.###.#
  #.#...#                 #.....#
  ###.###    J L     J    #.#.###
  #.....#    O F     P    #.#...#
  #.###.#####.#.#####.#####.###.#
  #...#.#.#...#.....#.....#.#...#
  #.#####.###.###.#.#.#########.#
  #...#.#.....#...#.#.#.#.....#.#
  #.###.#####.###.###.#.#.#######
  #.#.........#...#.............#
  #########.###.###.#############
           B   J   C
           U   P   P";

    #[test]
    fn test_parse1() {
        let m = Maze::from_str(SAMPLE_INPUT1);
        assert_eq!(m.entrance, (9, 16));
        assert_eq!(m.exit, (13, 2));

        let x2y3_edges = m.edges.get(&(2, 3)).expect("should exist");
        assert!(x2y3_edges.contains(&(3, 3)));
    }

    #[test]
    fn test_parse2() {
        let m = Maze::from_str(SAMPLE_INPUT2);

        // check YN portal
        let x2y13_edges = m.edges.get(&(2, 13)).expect("should exist");
        assert!(x2y13_edges.contains(&(26, 23)));
        let x26y23_edges = m.edges.get(&(26, 23)).expect("should exist");
        assert!(x26y23_edges.contains(&(2, 13)));

        // check JO portal
        let x2y17_edges = m.edges.get(&(2, 17)).expect("should exist");
        assert!(x2y17_edges.contains(&(13, 8)));
        let x13y8_edges = m.edges.get(&(13, 8)).expect("should exist");
        assert!(x13y8_edges.contains(&(2, 17)));
    }

    #[test]
    fn test_find_path1() {
        let m = Maze::from_str(SAMPLE_INPUT1);
        let p = find_path(&m);
        assert_eq!(p.len(), 24); // 23 steps. answer is of cells passed through, so len = steps + 1
    }

    #[test]
    fn test_find_path2() {
        let m = Maze::from_str(SAMPLE_INPUT2);
        let p = find_path(&m);
        assert_eq!(p.len(), 59); // 58 steps
    }
}
