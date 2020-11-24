use intcode::machine::Machine;
use intcode::num_traits::ToPrimitive;
use std::fmt;

pub type Point = (usize, usize);

#[derive(Debug, PartialEq)]
pub enum Heading {
    North,
    South,
    West,
    East,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tile {
    Scaffold,
    Space,
}

// coordinates: (x = 0, y = 0) starts at upper left
// x increases to right, y increases down
#[derive(Debug)]
pub struct View {
    points: Vec<Vec<Tile>>,
    droid: Point,
    droid_heading: Heading,
}

impl fmt::Display for View {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TODO")
    }
}

impl View {
    pub fn read_machine(machine: &mut Machine) -> View {
        machine.run_until_exit_or_input_wait();

        let out_str: String = machine
            .outputs
            .iter()
            .map(|x| {
                let x_u32: u32 = x.to_u32().unwrap();
                std::char::from_u32(x_u32).unwrap()
            })
            .collect();

        println!("DEBUG: scanned view is:\n{}\n", out_str);
        Self::scan(&out_str)
    }

    pub fn scan(s: &str) -> View {
        let str_lines: Vec<&str> = s.lines().collect();
        let mut droid_pos: Option<Point> = None;
        let mut droid_heading: Option<Heading> = None;

        let lines: Vec<Vec<Tile>> = str_lines
            .iter()
            .filter(|l| l.len() > 0)
            .enumerate()
            .map(|(y, l)| {
                l.chars()
                    .enumerate()
                    .map(|(x, c)| match c {
                        '^' => {
                            droid_pos = Some((x, y));
                            droid_heading = Some(Heading::North);
                            Tile::Scaffold
                        }
                        '<' => {
                            droid_pos = Some((x, y));
                            droid_heading = Some(Heading::West);
                            Tile::Scaffold
                        }
                        '>' => {
                            droid_pos = Some((x, y));
                            droid_heading = Some(Heading::East);
                            Tile::Scaffold
                        }
                        'v' => {
                            droid_pos = Some((x, y));
                            droid_heading = Some(Heading::South);
                            Tile::Scaffold
                        }
                        '#' => Tile::Scaffold,
                        '.' => Tile::Space,
                        _ => panic!("unexpected char {} in view", c),
                    })
                    .collect::<Vec<Tile>>()
            })
            .collect();

        // sanity check things before constructing & returning
        if droid_pos.is_none() || droid_heading.is_none() {
            panic!("we never saw the droid");
        }
        if lines.len() == 0 {
            panic!("no lines?");
        }
        let x_size = lines[0].len();
        if lines.iter().any(|l| l.len() != x_size) {
            let lens = lines.iter().map(|l| l.len()).collect::<Vec<usize>>();
            println!("DEBUG: x_size={:?} all_line_lengths={:?}", x_size, lens);
            panic!("lines should all be the same size");
        }

        View {
            points: lines,
            droid: droid_pos.unwrap(),
            droid_heading: droid_heading.unwrap(),
        }
    }

    // defaults to Space if out of bounds
    pub fn at_point(&self, (x, y): Point) -> Tile {
        let p = self
            .points
            .get(y as usize)
            .and_then(|line| line.get(x as usize));

        p.unwrap_or(&Tile::Space).clone()
    }

    // identify points at which scaffolds intersect
    pub fn intersections(&self) -> Vec<Point> {
        self.points
            .iter()
            .enumerate()
            .flat_map(|(y, line)| {
                line.iter()
                    .enumerate()
                    .filter(|(x, t)| {
                        if t != &&Tile::Scaffold {
                            return false;
                        }

                        // edges can't be intersections anyway, and since we're using usize for
                        // positions we can't subtract if either axis is 0
                        if x == &0 || y == 0 {
                            return false;
                        }

                        let neighbors = vec![
                            self.at_point((*x, y + 1)),
                            self.at_point((*x, y - 1)),
                            self.at_point((*x + 1, y)),
                            self.at_point((*x - 1, y)),
                        ];

                        neighbors.iter().all(|n| n == &Tile::Scaffold)
                    })
                    .map(|(x, _t)| (x, y))
                    .collect::<Vec<Point>>()
            })
            .collect::<Vec<Point>>()
    }

    // p1 answer: take intersections, multiply x * y each, sum them all
    pub fn calibrate(&self) -> usize {
        self.intersections().iter().map(|(x, y)| x * y).sum()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn aoc_ex_1() -> String {
        let mut s = String::new();

        s.push_str("..#..........\n");
        s.push_str("..#..........\n");
        s.push_str("#######...###\n");
        s.push_str("#.#...#...#.#\n");
        s.push_str("#############\n");
        s.push_str("..#...#...#..\n");
        s.push_str("..#####...^..\n");

        s
    }

    #[test]
    fn test_scanning_aoc_ex() {
        let v = View::scan(&aoc_ex_1());

        assert_eq!(v.droid, (10, 6));
    }

    #[test]
    fn test_intersections_aoc_ex() {
        let v = View::scan(&aoc_ex_1());

        assert_eq!(v.intersections(), vec![(2, 2), (2, 4), (6, 4), (10, 4),]);
    }

    #[test]
    fn test_calibration_aoc_ex() {
        let v = View::scan(&aoc_ex_1());

        assert_eq!(v.calibrate(), 76);
    }
}
