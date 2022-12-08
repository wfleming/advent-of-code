use std::cmp::max;
use std::collections::BTreeMap;
use std::env::args;
use std::fs;

type Point = (usize, usize);

struct Grid {
    // yes, nested vecs would be fine but then I couldn't use a type
    // with the word "tree" in the name.
    trees: BTreeMap<Point, u8>,
    max_x: usize,
    max_y: usize,
}

impl Grid {
    pub fn parse(input: &str) -> Self {
        let mut trees: BTreeMap<Point, u8> = BTreeMap::new();
        let (mut max_x, mut max_y) = (0, 0);

        for (y, l) in input.lines().enumerate() {
            max_y = max(max_y, y);
            for (x, c) in l.chars().enumerate() {
                max_x = max(max_x, x);
                // don't think there's a quick way to "parse" a char as int: take the ascii code
                // and subtract ascii code of 0 instead.
                trees.insert((x, y), (c as u8) - 48);
            }
        }

        Grid {
            trees: trees,
            max_x: max_x,
            max_y: max_y,
        }
    }

    // a vec for each direction. to simplify pt2, each vec runs from given pt -> edge
    pub fn pts_between_edge(&self, pt: &Point) -> Vec<Vec<Point>> {
        vec![
            (0..pt.0).map(|x| (x, pt.1)).rev().collect(), // to left
            ((pt.0 + 1)..=self.max_x).map(|x| (x, pt.1)).collect(), // to right
            (0..pt.1).map(|y| (pt.0, y)).rev().collect(), // above
            ((pt.1 + 1)..=self.max_y).map(|y| (pt.0, y)).collect(), // below
        ]
    }

    pub fn visible(&self, pt: &Point) -> bool {
        let at_pt = self.trees.get(pt).expect("an existing pt");
        self.pts_between_edge(pt).iter().any(|pts| {
            pts.iter()
                .all(|pt2| self.trees.get(pt2).expect("an existing pt") < at_pt)
        })
    }

    pub fn visible_count(&self) -> usize {
        self.trees
            .iter()
            .filter(|(pt, _)| self.visible(&pt))
            .count()
    }

    pub fn scenic_score(&self, pt: &Point) -> usize {
        if pt.0 == 0 || pt.0 == self.max_x || pt.1 == 0 || pt.1 == self.max_y {
            return 0;
        }

        let at_pt = self.trees.get(pt).expect("an existing pt");
        let mut score = 1;
        for pts in self.pts_between_edge(pt) {
            let mut visible = 0;
            for pt2 in &pts {
                visible += 1;
                if self.trees.get(pt2).expect("existing pt") >= at_pt {
                    break;
                }
            }
            score = score * visible;
        }
        score
    }
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let input = fs::read_to_string(input_path)
        .expect("read the input")
        .trim()
        .to_string();
    let grid = Grid::parse(&input);

    println!("p1: {} trees are visible", grid.visible_count());

    let best_spot = grid
        .trees
        .iter()
        .map(|(pt, _)| (pt, grid.scenic_score(&pt)))
        .max_by(|(_pt1, score1), (_pt2, score2)| score1.cmp(&score2))
        .expect("has a value");

    println!(
        "p2: the optimal tree house spot is {:?} with scenic score of {}",
        best_spot.0, best_spot.1
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_1: &str = "30373\n\
        25512\n\
        65332\n\
        33549\n\
        35390";

    #[test]
    fn sample_pt_1() {
        let grid = Grid::parse(&SAMPLE_1);
        assert_eq!(grid.visible_count(), 21);
    }

    #[test]
    fn sample_scenic_score_1() {
        let grid = Grid::parse(&SAMPLE_1);
        let pt = (2, 1);
        assert_eq!(*grid.trees.get(&pt).unwrap(), 5);
        assert_eq!(grid.scenic_score(&pt), 4);
    }

    #[test]
    fn sample_scenic_score_2() {
        let grid = Grid::parse(&SAMPLE_1);
        let pt = (2, 3);
        assert_eq!(*grid.trees.get(&pt).unwrap(), 5);
        assert_eq!(grid.scenic_score(&pt), 8);
    }

    #[test]
    fn sample_scenic_score_3() {
        let grid = Grid::parse(&SAMPLE_1);
        let pt = (2, 0);
        assert_eq!(*grid.trees.get(&pt).unwrap(), 3);
        assert_eq!(grid.scenic_score(&pt), 0);
    }
}
