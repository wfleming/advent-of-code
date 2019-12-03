use std::env;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::vec::Vec;

type Point = (i32, i32);
type Wire = Vec<Point>;

// read in the file, split by lines and split into moves
fn read_moves(path: &str) -> Vec<Vec<String>> {
    let file = File::open(path).expect("couldn't open file");
    let buffered = BufReader::new(file);

    buffered.lines().map(|l| {
        let l2 = l.expect("no line?").to_owned();
        l2.split(",").map(|s| s.trim().to_owned()).collect()
    }).collect()
}

fn build_wire(moves: &Vec<String>) -> Wire {
    let mut wire: Wire = Vec::new();
    let mut p = (0, 0);

    for m in moves.iter() {
        let dir = m.get(0..1)
            .expect("str should be long enough!");
        let dist: i32 = m.get(1..)
            .expect("str should be long enough!")
            .parse()
            .expect(&format!("couldn't get dist out of '{}'", m));

        match dir {
            "R" => run_move(&mut wire, p, (1, 0), dist),
            "L" => run_move(&mut wire, p, (-1, 0), dist),
            "D" => run_move(&mut wire, p, (0, -1), dist),
            "U" => run_move(&mut wire, p, (0, 1), dist),
            _ => panic!("invalid dir '{}' from move '{}'", dir, m),
        }

        p = *wire.last().expect("wire should have non-zero length by now");
    }

    wire
}

fn add_point(p1: Point, p2: Point) -> Point {
    (p1.0 + p2.0, p1.1 + p2.1)
}

fn run_move(wire: &mut Wire, start: Point, delta: Point, count: i32) {
    let mut p = start;
    for _i in 0..count {
        p = add_point(p, delta);
        wire.push(p);
    }
}

fn dist(p1: Point, p2: Point) -> i32 {
    (p1.0 - p2.0).abs() + (p1.1 - p2.1).abs()
}

fn step_dist(wire: &Wire, target: Point) -> i32 {
    let mut s = 0;
    for p in wire.iter() {
        s += 1;
        if p == &target {
            return s;
        }
    }

    assert!(false, "should only call this with a known intersection");
    -1
}

fn step_dist_sum(wires: &Vec<Wire>, target: Point) -> i32 {
    wires.iter().map(|w| step_dist(w, target)).sum()
}

fn intersections(wires: &Vec<Wire>) -> Vec<Point> {
    // I know there's only 2 wires, so just assume that for now.
    assert!(wires.len() == 2, "there are 2 wires");

    let w1 = &wires[0];
    let w2 = &wires[1];

    w1.iter().filter(|p1| w2.contains(p1)).map(|p| *p).collect()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let moves = read_moves(&args[1]);
    let wires = moves.iter().map(|wd| build_wire(wd)).collect();

    let intersects = intersections(&wires);
    let mut dists: Vec<i32> = intersects.iter().map(|p| dist((0,0), *p)).collect();
    dists.sort();

    // println!("DEBUG: wires= {:?}", wires);
    println!("p1: found intersects at {:?}", intersects);
    println!("p1: lowest dist is {}", dists[0]);

    let mut step_dists: Vec<i32> = intersects.iter().
        map(|p| step_dist_sum(&wires, *p)).
        collect();
    step_dists.sort();

    println!("p2: lowest steps is {}", step_dists[0]);
}
