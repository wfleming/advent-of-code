mod beam;

// use intcode::num_bigint::BigInt;
// use intcode::num_traits::cast::FromPrimitive;
use intcode::tape;
use std::env::args;

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let tape = tape::from_file(&input);

    // p1
    let mut b = beam::TractorBeam::new(tape.clone());
    b.scan_area(49, 49);
    println!("p1: there are {} points affected ", b.points.len());

}
