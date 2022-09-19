mod beam;

use intcode::tape;
use std::env::args;

fn print_p1_pts(beam: &mut beam::TractorBeam)  {
    let mut min_ratio = 200.0;
    let mut max_ratio = 0.0;

    for y in 0..50 {
        for x in 0..50 {
            if beam.check_pt(&(x,y)) {
                print!("o");

                let r = x as f32 / y as f32;
                if r < min_ratio {
                    min_ratio = r;
                }
                if r > max_ratio {
                    max_ratio = r;
                }
            } else {
                print!(".");
            }
        }
        print!("\n")
    }


    println!("The min x ratio was {}, the max was {}", min_ratio, max_ratio);
}

fn p1(beam: &mut beam::TractorBeam) -> u32 {
    let mut rv = 0;
    for y in 0..50 {
        for x in 0..50 {
            if beam.check_pt(&(x,y)) {
                 rv += 1;
            }
        }
    }

    rv
}

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let tape = tape::from_file(&input);

    let mut b = beam::TractorBeam::new(tape.clone());
    println!("p1: there are {} points affected", p1(&mut b));
    print_p1_pts(&mut b);
    let s = b.find_ship();
    match s {
        Some(p) => {
            println!("p2: ship starts at ({},{})", p.0, p.1);
            let a = (p.0 * 10000) + p.1;
            println!("p2: answer is {}", a);
        },
        None => println!("p2: didn't find the ship"),
    }
}
