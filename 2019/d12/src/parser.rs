use crate::moon::Moon;
use regex::Regex;
use std::fs::File;
use std::io::Read;
use std::vec::Vec;

pub fn parse_file(path: &str) -> Vec<Moon> {
    let mut file = File::open(path).expect("couldn't open file");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("should read to EOF");

    parse_str(&input)
}

pub fn parse_str(input: &str) -> Vec<Moon> {
    let re = Regex::new(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>").unwrap();

    re.captures_iter(input)
        .map(|cap| {
            let x: i32 = cap[1].parse().unwrap();
            let y: i32 = cap[2].parse().unwrap();
            let z: i32 = cap[3].parse().unwrap();

            Moon {
                pos: (x, y, z),
                vel: (0, 0, 0),
            }
        })
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "<x=-7, y=-8, z=9>\n<x=-12, y=-3, z=-4>\n";
        let moons = parse_str(input);

        assert_eq!(
            moons,
            vec![
                Moon {
                    pos: (-7, -8, 9),
                    vel: (0, 0, 0)
                },
                Moon {
                    pos: (-12, -3, -4),
                    vel: (0, 0, 0)
                },
            ]
        );
    }
}
