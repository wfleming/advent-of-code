use num_bigint::{BigInt};
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::vec::Vec;

pub type Tape = Vec<BigInt>;

pub fn from_file(path: &str) -> Tape {
    let file = File::open(path).expect("couldn't open file");
    let mut buffered = BufReader::new(file);
    let mut line = String::new();
    buffered.read_line(&mut line).expect("no line?");

    from_str(&line)
}

pub fn from_str(line: &str) -> Tape {
    line.split(",").map(|x| {
        BigInt::parse_bytes(x.trim().as_bytes(), 10)
            .expect(&format!("'{}' could not be parsed", x))
    }).collect()
}

#[cfg(test)]
mod test {
    use super::*;
    use num_traits::cast::FromPrimitive;

    #[test]
    fn test_from_str_1() {
        let tape = from_str("1,2,3");
        let expected = vec![
            BigInt::from_i32(1).unwrap(),
            BigInt::from_i32(2).unwrap(),
            BigInt::from_i32(3).unwrap(),
        ];

        assert_eq!(tape, expected);
    }
}
