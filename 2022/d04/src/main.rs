use std::env::args;
use std::fs;
use std::io::{self, BufRead};
use std::ops::RangeInclusive;

type ElfPair = (RangeInclusive<u32>, RangeInclusive<u32>);

fn parse_range(s: &str) -> RangeInclusive<u32> {
    let pieces: Vec<&str> = s.split("-").collect();
    let min = pieces[0].parse::<u32>().expect("should be a number");
    let max = pieces[1].parse::<u32>().expect("should be a number");
    min..=max
}

fn parse_elf_pair(s: &str) -> ElfPair {
    let elves: Vec<&str> = s.split(",").collect();
    (parse_range(elves[0]), parse_range(elves[1]))
}

fn parse_input(input_path: &str) -> Vec<ElfPair> {
    let f = fs::File::open(input_path)
        .map_err(|e| format!("{}", e))
        .expect("should be able to read file");
    let br = io::BufReader::new(f);

    br.lines().map(|l| parse_elf_pair(&l.unwrap())).collect()
}

// one range is fully contained in the other
fn pair_is_redundant(p: &ElfPair) -> bool {
    (p.0.contains(&p.1.start()) && p.0.contains(&p.1.end()))
        || (p.1.contains(&p.0.start()) && p.1.contains(&p.0.end()))
}

fn pair_overlaps(p: &ElfPair) -> bool {
    p.0.contains(&p.1.start())
        || p.0.contains(&p.1.end())
        || p.1.contains(&p.0.start())
        || p.1.contains(&p.0.end())
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let all_pairs = parse_input(&input_path);

    let p1 = all_pairs.iter().filter(|p| pair_is_redundant(p)).count();
    println!("p1: There are {} pairs with a redundant member", p1);

    let p2 = all_pairs.iter().filter(|p| pair_overlaps(p)).count();
    println!("p2: There are {} pairs with overlap", p2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_range() {
        assert_eq!(parse_range("1-3"), 1..=3);
    }

    #[test]
    fn test_elf_pair() {
        assert_eq!(parse_elf_pair("1-3,7-9"), (1..=3, 7..=9));
    }
}
