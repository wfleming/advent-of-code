use std::env::args;
use std::fs;
use std::io::{self, BufRead};

fn priority(c: char) -> u32 {
    if c.is_ascii_lowercase() {
        (c as u32) - 96 // 'a' is 97
    } else if c.is_ascii_uppercase() {
        (c as u32) - 38 // 'A' is 65
    } else {
        panic!("Invalid character, no defined priority")
    }
}

struct Rucksack(Vec<char>);

impl Rucksack {
    pub fn parse(line: &str) -> Rucksack {
        let chars = line.chars().collect::<Vec<char>>();
        Rucksack(chars)
    }

    pub fn shared_item(&self) -> char {
        let c1 = &self.0[0..(self.0.len() / 2)];
        let c2 = &self.0[(self.0.len() / 2)..];

        for i1 in c1 {
            for i2 in c2 {
                if i1 == i2 {
                    return *i1;
                }
            }
        }
        panic!("no common item found")
    }
}

fn parse_input(input_path: &str) -> Vec<Rucksack> {
    let f = fs::File::open(input_path).expect("Input path should exist");
    let br = io::BufReader::new(f);

    br.lines().map(|l| Rucksack::parse(&l.unwrap())).collect()
}

fn p1(rucksacks: &Vec<Rucksack>) -> u32 {
    rucksacks.iter().map(|r| priority(r.shared_item())).sum()
}

fn group_badge(rucksacks: &[Rucksack]) -> char {
    if rucksacks.len() != 3 {
        panic!("Unexpected slice size");
    }

    for i1 in &rucksacks[0].0 {
        for i2 in &rucksacks[1].0 {
            if i1 != i2 {
                continue;
            }
            for i3 in &rucksacks[2].0 {
                if i1 == i2 && i1 == i3 {
                    return *i1;
                }
            }
        }
    }
    panic!("No badge found in group");
}

fn p2(rucksacks: &Vec<Rucksack>) -> u32 {
    rucksacks
        .as_slice()
        .chunks(3)
        .map(|g| priority(group_badge(g)))
        .sum()
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let rucksacks = parse_input(&input_path);

    println!("p1: summed priorities of shared items: {}", p1(&rucksacks));
    println!("p1: summed priorities of badges: {}", p2(&rucksacks));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_priority() {
        assert_eq!(priority('a'), 1);
        assert_eq!(priority('z'), 26);
        assert_eq!(priority('A'), 27);
        assert_eq!(priority('Z'), 52);
    }

    #[test]
    fn test_shared_item() {
        let r = Rucksack::parse("vJrwpWtwJgWrhcsFMMfFFhFp");
        assert_eq!(r.shared_item(), 'p');
    }
}
