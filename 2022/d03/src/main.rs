use std::env::args;
use std::fs;
use std::io::{self, BufRead};

fn priority(c: char) -> Result<u32, String> {
    if c.is_ascii_lowercase() {
        Ok((c as u32) - 96) // 'a' is 97
    } else if c.is_ascii_uppercase() {
        Ok((c as u32) - 38) // 'A' is 65
    } else {
        Err("Invalid character, no defined priority".to_string())
    }
}

struct Rucksack(Vec<char>);

impl Rucksack {
    pub fn parse(line: &str) -> Rucksack {
        let chars = line.chars().collect::<Vec<char>>();
        Rucksack(chars)
    }

    pub fn shared_item(&self) -> Result<char, String> {
        let c1 = &self.0[0..(self.0.len() / 2)];
        let c2 = &self.0[(self.0.len() / 2)..];

        for i1 in c1 {
            for i2 in c2 {
                if i1 == i2 {
                    return Ok(*i1);
                }
            }
        }
        Err("no common item found".to_string())
    }
}

fn parse_input(input_path: &str) -> Result<Vec<Rucksack>, String> {
    let f = fs::File::open(input_path).map_err(|e| format!("{}", e))?;
    let br = io::BufReader::new(f);

    Ok(br.lines().map(|l| Rucksack::parse(&l.unwrap())).collect())
}

fn p1(rucksacks: &Vec<Rucksack>) -> Result<u32, String> {
    rucksacks.iter().map(|r| priority(r.shared_item()?)).sum()
}

fn group_badge(rucksacks: &[Rucksack]) -> Result<char, String> {
    if rucksacks.len() != 3 {
        return Err("Unexpected slice size".to_string());
    }

    for i1 in &rucksacks[0].0 {
        for i2 in &rucksacks[1].0 {
            if i1 != i2 {
                continue;
            }
            for i3 in &rucksacks[2].0 {
                if i1 == i2 && i1 == i3 {
                    return Ok(*i1);
                }
            }
        }
    }
    Err("No badge found in group".to_string())
}

fn p2(rucksacks: &Vec<Rucksack>) -> Result<u32, String> {
    rucksacks
        .as_slice()
        .chunks(3)
        .map(|g| priority(group_badge(g)?))
        .sum()
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let rucksacks = match parse_input(&input_path) {
        Ok(rs) => rs,
        Err(e) => panic!("Error parsing input: {}", e),
    };

    match p1(&rucksacks) {
        Ok(x) => println!("p1: summed priorities of shared items: {}", x),
        Err(e) => panic!("p1: error: {}", e),
    }

    match p2(&rucksacks) {
        Ok(x) => println!("p1: summed priorities of badges: {}", x),
        Err(e) => panic!("p2: error: {}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_priority() {
        assert_eq!(priority('a'), Ok(1));
        assert_eq!(priority('z'), Ok(26));
        assert_eq!(priority('A'), Ok(27));
        assert_eq!(priority('Z'), Ok(52));
    }

    #[test]
    fn test_shared_item() {
        let r = Rucksack::parse("vJrwpWtwJgWrhcsFMMfFFhFp");
        assert_eq!(r.shared_item(), Ok('p'));
    }
}
