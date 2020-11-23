use crate::reaction::*;
use std::fs::File;
use std::io::Read;
use std::vec::Vec;

pub fn parse_file(path: &str) -> Vec<Reaction> {
    let mut file = File::open(path).expect("couldn't open file");
    let mut input = String::new();
    file.read_to_string(&mut input).expect("should read to EOF");

    parse_str(&input)
}

pub fn parse_str(lines: &str) -> Vec<Reaction> {
    lines
        .trim()
        .split("\n")
        .map(|line| parse_reaction(line))
        .collect()
}

fn parse_reaction(line: &str) -> Reaction {
    let parts: Vec<&str> = line.trim().split(" => ").collect();
    if parts.len() != 2 {
        panic!("line '{}' should have 2 parts - inputs and outputs", line);
    }

    let inputs: Vec<MaterialAmount> = parts[0]
        .split(", ")
        .map(|input_str| {
            let cnt_and_name: Vec<&str> = input_str.split(" ").collect();
            if cnt_and_name.len() != 2 {
                panic!(
                    "inputs '{}' should have 2 parts - count and material",
                    parts[0]
                );
            }
            let cnt: i64 = cnt_and_name[0].parse().unwrap();

            (cnt, String::from(cnt_and_name[1]))
        })
        .collect();

    let out_parts: Vec<&str> = parts[1].split(" ").collect();
    if out_parts.len() != 2 {
        panic!(
            "ouput '{}' should have 2 parts - count and material",
            parts[1]
        );
    }
    let out_cnt: i64 = out_parts[0].parse().unwrap();

    Reaction {
        inputs: inputs,
        output: (out_cnt, out_parts[1].to_string()),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_reaction_1() {
        let line = "1 A, 2 B, 3 C => 4 D";
        let reaction = parse_reaction(line);

        assert_eq!(
            reaction,
            Reaction {
                inputs: vec![
                    (1, "A".to_string()),
                    (2, "B".to_string()),
                    (3, "C".to_string())
                ],
                output: (4, "D".to_string()),
            }
        );
    }

    #[test]
    fn test_parse_str_1() {
        let lines = "1 A, 2 B, 3 C => 4 D\n7 E => 1 F\n";
        let reactions = parse_str(lines);

        assert_eq!(
            reactions,
            vec![
                Reaction {
                    inputs: vec![
                        (1, "A".to_string()),
                        (2, "B".to_string()),
                        (3, "C".to_string())
                    ],
                    output: (4, "D".to_string()),
                },
                Reaction {
                    inputs: vec![(7, "E".to_string())],
                    output: (1, "F".to_string()),
                }
            ],
        );
    }
}
