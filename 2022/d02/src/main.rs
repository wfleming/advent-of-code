use std::env::args;
use std::fs;
use std::io::{self, BufRead};

#[derive(PartialEq, Clone, Copy)]
enum Rochambeau {
    Rock,
    Paper,
    Scissor,
}

impl Rochambeau {
    pub fn from_char(c: char) -> Rochambeau {
        match c {
            'A' | 'X' => Rochambeau::Rock,
            'B' | 'Y' => Rochambeau::Paper,
            'C' | 'Z' => Rochambeau::Scissor,
            _ => panic!("Unexpected character, no matching rochambeau play"),
        }
    }

    pub fn score(&self) -> u32 {
        match self {
            Self::Rock => 1,
            Self::Paper => 2,
            Self::Scissor => 3,
        }
    }

    pub fn beats(&self) -> Rochambeau {
        match self {
            Self::Rock => Self::Scissor,
            Self::Paper => Self::Rock,
            Self::Scissor => Self::Paper,
        }
    }

    pub fn loses_to(&self) -> Rochambeau {
        match self {
            Self::Rock => Self::Paper,
            Self::Paper => Self::Scissor,
            Self::Scissor => Self::Rock,
        }
    }
}

struct Round((Rochambeau, Rochambeau));

impl Round {
    pub fn parse_rounds(input_path: &str) -> Vec<Round> {
        let f = fs::File::open(input_path).expect("Input path should exist");
        let br = io::BufReader::new(f);

        br.lines()
            .map(|lr| {
                let l = lr.expect("Should not have errors reading lines");
                Round((
                    Rochambeau::from_char(l.chars().nth(0).unwrap()),
                    Rochambeau::from_char(l.chars().nth(2).unwrap()),
                ))
            })
            .collect()
    }

    pub fn parse_rounds_p2(input_path: &str) -> Vec<Round> {
        let f = fs::File::open(input_path).expect("Input path should exist");
        let br = io::BufReader::new(f);

        br.lines()
            .map(|lr| {
                let l = lr.expect("Should not have errors reading lines");
                let p1 = Rochambeau::from_char(l.chars().nth(0).unwrap());
                let goal = l.chars().nth(2).unwrap();
                let p2 = match goal {
                    'X' => p1.beats(),    // p2 loses intentionally
                    'Y' => p1,            // p2 draws
                    'Z' => p1.loses_to(), // p2 wins
                    _ => panic!("Unexpected char"),
                };
                Round((p1, p2))
            })
            .collect()
    }

    pub fn score(&self) -> (u32, u32) {
        if self.0 .0.beats() == self.0 .1 {
            (self.0 .0.score() + 6, self.0 .1.score())
        } else if self.0 .1.beats() == self.0 .0 {
            (self.0 .0.score(), self.0 .1.score() + 6)
        } else {
            (self.0 .0.score() + 3, self.0 .1.score() + 3)
        }
    }
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let rounds = Round::parse_rounds(&input_path);
    let tot_scores = rounds.iter().fold((0, 0), |tot, r| {
        let s = r.score();
        (tot.0 + s.0, tot.1 + s.1)
    });

    println!("p1: your total score is {}", tot_scores.1);

    let rounds = Round::parse_rounds_p2(&input_path);
    let tot_scores = rounds.iter().fold((0, 0), |tot, r| {
        let s = r.score();
        (tot.0 + s.0, tot.1 + s.1)
    });
    println!("p2: your total score is {}", tot_scores.1);
}
