use std::cmp::{Ord, Ordering};
use std::env::args;
use std::fmt;
use std::fs;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, PartialEq, Eq, Debug)]
enum PacketChunk {
    Num(u8),
    SubChunk(Vec<PacketChunk>),
}

impl PacketChunk {
    pub fn parse(chars: &mut Peekable<Chars>) -> PacketChunk {
        match chars.next().unwrap() {
            '[' => Self::parse_array(chars),
            ',' => Self::parse(chars),
            c => match chars.peek().unwrap() {
                ',' => {
                    // println!("debug: parsing digit {} before comma", c);
                    Self::Num(c.to_digit(10).unwrap() as u8)
                }
                ']' => {
                    // println!("debug: parsing digit {} before end bracket", c);
                    Self::Num(c.to_digit(10).unwrap() as u8)
                }
                _ => {
                    let n: String = [c, chars.next().unwrap()].iter().collect();
                    // println!("debug: 2 digit num {}", n);
                    Self::Num(n.parse::<u8>().unwrap())
                }
            },
        }
    }

    fn parse_array(chars: &mut Peekable<Chars>) -> PacketChunk {
        let mut chunk: Vec<PacketChunk> = vec![];

        while chars.peek() != Some(&']') {
            chunk.push(Self::parse(chars));
        }
        chars.next(); // consume ]

        Self::SubChunk(chunk)
    }

    fn correct_order(&self, other: &PacketChunk) -> bool {
        self.cmp(other) != Ordering::Greater
    }
}

impl Ord for PacketChunk {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => a.cmp(b),
            (Self::Num(_), Self::SubChunk(_)) => Self::SubChunk(vec![self.clone()]).cmp(other),
            (Self::SubChunk(_), Self::Num(_)) => self.cmp(&Self::SubChunk(vec![other.clone()])),
            (Self::SubChunk(a), Self::SubChunk(b)) => {
                // I had a bug at first where I didn't explicitly check when both are empty and
                // just returned "less" when a.is_empty(). That gave too high an answer for p1
                // because it effectively "short-circuited" the rest of the cmp logic, since a
                // packet is < another packet the *first* time any left element < right element.
                // I.e., by the described problem logic [[],5] > [[],3], but I was returning
                // [[],5] < [[],3]
                if a.is_empty() && b.is_empty() {
                    Ordering::Equal
                } else if a.is_empty() {
                    Ordering::Less
                } else if b.is_empty() {
                    Ordering::Greater
                } else if a[0].cmp(&b[0]) == Ordering::Less {
                    Ordering::Less
                } else if a[0].cmp(&b[0]) == Ordering::Greater {
                    Ordering::Greater
                } else {
                    let a2 = Self::SubChunk(a[1..].to_vec());
                    let b2 = Self::SubChunk(b[1..].to_vec());
                    a2.cmp(&b2)
                }
            }
        }
    }
}

impl PartialOrd for PacketChunk {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for PacketChunk {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Num(x) => write!(f, "{}", x),
            Self::SubChunk(xs) => {
                let xs_strs: Vec<String> = xs.iter().map(|x| format!("{}", x)).collect();
                write!(f, "[{}]", xs_strs.join(","))
            }
        }
    }
}

fn parse_packet_pairs(s: &str) -> Vec<(PacketChunk, PacketChunk)> {
    let mut pairs: Vec<(PacketChunk, PacketChunk)> = vec![];
    let mut lines = s.lines();

    while let (Some(l1), Some(l2)) = (lines.next(), lines.next()) {
        pairs.push((
            PacketChunk::parse(&mut l1.chars().peekable()),
            PacketChunk::parse(&mut l2.chars().peekable()),
        ));
        lines.next();
    }

    pairs
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let packet_pairs = parse_packet_pairs(&input);

    // DEBUG round trip of parsing
    // for pair in &packet_pairs {
    //     println!("{}\n{}\n", pair.0, pair.1);
    // }
    // return;
    // END DEBUG round trip of parsing

    let correct_order_indices = packet_pairs.iter().enumerate().filter_map(|(idx, pair)| {
        if pair.0.correct_order(&pair.1) {
            Some(idx + 1)
        } else {
            None
        }
    });
    println!(
        "p1: sum of correctly ordered pair indices is {}",
        correct_order_indices.sum::<usize>()
    );

    let mut all_packets: Vec<&PacketChunk> = packet_pairs
        .iter()
        .flat_map(|pair| [&pair.0, &pair.1])
        .collect();
    let divider1 = PacketChunk::parse(&mut "[[2]]".chars().peekable());
    let divider2 = PacketChunk::parse(&mut "[[6]]".chars().peekable());
    all_packets.push(&divider1);
    all_packets.push(&divider2);
    all_packets.sort();

    let div1_idx = all_packets.binary_search(&&divider1).unwrap() + 1;
    let div2_idx = all_packets.binary_search(&&divider2).unwrap() + 1;
    let decoder_key = div1_idx * div2_idx;
    println!(
        "p2: {} is at {}, {} is at {}, decoder key is {}",
        divider1, div1_idx, divider2, div2_idx, decoder_key
    );
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_1() {
        let p = PacketChunk::parse(&mut "[1,1,3,1,1]".chars().peekable());
        assert_eq!(
            p,
            PacketChunk::SubChunk(vec![
                PacketChunk::Num(1),
                PacketChunk::Num(1),
                PacketChunk::Num(3),
                PacketChunk::Num(1),
                PacketChunk::Num(1),
            ])
        )
    }

    #[test]
    fn test_parse_2() {
        let p = PacketChunk::parse(&mut "[1,[2,[3,[4,[5,6,7]]]],8,9]".chars().peekable());
        assert_eq!(
            p,
            PacketChunk::SubChunk(vec![
                PacketChunk::Num(1),
                PacketChunk::SubChunk(vec![
                    PacketChunk::Num(2),
                    PacketChunk::SubChunk(vec![
                        PacketChunk::Num(3),
                        PacketChunk::SubChunk(vec![
                            PacketChunk::Num(4),
                            PacketChunk::SubChunk(vec![
                                PacketChunk::Num(5),
                                PacketChunk::Num(6),
                                PacketChunk::Num(7),
                            ])
                        ])
                    ])
                ]),
                PacketChunk::Num(8),
                PacketChunk::Num(9),
            ])
        )
    }

    #[test]
    fn test_correct_order_1() {
        let p1 = PacketChunk::parse(&mut "[1,1,3,1,1]".chars().peekable());
        let p2 = PacketChunk::parse(&mut "[1,1,5,1,1]".chars().peekable());

        assert!(p1.correct_order(&p2));
    }

    #[test]
    fn test_correct_order_2() {
        let p1 = PacketChunk::parse(&mut "[[1],[2,3,4]]".chars().peekable());
        let p2 = PacketChunk::parse(&mut "[[1],4]".chars().peekable());

        assert!(p1.correct_order(&p2));
    }

    #[test]
    fn test_correct_order_3() {
        let p1 = PacketChunk::parse(&mut "[9]".chars().peekable());
        let p2 = PacketChunk::parse(&mut "[[8,7,6]]".chars().peekable());

        assert!(!p1.correct_order(&p2));
    }

    #[test]
    fn test_correct_order_4() {
        let p1 = PacketChunk::parse(&mut "[[4,4],4,4]".chars().peekable());
        let p2 = PacketChunk::parse(&mut "[[4,4],4,4,4]".chars().peekable());

        assert!(p1.correct_order(&p2));
    }

    #[test]
    fn test_correct_order_5() {
        let p1 = PacketChunk::parse(&mut "[7,7,7,7]".chars().peekable());
        let p2 = PacketChunk::parse(&mut "[7,7,7]".chars().peekable());

        assert!(!p1.correct_order(&p2));
    }

    #[test]
    fn test_correct_order_6() {
        let p1 = PacketChunk::parse(&mut "[]".chars().peekable());
        let p2 = PacketChunk::parse(&mut "[3]".chars().peekable());

        assert!(p1.correct_order(&p2));
    }

    #[test]
    fn test_correct_order_7() {
        let p1 = PacketChunk::parse(&mut "[[[]]]".chars().peekable());
        let p2 = PacketChunk::parse(&mut "[[]]".chars().peekable());

        assert!(!p1.correct_order(&p2));
    }

    #[test]
    fn test_correct_order_8() {
        let p1 = PacketChunk::parse(&mut "[1,[2,[3,[4,[5,6,7]]]],8,9]".chars().peekable());
        let p2 = PacketChunk::parse(&mut "[1,[2,[3,[4,[5,6,0]]]],8,9]".chars().peekable());

        assert!(!p1.correct_order(&p2));
    }

    #[test]
    fn test_eql_order() {
        let p1 = PacketChunk::parse(&mut "[1,[2,[3,[4,[5,6,7]]]],8,9]".chars().peekable());
        let p2 = p1.clone();

        assert_eq!(p1.cmp(&p2), Ordering::Equal);
    }
}
