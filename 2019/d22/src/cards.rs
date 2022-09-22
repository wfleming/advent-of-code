type Card = u16;
type Deck = Vec<Card>;

pub fn new_deck(n: usize) -> Deck {
    return (0..(n as Card)).collect();
}

pub fn stack_deal(d: Deck) -> Deck {
    // copied instead of collect?
    return d.iter().rev().map(|c| c.clone()).collect();
}

pub fn cut_deck(mut d: Deck, mut n: i32) -> Deck {
    if n < 0 {
        n = d.len() as i32 + n;
    }

    let mut r = d.split_off(n as usize);
    r.append(&mut d);
    r
}

pub fn incr_deal(d: Deck, n: usize) -> Deck {
    let mut d2: Vec<Option<Card>> = vec![None; d.len()];

    for i in 0..d.len() {
        let d2i = (i * n) % d.len();
        d2[d2i] = Some(d[i]);
    }

    d2.iter()
        .map(|c| c.expect("should have been filled"))
        .collect()
}

pub fn execute_shuffle(mut d: Deck, steps: &Vec<&str>) -> Deck {
    for s in steps {
        if s.trim() == "deal into new stack" {
            d = stack_deal(d);
        } else if s.starts_with("cut ") {
            let n: i32 = s[4..].parse().expect("cut number");
            d = cut_deck(d, n);
        } else if s.starts_with("deal with increment") {
            let n: usize = s[20..].parse().expect("deal increment");
            d = incr_deal(d, n);
        } else {
            panic!("Unknown step")
        }
    }

    d
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_small_deck() {
        let d = new_deck(10);
        assert_eq!(d, vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
    }

    #[test]
    fn test_stack_deal() {
        let d = stack_deal(new_deck(10));
        assert_eq!(d, vec![9, 8, 7, 6, 5, 4, 3, 2, 1, 0]);
    }

    #[test]
    fn test_cut_deck() {
        let d = cut_deck(new_deck(10), 3);
        assert_eq!(d, vec![3, 4, 5, 6, 7, 8, 9, 0, 1, 2]);
    }

    #[test]
    fn test_cut_deck_negative() {
        let d = cut_deck(new_deck(10), -4);
        assert_eq!(d, vec![6, 7, 8, 9, 0, 1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_incr_deal() {
        let d = incr_deal(new_deck(10), 3);
        assert_eq!(d, vec![0, 7, 4, 1, 8, 5, 2, 9, 6, 3]);
    }

    #[test]
    fn test_execute_shuffle_sample1() {
        let d = new_deck(10);
        let steps = vec![
            "deal with increment 7",
            "deal into new stack",
            "deal into new stack",
        ];
        let d2 = execute_shuffle(d, &steps);
        assert_eq!(d2, vec![0, 3, 6, 9, 2, 5, 8, 1, 4, 7]);
    }

    #[test]
    fn test_execute_shuffle_sample2() {
        let d = new_deck(10);
        let steps = vec!["cut 6", "deal with increment 7", "deal into new stack"];
        let d2 = execute_shuffle(d, &steps);
        assert_eq!(d2, vec![3, 0, 7, 4, 1, 8, 5, 2, 9, 6]);
    }

    #[test]
    fn test_execute_shuffle_sample3() {
        let d = new_deck(10);
        let steps = vec!["deal with increment 7", "deal with increment 9", "cut -2"];
        let d2 = execute_shuffle(d, &steps);
        assert_eq!(d2, vec![6, 3, 0, 7, 4, 1, 8, 5, 2, 9]);
    }

    #[test]
    fn test_execute_shuffle_sample4() {
        let d = new_deck(10);
        let steps = vec![
            "deal into new stack",
            "cut -2",
            "deal with increment 7",
            "cut 8",
            "cut -4",
            "deal with increment 7",
            "cut 3",
            "deal with increment 9",
            "deal with increment 3",
            "cut -1",
        ];
        let d2 = execute_shuffle(d, &steps);
        assert_eq!(d2, vec![9, 2, 5, 8, 1, 4, 7, 0, 3, 6]);
    }
}
