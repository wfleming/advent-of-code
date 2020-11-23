pub type Signal = Vec<i32>;

pub fn pattern_for_idx(idx: usize) -> Signal {
    let base_pat: Signal = vec![0, 1, 0, -1];
    base_pat
        .iter()
        .flat_map(|x| {
            let mut v = Signal::with_capacity(idx + 1);
            for _i in 0..(idx + 1) {
                v.push(*x);
            }
            v
        })
        .collect()
}

pub fn apply_phase(sig: Signal) -> Signal {
    let mut rv = Signal::with_capacity(sig.len());

    for idx in 0..sig.len() {
        let p: Signal = pattern_for_idx(idx)
            .iter()
            .cycle()
            .skip(1)
            .take(sig.len())
            .map(|x| *x)
            .collect();
        let new_val = sig.iter().zip(p).fold(0, |memo, (x, y)| memo + (x * y));
        let new_val = (new_val % 10).abs();

        rv.push(new_val);
    }

    rv
}

pub fn apply_phase_n(sig: Signal, n: usize) -> Signal {
    let mut s = sig;

    for _x in 0..n {
        s = apply_phase(s);
    }

    s
}

pub fn expand_10k(s: &Signal) -> Signal {
    let mut s10k = Vec::with_capacity(10_000 * s.len());
    for _x in 0..10_000 {
        s10k.extend(s.iter());
    }
    s10k
}

pub fn p2_offset(s: &Signal) -> usize {
    s.iter()
        .take(7)
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join("")
        .parse::<usize>()
        .unwrap()
}

pub fn apply_phase_p2(sig: Signal) -> Signal {
    let mut rv = sig.clone();

    for idx in 0..sig.len() {
        // let p: Signal = pattern_for_idx(idx)
        //     .iter()
        //     .cycle()
        //     .skip(1)
        //     .take(sig.len())
        //     .map(|x| *x)
        //     .collect();
        // let new_val = sig.iter().zip(p).filter(|(x,y)| **x != 0 && *y != 0).fold(0, |memo, (x, y)| memo + (x * y));
        // let new_val = (new_val % 10).abs();
        // rv.push(new_val);

        // try this: for idx i, sum *last* i entries, mod 10
        // let n = sig.len() - idx;
        // let new_val = sig.iter().skip(idx).sum::<i32>() % 10;
        // let new_val = sig[idx..].iter().sum::<i32>() % 10;
        // rv.push(new_val);

        if idx == 0 {
            rv[sig.len() - 1] = sig[sig.len() - 1];
        } else {
            rv[sig.len() - 1 - idx] = (rv[sig.len() - idx] + rv[sig.len() - 1 - idx]) % 10;
        }
    }

    rv
}

pub fn apply_phase_n_p2(sig: Signal, n: usize) -> Signal {
    let mut s = sig;

    for _x in 0..n {
        // println!("DEBUG: apply_phase_n_p2 iteration {}", _x + 1);
        s = apply_phase_p2(s);
    }

    s
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn aoc_ex_1_phase_1() {
        let s: Signal = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let s_prime = apply_phase(s);

        assert_eq!(s_prime, vec![4, 8, 2, 2, 6, 1, 5, 8]);
    }

    #[test]
    fn aoc_ex_1_phase_4() {
        let s: Signal = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let s_prime = apply_phase_n(s, 4);

        assert_eq!(s_prime, vec![0, 1, 0, 2, 9, 4, 9, 8]);
    }

    #[test]
    fn aoc_p2_ex_1() {
        let s: Signal = vec![
            0, 3, 0, 3, 6, 7, 3, 2, 5, 7, 7, 2, 1, 2, 9, 4, 4, 0, 6, 3, 4, 9, 1, 5, 6, 5, 4, 7, 4,
            6, 6, 4,
        ];
        let offset = p2_offset(&s);
        let s10k = expand_10k(&s);
        let s10k_p100 = apply_phase_n_p2(s10k, 100);
        let msg = s10k_p100
            .iter()
            .skip(offset)
            .take(8)
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join("");

        assert_eq!(&msg, "84462026");
    }

    #[test]
    fn aoc_p2_ex_2() {
        let s: Signal = vec![
            0, 2, 9, 3, 5, 1, 0, 9, 6, 9, 9, 9, 4, 0, 8, 0, 7, 4, 0, 7, 5, 8, 5, 4, 4, 7, 0, 3, 4,
            3, 2, 3,
        ];
        let offset = p2_offset(&s);
        let s10k = expand_10k(&s);
        let s10k_p100 = apply_phase_n_p2(s10k, 100);
        let msg = s10k_p100
            .iter()
            .skip(offset)
            .take(8)
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join("");

        assert_eq!(&msg, "78725270");
    }
}
