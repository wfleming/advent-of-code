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
}
