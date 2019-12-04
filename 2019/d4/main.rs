use std::vec::Vec;

type Password = i32;

const MIN: Password = 356261;
const MAX: Password = 846303;

fn pass_digits(pass: Password) -> Vec<i32> {
    pass.to_string()
        .chars()
        .filter_map(|x| x.to_digit(10))
        .map(|x| x as i32)
        .collect()
}

fn digits_repeat(digits: &Vec<i32>) -> bool {
    for i in 0..(digits.len() - 1) {
        if digits[i] == digits[i + 1] {
            return true;
        }
    }

    false
}

fn digits_dont_decrease(digits: &Vec<i32>) -> bool {
    // the fact that .. is *inclusive* in ruby and *exclusive* on the right in rust is really
    // messing me up.
    for i in 0..(digits.len() - 1) {
        if digits[i + 1] < digits[i] {
            return false;
        }
    }
true
}

fn valid_pass_p1(pass: Password) -> bool {
    let digits = pass_digits(pass);
    digits_repeat(&digits) && digits_dont_decrease(&digits)
}

// for p2, the group must be *exactly* 2 long
fn digits_repeat_p2(digits: &Vec<i32>) -> bool {
    let mut repeats: Vec<i32> = Vec::new();

    for i in 0..digits.len() {
        if 0 == repeats.len() {
            repeats.push(digits[i]);
        } else if *repeats.last().expect("non-empty vec") == digits[i] {
            repeats.push(digits[i]);
        } else {
            // if we're switching to a new group and this one was exactly 2, break so we can return
            // true
            if repeats.len() == 2 {
                break
            }
            repeats.clear();
            repeats.push(digits[i]);
        }
    }

    repeats.len() == 2
}

fn valid_pass_p2(pass: Password) -> bool {
    let digits = pass_digits(pass);
    digits_repeat_p2(&digits) && digits_dont_decrease(&digits)
}


fn main() {
    let mut valid_count_p1 = 0;
    let mut valid_count_p2 = 0;

    for pass in MIN..MAX {
        if valid_pass_p1(pass) {
            valid_count_p1 += 1;
        }
        if valid_pass_p2(pass) {
            valid_count_p2 += 1;
        }
    }

    println!("p1: found {} valid passwords in range", valid_count_p1);
    println!("p2: found {} valid passwords in range", valid_count_p2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_pass_p1() {
        assert_eq!(valid_pass_p1(111111), true);
        assert_eq!(valid_pass_p1(111123), true);
        assert_eq!(valid_pass_p1(122345), true);
        assert_eq!(valid_pass_p1(135679), false);
        assert_eq!(valid_pass_p1(223450), false);
        assert_eq!(valid_pass_p1(123789), false);
    }

    #[test]
    fn test_valid_pass_p2() {
        assert_eq!(valid_pass_p2(112233), true);
        assert_eq!(valid_pass_p2(123444), false);
        assert_eq!(valid_pass_p2(111122), true);

        assert_eq!(valid_pass_p2(133344), true);
        assert_eq!(valid_pass_p2(133345), false);
    }
}
