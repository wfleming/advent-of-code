extern crate crypto;

use crypto::digest::Digest;
use crypto::md5::Md5;
use std::iter::FromIterator;

#[derive(Debug,PartialEq)]
struct CharPos {
  c: char,
  i: u64,
}

fn main() {
  let seed = "reyedfim";
  let mut idx = 0;
  let mut pass : Vec<Option<char>> = vec![
    None, None, None, None, None, None, None, None
  ];

  while pass.iter().any(|o| o.is_none()) {
    //if 0 ==  idx % 100000 {
      //println!(
        //"doing iteration: {}, found {} entries",
        //idx,
        //pass.iter().filter(|o| o.is_some()).count()
      //);
    //}

    let h = hash(seed, idx);
    if valid_char(&h) {
      let cp = char_pos(&h);
      let cp_i = cp.i as usize;

      if pass[cp_i].is_none() {
        pass[cp_i] = Some(cp.c);
      }
    }

    idx += 1;
  }

  let pass_str : Vec<char> = pass.iter().map(|o| o.expect("didn't find them all")).collect();

  println!("");
  println!("Done: {:?}", pass_str);
}

fn hash(seed: &str, idx: u64) -> String {
  let mut hasher = Md5::new();

  hasher.input(seed.as_bytes());
  hasher.input(idx.to_string().as_bytes());

  hasher.result_str()
}

fn valid_char(hash: &str) -> bool {
  if !hash.starts_with("00000") {
    return false;
  }

  let c_idx_res = hash[5..6].parse::<u64>();
  if c_idx_res.is_err() {
    return false;
  }

  let c_idx = c_idx_res.expect("wtf");
  c_idx < 8
}

fn char_pos(hash: &str) -> CharPos {
  let chars = Vec::from_iter(hash.chars());
  let c = chars[6];
  let c_idx = hash[5..6].parse::<u64>().expect("validate before calling");

  CharPos { c: c, i: c_idx }
}

#[test]
fn test_good_hash() {
  let h = hash("abc", 3231929);

  println!("testing the good hash: {}", h);
  assert_eq!(true, valid_char(&h));
  assert_eq!(CharPos { c: '5', i: 1 }, char_pos(&h));
}

#[test]
fn test_bad_hash() {
  let h = hash("abc", 5017308);

  assert_eq!(false, valid_char(&h))
}
