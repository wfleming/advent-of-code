use std::cell::RefCell;
use std::collections::VecDeque;
use std::env::args;
use std::fs;

#[derive(Clone)]
enum OpArg {
    Old,
    Lit(u32),
}

impl OpArg {
    pub fn parse(input: &str) -> Self {
        match input {
            "old" => Self::Old,
            _ => Self::Lit(input.parse::<u32>().unwrap()),
        }
    }

    pub fn val(&self, old: u32) -> u32 {
        match self {
            Self::Old => old,
            Self::Lit(x) => *x,
        }
    }
}

#[derive(Clone)]
enum Op {
    Add(OpArg),
    Mul(OpArg),
}

impl Op {
    pub fn parse(input: &str) -> Self {
        let op_pieces: Vec<&str> = input.split(" ").collect();
        match op_pieces[0] {
            "+" => Self::Add(OpArg::parse(op_pieces[1])),
            "*" => Self::Mul(OpArg::parse(op_pieces[1])),
            _ => panic!("unupported op {}", input),
        }
    }

    pub fn apply(&self, x: u32) -> u32 {
        match self {
            Self::Add(arg) => x + arg.val(x),
            Self::Mul(arg) => x * arg.val(x),
        }
    }
}

#[derive(Clone)]
struct Monkey {
    items: VecDeque<u32>,
    inspect_op: Op,
    test_mod: u32,
    true_target: usize,
    false_target: usize,
    inspect_count: u32,
}

fn parse_monkeys(input: &str) -> Vec<RefCell<Monkey>> {
    let mut lines = input.lines();
    let mut monkeys: Vec<RefCell<Monkey>> = vec![];

    while let Some(line) = lines.next() {
        if line.starts_with("Monkey ") {
            let items_line = lines
                .next()
                .unwrap()
                .strip_prefix("  Starting items: ")
                .unwrap();
            let op_line = lines
                .next()
                .unwrap()
                .strip_prefix("  Operation: new = old ")
                .unwrap();
            let test_line = lines
                .next()
                .unwrap()
                .strip_prefix("  Test: divisible by ")
                .unwrap();
            let true_target_line = lines
                .next()
                .unwrap()
                .strip_prefix("    If true: throw to monkey ")
                .unwrap();
            let false_target_line = lines
                .next()
                .unwrap()
                .strip_prefix("    If false: throw to monkey ")
                .unwrap();

            monkeys.push(RefCell::new(Monkey {
                items: items_line
                    .split(", ")
                    .map(|x| x.parse::<u32>().unwrap())
                    .collect(),
                inspect_op: Op::parse(op_line),
                test_mod: test_line.parse::<u32>().unwrap(),
                true_target: true_target_line.parse::<usize>().unwrap(),
                false_target: false_target_line.parse::<usize>().unwrap(),
                inspect_count: 0,
            }))
        }
    }

    monkeys
}

fn take_turn(monkey: &RefCell<Monkey>, monkeys: &Vec<RefCell<Monkey>>) {
    while monkey.borrow().items.len() > 0 {
        let mut i = monkey.borrow_mut().items.pop_front().unwrap();
        i = monkey.borrow().inspect_op.apply(i) / 3;
        if i % monkey.borrow().test_mod == 0 {
            monkeys[monkey.borrow().true_target]
                .borrow_mut()
                .items
                .push_back(i);
        } else {
            monkeys[monkey.borrow().false_target]
                .borrow_mut()
                .items
                .push_back(i);
        }
        monkey.borrow_mut().inspect_count += 1;
    }
}

fn do_round(monkeys: &Vec<RefCell<Monkey>>) {
    for m in monkeys {
        take_turn(m, monkeys);
    }
}

fn p1(mut monkeys: Vec<RefCell<Monkey>>) {
    for _ in 0..20 {
        do_round(&monkeys);
    }

    monkeys.sort_by(|a, b| b.borrow().inspect_count.cmp(&a.borrow().inspect_count));

    let m0 = monkeys[0].borrow().inspect_count;
    let m1 = monkeys[1].borrow().inspect_count;
    println!(
        "p1: Two most active monkeys inspected {} and {} times",
        m0, m1
    );
    println!("p1: That's {} monkey business", m0 * m1);
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let monkeys = parse_monkeys(&input);

    p1(monkeys.clone());
}
