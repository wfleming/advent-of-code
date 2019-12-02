use std::env::args;
use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use std::vec::Vec;

#[derive(Debug)]
#[derive(Clone)]
struct IntcodeMachine {
    tape: Vec<i32>,
    pos: usize,
}

fn new(tape: Vec<i32>) -> IntcodeMachine {
    IntcodeMachine {
        tape: tape,
        pos: 0,
    }
}

impl IntcodeMachine {
    fn run_until_exit(&mut self) {
        while !self.is_exited() {
            self.step();
        };
    }

    fn step(&mut self) {
        // println!("DEBUG: step state={:?}", self);
        match self.cur_opcode() {
            1 => self.add(),
            2 => self.mult(),
            99 => (),
            _ => panic!("bad machine state, invalid opcode"),
        };
    }

    fn add(&mut self) {
        let l = self.fetch_ref(self.pos + 1);
        let r = self.fetch_ref(self.pos + 2);
        let dest_pos = self.tape[self.pos + 3] as usize;
        self.tape[dest_pos] = l + r;
        self.pos = self.pos + 4;
    }

    fn mult(&mut self) {
        let l = self.fetch_ref(self.pos + 1);
        let r = self.fetch_ref(self.pos + 2);
        let dest_pos = self.tape[self.pos + 3] as usize;
        self.tape[dest_pos] = l * r;
        self.pos = self.pos + 4;
    }

    fn fetch_ref(&self, pos: usize) -> i32 {
        let ref_pos = self.tape[pos] as usize;
        self.tape[ref_pos]
    }

    fn is_exited(&self) -> bool {
          99 == self.cur_opcode()
    }

    fn cur_opcode(&self) -> i32 {
        self.tape[self.pos]
    }
}

fn read_tape(path: &str) -> Vec<i32> {
    let file = File::open(path).expect("couldn't open file");
    let mut buffered = BufReader::new(file);
    let mut line = String::new();
    buffered.read_line(&mut line).expect("no line?");

    line.split(",").map(|x| {
        let x: i32 = x.trim().parse().expect(&format!("'{}' not an int?", x));
        x
    }).collect()
}

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let tape = read_tape(&input);

    // p1 modification: replace pos 1 with val 12 and pos 2 and with value 2
    let mut tape_p1 = tape.clone();
    tape_p1[1] = 12;
    tape_p1[2] = 2;
    let mut machine_p1 = new(tape_p1);
    machine_p1.run_until_exit();
    println!("p1: pos 0 = {}", machine_p1.tape[0]);

    // p2: find input that terminates with tape[0] == 19690720
    // thankfully (I think) all of these *must* terminate, since the machine doesn't support `jmp`
    // instructions. So everything would either terminate or hit an invalid state.
    for noun in 0..99 {
        for verb in 0..99 {
            let mut tape_p2 = tape.clone();
            tape_p2[1] = noun;
            tape_p2[2] = verb;
            let mut machine_p2 = new(tape_p2);
            machine_p2.run_until_exit();

            if machine_p2.tape[0] == 19690720 {
                println!("p2: noun={}, verb={}, 100*noun+verb={}", noun, verb, (100 * noun) + verb);
                return;
            }
        }
    }
}
