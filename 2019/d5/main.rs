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
    inputs: Vec<i32>,
    outputs: Vec<i32>,
}

fn new(tape: Vec<i32>) -> IntcodeMachine {
    IntcodeMachine {
        tape: tape,
        pos: 0,
        inputs: Vec::new(),
        outputs: Vec::new(),
    }
}

impl IntcodeMachine {
    fn push_input(&mut self, v: i32) {
        self.inputs.push(v);
    }

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
            3 => self.read_input(),
            4 => self.write_output(),
            99 => (), // halt
            _ => panic!("bad machine state, invalid opcode from '{}'", self.tape[self.pos]),
        };
    }

    fn add(&mut self) {
        let l = self.nth_param(1);
        let r = self.nth_param(2);
        let dest_pos = self.tape[self.pos + 3] as usize;
        self.tape[dest_pos] = l + r;
        self.pos = self.pos + 4;
    }

    fn mult(&mut self) {
        let l = self.nth_param(1);
        let r = self.nth_param(2);
        let dest_pos = self.tape[self.pos + 3] as usize;
        self.tape[dest_pos] = l * r;
        self.pos = self.pos + 4;
    }

    fn read_input(&mut self) {
        let dest_pos = self.tape[self.pos + 3] as usize;
        self.tape[dest_pos] = self.inputs.pop().expect("input should be non-empty");
        self.pos += 2;
    }

    fn write_output(&mut self) {
        self.outputs.push(self.nth_param(1));
        self.pos += 2;
    }

    fn nth_param(&self, n: u32) -> i32 {
        self.value_at(
            self.pos as usize + n as usize,
            self.nth_param_mode(n),
        )
    }

    fn value_at(&self, pos: usize, mode: i32) -> i32 {
        self.value(self.tape[pos], mode)
    }

    fn value(&self, v: i32, mode: i32) -> i32 {
        match mode {
            0 => self.tape[v as usize],
            1 => v,
            _ => panic!("invalid mode"),
        }
    }

    fn is_exited(&self) -> bool {
          99 == self.cur_opcode()
    }

    fn nth_param_mode(&self, n: u32) -> i32 {
        assert!(n >= 1 && n <=3);
        let div = (10 as i32).pow(n + 1);
        (self.tape[self.pos] / div) % 10
    }

    fn cur_opcode(&self) -> i32 {
        // each digit in the tape entry is (comma sep):
        // p3mode,p2mode,p1mode,0,opcode
        self.tape[self.pos] % 100
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

    // p1 - intput 1, run to halt, show outputs
    let tape_p1 = tape.clone();
    let mut machine_p1 = new(tape_p1);
    machine_p1.push_input(1);
    machine_p1.run_until_exit();
    println!("p1: outputs = {:?}", machine_p1.outputs);
}

mod test {
    use super::*;
}
