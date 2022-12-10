use std::env::args;
use std::fs;

#[derive(Debug, Clone, Copy)]
enum Instr {
    Noop,
    Addx(i32),
}

impl Instr {
    pub fn parse(s: &str) -> Instr {
        let pieces: Vec<&str> = s.split(' ').collect();
        match pieces[0] {
            "noop" => Self::Noop,
            "addx" => Self::Addx(pieces[1].parse::<i32>().unwrap()),
            _ => panic!("Invalid instruction"),
        }
    }

    pub fn cycle_cost(&self) -> u8 {
        match self {
            Self::Noop => 1,
            Self::Addx(_) => 2,
        }
    }
}

struct CPU {
    instrs: Vec<Instr>,
    x: i32,
    cycles_elapsed: u32,
    ip: usize,
    instr_cycles_left: u8,
}

impl CPU {
    pub fn new(instrs: Vec<Instr>) -> CPU {
        let initial_cycles_left = instrs[0].cycle_cost();
        CPU {
            instrs: instrs,
            x: 1,
            cycles_elapsed: 0,
            ip: 0,
            instr_cycles_left: initial_cycles_left,
        }
    }

    pub fn step(&mut self) {
        if self.ip >= self.instrs.len() {
            panic!("no more instructions");
        }

        if self.instr_cycles_left > 0 {
            // println!("DEBUG: pause cycle={}->{} for instr {:?}", self.cycles_elapsed, self.cycles_elapsed+1, self.instrs[self.ip]);
            self.instr_cycles_left -= 1;
            self.cycles_elapsed += 1;
        } else {
            // println!("DEBUG: executing instr {:?} after cycle {}", self.instrs[self.ip], self.cycles_elapsed);
            let instr = self.instrs[self.ip];
            self.exec_instr(&instr);
            self.ip += 1;
            if self.ip < self.instrs.len() {
                self.instr_cycles_left = self.instrs[self.ip].cycle_cost();
            }
        }
    }

    pub fn exec_instr(&mut self, i: &Instr) {
        match i {
            Instr::Noop => (),
            Instr::Addx(x) => self.x += x,
        }
    }
}

const TRACK_CYCLES: [u32; 6] = [20, 60, 100, 140, 180, 220];
fn p1(instrs: &Vec<Instr>) {
    let mut cpu = CPU::new(instrs.clone());
    let mut next_cycle_idx = 0;
    let mut signal_strength_accum: i32 = 0;

    while cpu.cycles_elapsed < TRACK_CYCLES[TRACK_CYCLES.len() - 1] {
        cpu.step();
        if cpu.cycles_elapsed == TRACK_CYCLES[next_cycle_idx] {
            let signal_strength = (TRACK_CYCLES[next_cycle_idx] as i32) * cpu.x;
            signal_strength_accum += signal_strength;
            println!("p1: at cycle {} about to exec {}: {:?} reg x has value {} signal strength right now is {}", TRACK_CYCLES[next_cycle_idx], cpu.ip + 1, instrs[cpu.ip], cpu.x, signal_strength);
            next_cycle_idx += 1;
        }
    }

    println!(
        "p1 total accumulated signal strength is {}",
        signal_strength_accum
    );
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let instrs: Vec<Instr> = fs::read_to_string(input_path)
        .expect("read the file")
        .lines()
        .map(|l| Instr::parse(l))
        .collect();

    p1(&instrs);
}
