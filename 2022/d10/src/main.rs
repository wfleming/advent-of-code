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

struct Cpu {
    instrs: Vec<Instr>,
    x: i32,
    cycles_elapsed: u32,
    ip: usize,
    instr_cycles_left: u8,
}

impl Cpu {
    pub fn new(instrs: Vec<Instr>) -> Cpu {
        let initial_cycles_left = instrs[0].cycle_cost();
        Cpu {
            instrs,
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
            // println!("DEBUG: pause cycle={}->{} for instr {:?}", self.cycles_elapsed, self.cycles_elapsed+1, self.cur_instr());
            self.instr_cycles_left -= 1;
            self.cycles_elapsed += 1;
        } else {
            // println!("DEBUG: executing instr {:?} after cycle {}", self.cur_instr(), self.cycles_elapsed);
            let instr = self.cur_instr();
            self.exec_instr(&instr);
            self.ip += 1;
            if self.ip < self.instrs.len() {
                self.instr_cycles_left = self.cur_instr().cycle_cost();
            }
        }
    }

    pub fn cur_instr(&self) -> Instr {
        self.instrs[self.ip]
    }

    fn exec_instr(&mut self, i: &Instr) {
        match i {
            Instr::Noop => (),
            Instr::Addx(x) => self.x += x,
        }
    }
}

const TRACK_CYCLES: [u32; 6] = [20, 60, 100, 140, 180, 220];
const ROW_PIXELS: u32 = 40;
const ROW_COUNT: u32 = 6;
fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let instrs: Vec<Instr> = fs::read_to_string(input_path)
        .expect("read the file")
        .lines()
        .map(Instr::parse)
        .collect();

    let mut cpu = Cpu::new(instrs);
    let mut signal_strength_accum: i32 = 0;
    let mut screen = String::with_capacity((ROW_PIXELS * ROW_COUNT) as usize);

    while cpu.cycles_elapsed < ROW_PIXELS * ROW_COUNT {
        let screen_row_x: i32 = (cpu.cycles_elapsed % ROW_PIXELS) as i32;
        cpu.step();

        // p1 signal logic
        if TRACK_CYCLES.contains(&cpu.cycles_elapsed) {
            let signal_strength = cpu.cycles_elapsed as i32 * cpu.x;
            signal_strength_accum += signal_strength;
            println!("p1: at cycle {} about to exec {}: {:?} reg x has value {} signal strength right now is {}", cpu.cycles_elapsed, cpu.ip + 1, cpu.cur_instr(), cpu.x, signal_strength);
        }
        if cpu.cycles_elapsed == TRACK_CYCLES[TRACK_CYCLES.len() - 1] {
            println!(
                "p1: total accumulated signal strength is {}",
                signal_strength_accum
            );
        }

        // p2 crt logic
        if cpu.x >= screen_row_x - 1 && cpu.x <= screen_row_x + 1 {
            screen.push('#');
        } else {
            screen.push(' '); // '.' is easier for debugging, but this is easier for reading output
        }
        if screen_row_x == (ROW_PIXELS - 1) as i32 {
            screen.push('\n');
        }

        // execute the actual instruction in the loop since we depend on cycles_elapsed to draw the
        // crt, and it doesn't change when the instruction actually happens
        if cpu.instr_cycles_left == 0 {
            cpu.step();
        }
    }

    println!("p2:\n{}", screen);
}
