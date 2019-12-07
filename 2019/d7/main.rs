use std::env::args;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::ops::Range;
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
        }
    }

    fn run_until_exit_or_input_wait(&mut self) {
        loop {
            if self.is_exited() {
                break;
            }
            if self.cur_opcode() == 3 && self.inputs.len() == 0 {
                break;
            }
            self.step();
        }
    }


    fn step(&mut self) {
        // println!("DEBUG: step state={:?} opcode={} from tape={}", self, self.cur_opcode(), self.tape[self.pos]);
        match self.cur_opcode() {
            1 => self.add(),
            2 => self.mult(),
            3 => self.read_input(),
            4 => self.write_output(),
            5 => self.jmp_if_true(),
            6 => self.jmp_if_false(),
            7 => self.less_than(),
            8 => self.equals(),
            99 => (), // halt
            _ => panic!("bad machine state, invalid opcode from '{}'", self.tape[self.pos]),
        };
    }

    fn add(&mut self) {
        let l = self.nth_param(1);
        let r = self.nth_param(2);
        let dest_pos = self.tape[self.pos + 3] as usize;
        // println!("DEBUG: add {}, {} -> {}", l, r, dest_pos);
        self.tape[dest_pos] = l + r;
        self.pos = self.pos + 4;
    }

    fn mult(&mut self) {
        let l = self.nth_param(1);
        let r = self.nth_param(2);
        let dest_pos = self.tape[self.pos + 3] as usize;
        // println!("DEBUG: mult {}, {} -> {}", l, r, dest_pos);
        self.tape[dest_pos] = l * r;
        self.pos = self.pos + 4;
    }

    fn read_input(&mut self) {
        let dest_pos = self.tape[self.pos + 1] as usize;
        // println!("DEBUG: machine reading inputs. input queue={:?}", self.inputs);
        self.tape[dest_pos] = self.inputs.remove(0);
        // println!("DEBUG: machine reading inputs. got dest_pos={}, input={}, full tape={:?}", dest_pos, self.tape[dest_pos], self.tape);
        self.pos += 2;
    }

    fn write_output(&mut self) {
        let o = self.nth_param(1);
        // println!("DEBUG: output {}", o);
        self.outputs.push(o);
        self.pos += 2;
    }

    fn jmp_if_true(&mut self) {
        if self.nth_param(1) != 0 {
            self.pos = self.nth_param(2) as usize;
        } else {
            self.pos += 3;
        }
    }

    fn jmp_if_false(&mut self) {
        if self.nth_param(1) == 0 {
            self.pos = self.nth_param(2) as usize;
        } else {
            self.pos += 3;
        }
    }

    fn less_than(&mut self) {
        let target: usize = self.tape[self.pos + 3] as usize;
        if self.nth_param(1) < self.nth_param(2) {
            self.tape[target] = 1;
        } else {
            self.tape[target] = 0;
        }
        self.pos += 4;
    }

    fn equals(&mut self) {
        let target: usize = self.tape[self.pos + 3] as usize;
        if self.nth_param(1) == self.nth_param(2) {
            self.tape[target] = 1;
        } else {
            self.tape[target] = 0;
        }
        self.pos += 4;
    }

    fn nth_param(&self, n: u32) -> i32 {
        self.value_at(
            self.pos as usize + n as usize,
            self.nth_param_mode(n),
        )
    }

    fn value_at(&self, pos: usize, mode: i32) -> i32 {
        // println!("DEBUG machine value_at pos={} mode={} tape[pos]={}, v={}", pos, mode, self.tape[pos], self.value(self.tape[pos], mode));
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

fn tape_from_file(path: &str) -> String {
    let file = File::open(path).expect("couldn't open file");
    let mut buffered = BufReader::new(file);
    let mut line = String::new();
    buffered.read_line(&mut line).expect("no line?");

    line
}

fn read_tape(line: &str) -> Vec<i32> {
    line.split(",").map(|x| {
        let x: i32 = x.trim().parse().expect(&format!("'{}' not an int?", x));
        x
    }).collect()
}

fn input_choices(range: Range<i32>) -> Vec<Vec<i32>> {
    let mut choices = Vec::new();

    for phase_a in range.clone() {
        choices.append(&mut build_choices(vec![phase_a], range.clone()));
    }

    choices
}

fn build_choices(phases: Vec<i32>, range: Range<i32>) -> Vec<Vec<i32>> {
    if phases.len() == 5 {
        vec![phases]
    } else {
        range.clone().filter(|x| !phases.contains(x))
            .flat_map(|x| {
                let mut phases2 = phases.clone();
                phases2.push(x);
                build_choices(phases2, range.clone())
            }).collect()
    }
}

fn thrust_signal(phases: &Vec<i32>, tape: &Vec<i32>) -> i32 {
    // also could be considered "last amp output"
    let mut next_amp_input = 0;

    for phase in phases.iter() {
        let mut machine = new(tape.clone());
        machine.push_input(*phase);
        machine.push_input(next_amp_input);
        machine.run_until_exit();

        // println!("DEBUG: ran machine with phase={}, last_amp={}, output={:?}", phase, next_amp_input, machine.outputs);
        next_amp_input = *machine.outputs.last().expect("machine should output stuff");
    }

    // the output of the final amp becomes the input to the thrusters
    next_amp_input
}

fn thrust_signal_feedback_loop(phases: &Vec<i32>, tape: &Vec<i32>) -> i32 {
    let mut amps = vec![
        new(tape.clone()), // a
        new(tape.clone()), // b
        new(tape.clone()), // c
        new(tape.clone()), // d
        new(tape.clone()), // e
    ];

    for i in 0..5 {
        amps[i].push_input(phases[i]);
    }
    amps[0].push_input(0); // "seed" previous amp value

    let mut running_amp = 0;
    while amps.iter().any(|a| !a.is_exited()) {
        amps[running_amp].run_until_exit_or_input_wait();
        let out = *amps[running_amp].outputs.last().expect("should be some output");
        running_amp = (running_amp + 1) % amps.len();
        amps[running_amp].push_input(out);
    }

    *amps[4].outputs.last().expect("should be some output")
}

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let tape = read_tape(&tape_from_file(&input));

    // p1
    let all_phases = input_choices(0..5);
    // println!("DEBUG: all input choices = {:?}", all_phases);
    let mut best_phases: Option<&Vec<i32>> = None;
    let mut best_signal = 0;
    for phases in all_phases.iter() {
        let s = thrust_signal(&phases, &tape);
        if s > best_signal {
            best_phases = Some(&phases);
            best_signal = s;
        }
    }
    println!("p1: best phases = {:?}, max signal = {}", best_phases, best_signal);

    // p2
    let all_phases = input_choices(5..10);
    let mut best_phases: Option<&Vec<i32>> = None;
    let mut best_signal = 0;
    for phases in all_phases.iter() {
        let s = thrust_signal_feedback_loop(&phases, &tape);
        if s > best_signal {
            best_phases = Some(&phases);
            best_signal = s;
        }
    }
    println!("p2: best phases = {:?}, max signal = {}", best_phases, best_signal);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_p1_sample_1_simple() {
        // I think this program = x + (y * 10)
        let tape = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
        let tape = read_tape(&tape);

        let mut machine = new(tape.clone());
        machine.push_input(1);
        machine.push_input(2);
        machine.run_until_exit();

        assert_eq!(21, *machine.outputs.first().expect("should output"));
    }

    #[test]
    fn test_p1_sample_1_phases_43210() {
        let tape = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
        let tape = read_tape(&tape);

        let signal = thrust_signal(&vec![4, 3, 2, 1, 0], &tape);

        assert_eq!(43210, signal);
    }

    #[test]
    fn test_p2_sample_1() {
        let tape = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
        let tape = read_tape(&tape);

        let signal = thrust_signal_feedback_loop(&vec![9, 8, 7, 6, 5], &tape);

        assert_eq!(139629729, signal);
    }
}
