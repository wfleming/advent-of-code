use intcode::tape;
use intcode::machine::Machine;
use intcode::num_traits::ToPrimitive;
use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;
use std::env::args;
use std::fs;

fn run_programs(intcode_tape: &tape::Tape, botcode: &str) {
  let mut m = Machine::new(intcode_tape.clone());

  botcode.chars().for_each(|c| m.push_input(BigInt::from_u32(c as u32).unwrap()));

  let mut os = m.outputs.len();
  while !m.is_exited() {
      m.run_until_exit_or_output();
      for i in os..m.outputs.len() {
          let o = m.outputs[i].to_u32().expect("should be in u32 range");
          if o < 255 {
              print!("{}", (o as u8) as char);
          } else {
              println!("\n\nhull damage is {}", o);
          }
      }
      os = m.outputs.len();
  }
}

fn main() {
    let mut argv = args();
    argv.next();
    let intcode_path = argv.next().expect("provide a filename for the intcode input");
    let botcode_path = argv.next().expect("provide a filename for the botcode input");

    let tape = tape::from_file(&intcode_path);
    let botcode = fs::read_to_string(botcode_path).expect("File not readable");

    run_programs(&tape, &botcode);
}
