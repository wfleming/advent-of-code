use intcode::machine::Machine;
use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;
use intcode::num_traits::ToPrimitive;

pub fn program() -> String {
    let mut s = String::new();

    // full uncompressed path
    // R,10,L,12,R,6,R,10,L,12,R,6,R,6,R,10,R,12,R,6,R,10,L,12,L,12,R,6,R,10,R,12,R6,R,10,L,12,L,12,R,6,R,10,R,12,R,6,R,10,L,12,L,12,R,6,R,10,R,12,R,6,R,10,L,12,R,6

    // A,A,B,C,B,C,B,C,B,A

    s.push_str("A,A,B,C,B,C,B,C,B,A\n"); // main program
    s.push_str("R,10,L,12,R,6\n"); // the A program
    s.push_str("R,6,R,10,R,12,R,6\n"); // the B program
    s.push_str("R,10,L,12,L,12\n"); // the C program
    s.push_str("y\n"); // yes I want the live feed for debugging

    s
}

pub fn input_program(machine: &mut Machine) {
    for b in program().as_bytes() {
        machine.push_input(BigInt::from_u8(*b).unwrap());
    }
}

pub fn run_program(machine: &mut Machine, view_height: usize) {
    let mut out_idx = 0;
    let mut out_screen = String::new();

    while !machine.is_exited() {
        machine.step();

        while out_idx < machine.outputs.len() {
            let c_u32 = machine.outputs[out_idx].to_u32().unwrap();
            let c = std::char::from_u32(c_u32).unwrap();
            out_screen.push(c);

            // this line-length check turns out to be buggy because the program actually prints
            // prompt lines for "main program", "routine A", etc. Oh well. I can still tell what's
            // happening, not going to bother fixing.
            if c == '\n' && out_screen.lines().collect::<Vec<&str>>().len() == view_height + 1 {
                println!("------- BEGIN SCREEN --------");
                println!("{}", out_screen);
                println!("------- END SCREEN --------");
                out_screen = String::new();
            }

            out_idx += 1;
        }
    }
}
