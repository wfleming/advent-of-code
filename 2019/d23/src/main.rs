use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::{FromPrimitive, ToPrimitive};
use intcode::num_traits::Zero;
use intcode::{machine::Machine, tape};
use std::cell::RefCell;
use std::env::args;

fn nat_address() -> BigInt { BigInt::from_i32(255).unwrap() }
fn filler_input() -> BigInt { BigInt::from_i32(-1).unwrap() }

fn construct_computers(instrs: &tape::Tape) -> Vec<RefCell<Machine>> {
    let mut computers = Vec::with_capacity(50);
    for i in 0..50 {
        let mut m = Machine::new(instrs.clone());
        m.push_input(BigInt::from_usize(i).unwrap());
        computers.push(RefCell::new(m));
    }

    computers
}

fn simulate_computers_p1(instrs: &tape::Tape) -> i32 {
    let computers = construct_computers(instrs);

    loop {
        for cell in &computers {
            if cell.borrow().is_exited() {
                panic!("I didn't expect a machine to exit");
            }

            if cell.borrow().is_waiting_for_input() {
                cell.borrow_mut().push_input(filler_input());
            }

            let oc = cell.borrow().outputs.len();
            cell.borrow_mut().step();
            if cell.borrow().outputs.len() > oc {
                // a packet started, simulate this machine until it finishes the packet
                while cell.borrow().outputs.len() < oc + 3 {
                    if cell.borrow().is_waiting_for_input() {
                        panic!("I didn't expect a machine to ask for input in the middle of emitting a packet");
                    }
                    cell.borrow_mut().step();
                }

                let oc = cell.borrow().outputs.len();
                let (a, x, y) = (cell.borrow().outputs[oc - 3].clone(), cell.borrow().outputs[oc - 2].clone(), cell.borrow().outputs[oc - 1].clone());
                if a == nat_address() { // p1 solution
                    return y.to_i32().unwrap();
                } else {
                    computers[a.to_usize().unwrap()].borrow_mut().push_input(x);
                    computers[a.to_usize().unwrap()].borrow_mut().push_input(y);
                }
            }
        }
    }
}

fn simulate_computers_p2(instrs: &tape::Tape) -> i32 {
    let computers = construct_computers(instrs);
    let mut nat_packet: (BigInt, BigInt) = (Zero::zero(), Zero::zero());
    let mut last_nat_y: BigInt = Zero::zero();

    loop {
        for cell in &computers {
            if cell.borrow().is_exited() {
                panic!("I didn't expect a machine to exit");
            }

            if cell.borrow().is_waiting_for_input() {
                cell.borrow_mut().push_input(filler_input());
            }

            let oc = cell.borrow().outputs.len();
            cell.borrow_mut().step();
            if cell.borrow().outputs.len() > oc {
                // a packet started, simulate this machine until it finishes the packet
                while cell.borrow().outputs.len() < oc + 3 {
                    if cell.borrow().is_waiting_for_input() {
                        panic!("I didn't expect a machine to ask for input in the middle of emitting a packet");
                    }
                    cell.borrow_mut().step();
                }

                let oc = cell.borrow().outputs.len();
                let (a, x, y) = (cell.borrow().outputs[oc - 3].clone(), cell.borrow().outputs[oc - 2].clone(), cell.borrow().outputs[oc - 1].clone());
                if a == nat_address() {
                    if y == last_nat_y { // p2 answer
                        return y.to_i32().unwrap();
                    }
                    nat_packet = (x,y)
                } else {
                    computers[a.to_usize().unwrap()].borrow_mut().push_input(x);
                    computers[a.to_usize().unwrap()].borrow_mut().push_input(y);
                }
            }
        }

        let all_idle = computers.iter().all(|cell| cell.borrow().is_waiting_for_input() || cell.borrow().inputs.iter().all(|i| i == &filler_input()));
        if all_idle { // send the nat packet to address 0
            last_nat_y = nat_packet.1.clone();
            computers[0].borrow_mut().push_input(nat_packet.0.clone());
            computers[0].borrow_mut().push_input(nat_packet.1.clone());
        }
    }
}

fn main() {
    let mut argv = args();
    argv.next();
    let input = argv.next().expect("provide a filename");
    let instrs = tape::from_file(&input);

    let p1 = simulate_computers_p1(&instrs);
    println!("p1: {}", p1);
    let p2 = simulate_computers_p2(&instrs);
    println!("p2: {}", p2);
}
