use intcode::num_traits::cast::ToPrimitive;
use intcode::machine::Machine;
use std::collections::HashMap;

pub type Point = (i64, i64);

#[derive(PartialEq)]
#[derive(Eq)]
pub enum Entity {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball
}

pub struct Game {
    pub tiles: HashMap<Point, Entity>,
}

impl Game {
    pub fn new() -> Game {
        Game {
            tiles: HashMap::new(),
        }
    }

    pub fn run(&mut self, machine: &mut Machine) {
        let mut output_idx: usize = 0;

        while !machine.is_exited() {
            if machine.is_waiting_for_input() {
                panic!("machine is waiting for input but there's nothing to give it");
            }

            machine.step();

            // read output (comes in blocks of 3, so wait to be 3 behind to process
            while output_idx + 2 < machine.outputs.len() {
                let x = machine.outputs.get(output_idx).unwrap().to_i64().unwrap();
                let y = machine.outputs.get(output_idx + 1).unwrap().to_i64().unwrap();
                let entity_code = machine.outputs.get(output_idx + 2).unwrap().to_u32().unwrap();
                let entity = Game::entity_from_code(entity_code);

                self.tiles.insert((x, y), entity);

                output_idx += 3;
            }
        }
    }

    fn entity_from_code(code: u32) -> Entity {
        match code {
            0 => Entity::Empty,
            1 => Entity::Wall,
            2 => Entity::Block,
            3 => Entity::Paddle,
            4 => Entity::Ball,
            _ => panic!("{} is not a valid entity code", code),
        }
    }
}

