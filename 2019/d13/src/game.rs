use intcode::machine::Machine;
use intcode::num_bigint::BigInt;
use intcode::num_traits::cast::FromPrimitive;
use intcode::num_traits::cast::ToPrimitive;
use std::collections::HashMap;
use std::thread::sleep;
use std::time::Duration;

pub type Point = (i64, i64);

#[derive(PartialEq, Eq)]
pub enum Entity {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl Entity {
    fn from_code(code: u32) -> Entity {
        match code {
            0 => Self::Empty,
            1 => Self::Wall,
            2 => Self::Block,
            3 => Self::Paddle,
            4 => Self::Ball,
            _ => panic!("{} is not a valid entity code", code),
        }
    }

    fn char(&self) -> char {
        match self {
            Self::Empty => ' ',
            Self::Wall => '|',
            Self::Block => '#',
            Self::Paddle => '-',
            Self::Ball => 'o',
        }
    }
}

pub struct Game {
    pub tiles: HashMap<Point, Entity>,
    pub score: u32,
    pub joystick: i32,
}

impl Game {
    pub fn new() -> Game {
        Game {
            tiles: HashMap::new(),
            score: 0,
            joystick: 0,
        }
    }

    pub fn run(&mut self, machine: &mut Machine) {
        let mut output_idx: usize = 0;

        while !machine.is_exited() {
            if machine.is_waiting_for_input() {
                machine.push_input(BigInt::from_i32(self.joystick).unwrap());

                // only displaying when paused for input means faster rendering
                if self.tiles.len() > 0 {
                    println!("{}", self.display());
                }
                // sleep(Duration::from_millis(500)); //DEBUG - watch the game
            }

            machine.step();

            // read output (comes in blocks of 3, so wait to be 3 behind to process)
            while output_idx + 2 < machine.outputs.len() {
                let x = machine.outputs.get(output_idx).unwrap().to_i64().unwrap();
                let y = machine
                    .outputs
                    .get(output_idx + 1)
                    .unwrap()
                    .to_i64()
                    .unwrap();

                if x == -1 && y == 0 {
                    // score, not tile
                    let score = machine
                        .outputs
                        .get(output_idx + 2)
                        .unwrap()
                        .to_u32()
                        .unwrap();
                    self.score = score;
                } else {
                    // normal tile
                    let entity_code = machine
                        .outputs
                        .get(output_idx + 2)
                        .unwrap()
                        .to_u32()
                        .unwrap();
                    let entity = Entity::from_code(entity_code);

                    self.tiles.insert((x, y), entity);
                }

                output_idx += 3;
            }

            // move the joystick if needed
            let bpos = self.ball_pos();
            let ppos = self.paddle_pos();
            if bpos.is_some() && ppos.is_some() {
                let bx = bpos.unwrap().0;
                let px = ppos.unwrap().0;

                if px < bx {
                    self.joystick = 1;
                } else if px > bx {
                    self.joystick = -1;
                } else {
                    self.joystick = 0;
                }
            }
        }

        if self.tiles.len() > 0 {
            println!("{}", self.display());
        }
    }

    fn ball_pos(&self) -> Option<Point> {
        for (pt, kind) in &self.tiles {
            if kind == &Entity::Ball {
                return Option::Some(*pt);
            }
        }

        Option::None
    }

    fn paddle_pos(&self) -> Option<Point> {
        for (pt, kind) in &self.tiles {
            if kind == &Entity::Paddle {
                return Option::Some(*pt);
            }
        }

        Option::None
    }

    pub fn display(&self) -> String {
        // x goes left -> right, y goes top -> bottom

        let mut max_x = i64::MIN;
        let mut max_y = i64::MIN;

        for point in self.tiles.keys() {
            if point.0 > max_x {
                max_x = point.0
            }
            if point.1 > max_y {
                max_y = point.1
            }
        }

        let mut s = String::new();
        s.reserve((max_x * max_y) as usize);

        let sep_line = format!("{}\n", "=".repeat((max_x + 1) as usize));

        // insert a line at the top, then print the score
        s.push_str(&sep_line);
        s.push_str(&format!("score: {}\n", self.score));

        for y in (0..(max_y + 1)).rev() {
            for x in 0..(max_x + 1) {
                let entity = self.tiles.get(&(x, y)).unwrap_or(&Entity::Empty);
                s.push(entity.char());
            }
            s.push('\n');
        }

        // insert a line at the bottom
        s.push_str(&sep_line);

        s
    }
}
