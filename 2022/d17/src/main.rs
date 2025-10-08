use std::collections::VecDeque;
use std::env::args;
use std::fs;

const CHAMBER_WIDTH: usize = 7;
const NEW_ROCK_L_BUF: usize = 2;
const NEW_ROCK_V_BUF: usize = 3;

type Row = [bool; CHAMBER_WIDTH];

enum Block {
    Row,
    Cross,
    L,
    Col,
    Block,
}

impl Block {
    fn spawn(&self, max_y: usize) -> Vec<(usize, usize)> {
        match self {
            Self::Row => vec![
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF + 1, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF + 2, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF + 3, max_y + NEW_ROCK_V_BUF + 1),
            ],
            Self::Cross => vec![
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 2),
                (NEW_ROCK_L_BUF + 1, max_y + NEW_ROCK_V_BUF + 2),
                (NEW_ROCK_L_BUF + 2, max_y + NEW_ROCK_V_BUF + 2),
                (NEW_ROCK_L_BUF + 1, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF + 1, max_y + NEW_ROCK_V_BUF + 3),
            ],
            Self::L => vec![
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF + 1, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF + 2, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF + 2, max_y + NEW_ROCK_V_BUF + 2),
                (NEW_ROCK_L_BUF + 2, max_y + NEW_ROCK_V_BUF + 3),
            ],
            Self::Col => vec![
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 2),
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 3),
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 4),
            ],
            Self::Block => vec![
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF + 1, max_y + NEW_ROCK_V_BUF + 1),
                (NEW_ROCK_L_BUF, max_y + NEW_ROCK_V_BUF + 2),
                (NEW_ROCK_L_BUF + 1, max_y + NEW_ROCK_V_BUF + 2),
            ],
        }
    }

    fn next(&self) -> Block {
        match self {
            Self::Row => Self::Cross,
            Self::Cross => Self::L,
            Self::L => Self::Col,
            Self::Col => Self::Block,
            Self::Block => Self::Row,
        }
    }
}

fn parse_jets(input: &str) -> Vec<i8> {
    input
        .trim()
        .chars()
        .map(|c| match c {
            '>' => 1,
            '<' => -1,
            _ => panic!("other chars should have gotten stripped out"),
        })
        .collect()
}

struct Chamber {
    jets: Vec<i8>,
    jets_idx: usize,
    grid: VecDeque<Row>,
    next_block: Block,
}

impl Chamber {
    fn new(jets: Vec<i8>) -> Self {
        Chamber {
            jets,
            jets_idx: 0,
            grid: VecDeque::new(),
            next_block: Block::Row,
        }
    }

    fn drop_block(&mut self) {
        let max_y = self.grid.len();
        let mut blocks = self.next_block.spawn(max_y);
        let mut at_rest = false;

        while !at_rest {
            // jet shift
            let jet_shift = self.jets[self.jets_idx];
            self.jets_idx += (self.jets_idx + 1) % self.jets.len();
            // TODO probably need to check actual blocks in grid, not just chamber walls
            if (jet_shift == 1 && !blocks.iter().any(|(x,_y) x == CHAMBER_WIDTH - 1)) ||
                (jet_shift == -1 && !blocks.iter().any(|(x,_y) x == 0))) {

            }

            // fall
        }

        // TODO add rows as needed
        self.next_block = self.next_block.next();
    }
}

fn main() {
    let mut args = args();
    args.next();
    let input_path = args.next().expect("input filename expected");
    let input = fs::read_to_string(input_path).expect("read the file");
    let jets = parse_jets(&input);

    let mut chamber = Chamber::new(jets);
    chamber.drop_block();
}
