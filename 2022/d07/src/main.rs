use std::cell::RefCell;
use std::env::args;
use std::fs;
use std::rc::Rc;

#[derive(PartialEq)]
enum Entry {
    Dir {
        name: String,
        children: Vec<Rc<RefCell<Entry>>>,
        parent: Option<Rc<RefCell<Self>>>,
    },
    File {
        name: String,
        size: u32,
    },
}

// default impl is bad for self-referencing structs with circular refs (i.e. parent -> children ->
// parent)
impl std::fmt::Debug for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        let s = match self {
            Entry::Dir { name, parent, .. } => format!("Dir {{ name: {name}, parent: {parent:?}}}"),
            Entry::File { name, size } => format!("File {{ name: {name}, size: {size}}}"),
        };

        write!(f, "{}", s)
    }
}

impl Entry {
    pub fn tot_size(&self) -> u32 {
        match self {
            Self::Dir { children, .. } => children.iter().map(|e| e.borrow().tot_size()).sum(),
            Self::File { size, .. } => *size,
        }
    }

    pub fn eql_dir(&self, other_name: &str) -> bool {
        match self {
            Self::Dir { name, .. } => name == other_name,
            _ => false,
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            Self::Dir { name, .. } => name,
            Self::File { name, .. } => name,
        }
    }

    // only call this on dirs. Convenience to avoid match cruft elsewhere.
    pub fn get_parent(&self) -> Rc<RefCell<Entry>> {
        match self {
            Self::Dir { parent, .. } => parent.as_ref().expect("don't go up from root").clone(),
            _ => panic!("don't call get_parent on Files"),
        }
    }

    // only call this on dirs. Convenience to avoid match cruft elsewhere.
    pub fn get_children(&self) -> Vec<Rc<RefCell<Entry>>> {
        match self {
            Self::Dir { children, .. } => children.to_vec(),
            _ => panic!("don't call get_children on Files"),
        }
    }

    // only call this on dirs. Convenience to avoid match cruft elsewhere.
    pub fn add_child(&mut self, child: Entry) {
        match self {
            Self::Dir { children, .. } => children.push(Rc::new(RefCell::new(child))),
            _ => panic!("don't call add_child on Files"),
        }
    }
}

// walk the tree, flatten to just directories
fn all_dirs(entry: Rc<RefCell<Entry>>) -> Vec<Rc<RefCell<Entry>>> {
    let mut r = vec![];

    match &*entry.borrow() {
        Entry::Dir { children, .. } => {
            r.push(entry.clone());
            for c in children {
                r.append(&mut all_dirs(c.clone()));
            }
        }
        _ => (),
    }

    r
}

fn simulate_terminal(term: &str) -> Rc<RefCell<Entry>> {
    let root = Rc::new(RefCell::new(Entry::Dir {
        name: "/".to_string(),
        children: vec![],
        parent: Option::None,
    }));
    let mut cur_dir = root.clone();

    let mut lines = term.lines().peekable();
    let mut line = lines.next();

    while line != Option::None {
        let line_s = line.unwrap();
        if line_s.starts_with("$") {
            let cmd = &line_s[2..];
            if cmd == "ls" {
                while lines.peek().is_some() && !lines.peek().unwrap().starts_with("$") {
                    let line = lines.next();
                    let entry_pieces: Vec<&str> = line.unwrap().split(" ").collect();
                    let e = match entry_pieces[0] {
                        "dir" => Entry::Dir {
                            name: entry_pieces[1].to_string(),
                            children: vec![],
                            parent: Option::Some(cur_dir.clone()),
                        },
                        _ => {
                            let size = entry_pieces[0].parse::<u32>().expect("size is first col");
                            Entry::File {
                                name: entry_pieces[1].to_string(),
                                size: size,
                            }
                        }
                    };
                    cur_dir.borrow_mut().add_child(e);
                }
            } else if cmd.starts_with("cd ") {
                let dir = &cmd[3..];
                if dir == "/" {
                    cur_dir = root.clone();
                } else if dir == ".." {
                    let next_dir = cur_dir.borrow().get_parent().clone();
                    cur_dir = next_dir;
                } else {
                    // if we already know about the child, go there, otherwise create it
                    let next_dir = cur_dir
                        .borrow()
                        .get_children()
                        .iter()
                        .find(|e| e.borrow().eql_dir(dir))
                        .expect("child should exist: always ls before cd")
                        .clone();
                    cur_dir = next_dir;
                }
            } else {
                panic!("unexpected command");
            }
            line = lines.next();
        } else {
            panic!("Unexpected line in outer loop");
        }
    }

    root
}

fn p1(fs: Rc<RefCell<Entry>>) -> u32 {
    all_dirs(fs)
        .iter()
        .map(|e| e.borrow().tot_size())
        .filter(|s| *s <= 100_000)
        .sum()
}

const CAPACITY: u32 = 70_000_000;
const SPACE_NEEDED: u32 = 30_000_000;

fn p2(fs: Rc<RefCell<Entry>>) -> u32 {
    let cur_used = fs.borrow().tot_size();
    let cur_free = CAPACITY - cur_used;
    let needed = SPACE_NEEDED - cur_free;

    all_dirs(fs)
        .iter()
        .filter(|e| e.borrow().tot_size() >= needed)
        .min_by(|a, b| a.borrow().tot_size().cmp(&b.borrow().tot_size()))
        .expect("should find some deletable dirs")
        .borrow()
        .tot_size()
}

fn main() {
    let mut argv = args();
    argv.next();
    let input_path = argv.next().expect("provide a filename");
    let input = fs::read_to_string(input_path)
        .expect("read the input")
        .trim()
        .to_string();
    let fs = simulate_terminal(&input);

    println!("p1: total size of dirs < 100,000: {}", p1(fs.clone()));
    println!("p2: size of dir to delete: {}", p2(fs.clone()));
}

#[cfg(test)]
mod tests {
    use super::*;

    const sample_1: &str = "$ cd /\n\
        $ ls\n\
        dir a\n\
        123 a.txt";

    const sample_2: &str = "$ cd /\n\
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    #[test]
    fn test_sim_1() {
        let r = simulate_terminal(sample_1);
        assert_eq!(r.borrow().tot_size(), 123);
    }

    #[test]
    fn test_all_dirs_1() {
        let r = simulate_terminal(sample_2);
        let dir_names: Vec<String> = all_dirs(r)
            .iter()
            .map(|d| d.borrow().get_name().to_string())
            .collect();
        assert_eq!(dir_names, vec!["/", "a", "e", "d"]);
    }
}
