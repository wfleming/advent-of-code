use std::env::args;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::vec::Vec;
use trees::{tr, Node, Tree};

fn read_orbits(path: &str) -> Vec<(String, String)> {
    let file = File::open(path).expect("couldn't open file");
    let buffered = BufReader::new(file);

    buffered
        .lines()
        .map(|l| {
            let l = l.expect("should have a line");
            let pieces: Vec<&str> = l.split(")").map(|p| p.trim()).collect();
            assert!(pieces.len() == 2, "each line should have exactly 2 objects");
            (pieces[0].to_string(), pieces[1].to_string())
        })
        .collect()
}

fn build_orbit_tree(orbits: &Vec<(String, String)>) -> Tree<String> {
    // COM is known to be the root
    let mut root = tr("COM".to_string());

    add_orbits(&mut root, orbits);

    root
}

// add the children for a given node, recursing for each child
fn add_orbits(parent: &mut Tree<String>, orbits: &Vec<(String, String)>) {
    let children_names = orbits
        .iter()
        .filter(|orbit| parent.data == orbit.0)
        .map(|orbit| orbit.1.clone())
        .collect::<Vec<String>>();

    for name in children_names.iter() {
        let mut new_node = tr(name.clone());
        add_orbits(&mut new_node, orbits);
        parent.root_mut().push_back(new_node);
    }
}

// direct & indirect orbits count of a single node
fn node_orbits_count(node: &Node<String>) -> i32 {
    // println!("DEBUG: node_orbits_count node={:?}", node);
    if let Some(parent) = node.parent() {
        1 + node_orbits_count(parent)
    } else {
        0
    }
}

// count all orbits, both direct and indirect
fn tree_total_orbits_count<'a, I>(iter: I) -> i32
where
    I: Iterator<Item = &'a Node<String>>,
{
    iter.fold(0, |acc, node| {
        acc + node_orbits_count(node) + tree_total_orbits_count(node.iter())
    })
}

fn node_path_down<'a>(node: &'a Node<String>, val: &str) -> Option<Vec<&'a Node<String>>> {
    let mut path = vec![node];

    for c in node.iter() {
        // println!("DEBUG: node_path node={} child={}", node.data, c.data);
        if c.data == val {
            path.push(c);
            return Some(path);
        } else if let Some(mut sub_path) = node_path_down(c, val) {
            path.append(&mut sub_path);
            return Some(path);
        }
    }

    None
}

fn path_between<'a>(start: &str, end: &str, tree: &'a Tree<String>) -> Vec<&'a Node<String>> {
    let target_from_root = node_path_down(tree.root(), end).expect("target should be found");
    //println!("DEBUG: target_from_root={:?}", target_from_root);

    let start_from_root = node_path_down(tree.root(), start).expect("start should be found");
    //println!("DEBUG: start_from_root{:?}", start_from_root);

    // find the shared point closet to start
    let split_point = start_from_root
        .iter()
        .rev()
        .find(|n| target_from_root.contains(n))
        .expect("split point should be found");
    // println!("DEBUG: split point found at {}", split_point.data);

    let mut path = start_from_root
        .iter()
        .rev()
        .take_while(|n| n.data != split_point.data)
        .map(|x| *x)
        .collect::<Vec<&'a Node<String>>>();

    let mut part_2 = target_from_root
        .iter()
        .skip_while(|n| n.data != split_point.data)
        .map(|x| *x)
        .collect::<Vec<&'a Node<String>>>();

    path.append(&mut part_2);

    path
}

fn main() {
    let input = &args().collect::<Vec<String>>()[1];
    let orbits = read_orbits(input);
    let tree = build_orbit_tree(&orbits);

    // println!("DEBUG: built tree {:?}", tree);
    println!(
        "p1: total orbits count = {}",
        tree_total_orbits_count(tree.iter())
    );

    let path = path_between("YOU", "SAN", &tree);
    let path_names: Vec<&String> = path.iter().map(|x| &x.data).collect();
    println!("p2: transfer path = {:?}", path_names);
    println!("p2: # transfers = {}", path_names.len() - 3);
}
