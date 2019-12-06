use std::env::args;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::vec::Vec;
use trees::{tr, Tree, Node};

fn read_orbits(path: &str) -> Vec<(String, String)> {
    let file = File::open(path).expect("couldn't open file");
    let buffered = BufReader::new(file);

    buffered.lines().map(|l| {
        let l = l.expect("should have a line");
        let pieces: Vec<&str> = l.split(")").map(|p| p.trim()).collect();
        assert!(pieces.len() == 2, "each line should have exactly 2 objects");
        (pieces[0].to_string(), pieces[1].to_string())
    }).collect()
}

fn build_orbit_tree(orbits: &Vec<(String, String)>) -> Tree<String> {
    // COM is known to be the root
    let mut root = tr("COM".to_string());

    add_orbits(&mut root, orbits);

    root
}

// add the children for a given node, recursing for each child
fn add_orbits(parent: &mut Tree<String>, orbits: &Vec<(String, String)>) {
    let children_names = orbits.iter().
        filter(|orbit| parent.data == orbit.0).
        map(|orbit| orbit.1.clone()).collect::<Vec<String>>();

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
where I: Iterator<Item = &'a Node<String>>
{
    iter.fold(0, |acc, node| acc + node_orbits_count(node) + tree_total_orbits_count(node.iter()))
}

fn find_node_bfs<'a, I>(iter: &I, val: &str) -> &'a Node<String>
where I: Iterator<Item = &'a Node<String>>
{
    for n in iter {
        if n.data == val {
            return &n;
        } else {
            return find_node_bfs(&n.iter(), val);
        }
    }

    panic!("should have returned by now");
}

fn path_between(start: &str, end: &str, tree: Tree<String>) -> Vec<Node<String>> {
    let start_node = find_node_bfs(&tree.iter(), start);

    // TODO
    vec![]
}

fn search_paths() {
    // TODO
}

fn main() {
    let input = &args().collect::<Vec<String>>()[1];
    let orbits = read_orbits(input);
    let tree = build_orbit_tree(&orbits);

    // println!("DEBUG: built tree {:?}", tree);
    println!("p1: total orbits count = {}", tree_total_orbits_count(tree.iter()));

    println!("p2: transfers needed = {}", tree_total_orbits_count(tree.iter()));
}
