#include <cassert>
#include "d18.cpp"

const char* sample_input = R"(########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
)";

void test_point_eq() {
  auto a = Point(1,1), b = Point(1,1), c = Point(1,2), d = Point(1,2);
  assert(a == a);
  assert(a == b);
  assert(c == d);
  assert(a != c);
}

void test_maze_parse() {
  auto maze = Maze::parse(istringstream{sample_input});

  assert(maze.start == Point(15,1));
}

void test_find_path_to_a() {
  auto maze = Maze::parse(istringstream{sample_input});
  auto pts = find_path(maze, maze.start, Point(17,1), set<char>{});

  assert(pts.size() == 3); // 2 steps
}

void test_find_path_to_b() {
  auto maze = Maze::parse(istringstream{sample_input});
  auto pts = find_path(maze, maze.start, Point(11,1), set<char>{});

  assert(pts.size() == 0); // can't walk to b without the key for A

  pts = find_path(maze, maze.start, Point(11,1), set<char>{'a'});

  assert(pts.size() == 5); // with key a it takes 4 steps
}

void test_part_1_search() {
  auto maze = Maze::parse(istringstream{sample_input});

  auto path = part1(maze);

  assert(path.steps == 86);
}

void test_foo() {
  auto maze = Maze::parse(istringstream{sample_input});

  auto p1 = Path(maze);
  assert(p1.keys.size() == 0);

  auto p2 = Path(p1);
  p2.keys.insert('a');
  assert(p2.keys.size() == 1);
  assert(p1.keys.size() == 0);
}

int main(int, char**) {
  test_point_eq();
  test_maze_parse();
  test_find_path_to_a();
  test_find_path_to_b();
  test_part_1_search();
  test_foo();
}
