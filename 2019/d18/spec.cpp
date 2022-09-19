#include <cassert>
#include "d18.cpp"

const char* sample_input = R"(########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
)";

const char* sample_input3 = R"(#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
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
  assert(!maze.all_paths.empty());
}

void test_sample_initial_next_states() {
  auto maze = Maze::parse(istringstream{sample_input});
  auto state0 = State{maze};

  // can only walk to key a
  assert(state0.next_states().size() == 1);

  // from there you can only walk to key b
  auto state1 = state0.next_states().front();
  assert(state1.next_states().size() == 1);
}

void test_part_1_search_sample0() {
  auto maze = Maze::parse(istringstream{sample_input});

  auto path = part1(maze);

  assert(path.has_value());
  assert(path->steps_count() == 86);
}

void test_part_1_search_sample3() {
  auto maze = Maze::parse(istringstream{sample_input3});

  auto path = part1(maze);

  assert(path.has_value());
  assert(path->steps_count() == 136);
}

int main(int, char**) {
  test_point_eq();
  test_maze_parse();
  test_sample_initial_next_states();
  test_part_1_search_sample0();
  test_part_1_search_sample3();

  cout << "Tests completed" << endl;
}
