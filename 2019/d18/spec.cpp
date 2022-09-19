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

  assert(maze.starts == vector<Point>{Point(15,1)});
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

void test_state_equality_and_hashing() {
  auto maze = Maze::parse(istringstream{sample_input});

  auto s0 = State{maze}, s1 = State{maze};
  assert(s0 == s1);
  assert(hash<State>{}(s0) == hash<State>{}(s1));
}

void test_part_1_search_sample0() {
  auto maze = Maze::parse(istringstream{sample_input});

  auto path = find_goal(State{maze});

  assert(path.has_value());
  assert(path->steps_count() == 86);
}

void test_part_1_search_sample3() {
  auto maze = Maze::parse(istringstream{sample_input3});

  auto path = find_goal(State{maze});

  assert(path.has_value());
  assert(path->steps_count() == 136);
}

const char *p2_sample_input = R"(#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#.b#
#######
)";

void test_part_2_maze() {
  auto maze = Maze::parse(istringstream{p2_sample_input});
  auto maze2=p2maze(maze);

  // print for visual inspection
  int min_x = 100, max_x = 0, min_y = 100, max_y = 0;
  for (auto pt : maze2.floor) {
    if (pt.x < min_x) { min_x = pt.x; }
    if (pt.x > max_x) { max_x = pt.x; }
    if (pt.y < min_y) { min_y = pt.y; }
    if (pt.y > max_y) { max_y = pt.y; }
  }

  for (auto y = min_y - 1; y <= max_y + 1; y++) {
    for (auto x = min_x - 1; x <= max_x + 1; x++) {
      auto p = Point{x,y};
      if (!maze2.floor.contains(p)) {
        cout << '#';
      } else {
        cout << '.';
      }
    }

    cout << endl;
  }
}

void test_part_2_search() {
  auto maze = Maze::parse(istringstream{p2_sample_input});
  auto maze2 = p2maze(maze);

  auto path = find_goal(State{maze2});

  assert(path.has_value());
  assert(path->steps_count() == 8);
}

int main(int, char**) {
  test_point_eq();
  test_maze_parse();
  test_sample_initial_next_states();
  test_part_1_search_sample0();
  test_part_1_search_sample3();
  /* test_part_2_maze(); */
  test_part_2_search();

  cout << "Tests completed" << endl;
}
