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
  cout << "DEBUG: steps_count=" << path->steps_count() << endl;
  assert(path->steps_count() == 86);
}

void test_queue_sorting() {
  auto cmp = [](const State& s1, const State& s2) { return s1.steps_count() > s2.steps_count(); };
  priority_queue<State, vector<State>, decltype(cmp)> queue{cmp};

  auto maze = Maze::parse(istringstream{sample_input3});
  auto s0 = State{maze};
  s0.keys.insert('a');
  s0.steps.push_back(Step{Point{0,0}, Point{0,0}, 5});
  queue.push(s0);

  auto s1 = State{maze};
  s1.keys.insert('b');
  s1.steps.push_back(Step{Point{0,0}, Point{0,0}, 3});
  queue.push(s1);

  auto s2 = State{maze};
  s2.keys.insert('c');
  s2.steps.push_back(Step{Point{0,0}, Point{0,0}, 4});
  queue.push(s2);

  assert(queue.top() == s1);
  queue.pop();
  assert(queue.top() == s2);
  queue.pop();
  assert(queue.top() == s0);
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
  test_queue_sorting();
  test_part_1_search_sample3();

  cout << "Tests completed" << endl;
}
