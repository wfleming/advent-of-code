#define CATCH_CONFIG_MAIN
#include <catch.hpp>
#include "d18.cpp"

const char* sample_input = R"(########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
)";

TEST_CASE("==", "[Point]") {
  auto a = Point(1,1), b = Point(1,1), c = Point(1,2), d = Point(1,2);
  REQUIRE(a == a);
  REQUIRE(a == b);
  REQUIRE(c == d);
  REQUIRE(a != c);
}

TEST_CASE("parse", "[Maze]") {
  auto in = istringstream{sample_input};
  auto maze = Maze::parse(in);

  REQUIRE(maze.cur_pos == Point(15,1));
}

TEST_CASE("step_to", "[Maze]") {
  auto in = istringstream{sample_input};
  auto maze = Maze::parse(in);

  // step_to doesn't validate that the dest is a neighbor of cur_pos, which I'm
  // taking advantage of here. In production code step_to would verify that
  // invariant here, but for AOC it's fine since I know `next_states` takes care
  // of it, and it's convenient for a test here.
  auto maze2 = maze.step_to(Point(17, 1));

  REQUIRE(maze2.cur_pos == Point(17,1));
  REQUIRE(maze.cur_pos == Point(15,1)); // original maze should not have changed

  REQUIRE(maze2.held_keys.size() == 1);
  REQUIRE(maze2.held_keys.contains('a'));
  REQUIRE(maze.held_keys.size() == 0); // original maze should not have changed
}

TEST_CASE("search", "[astar]") {
  auto in = istringstream{sample_input};
  auto maze = Maze::parse(in);

  auto path = astar(maze);

  REQUIRE(path.size() == 87); // should be 86 steps, which means 87 states
}
