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
