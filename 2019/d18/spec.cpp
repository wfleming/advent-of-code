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
  auto maze = Maze::parse(istringstream{sample_input});

  REQUIRE(maze.start == Point(15,1));
}

TEST_CASE("find-path-to-a", "[find_path]") {
  auto maze = Maze::parse(istringstream{sample_input});
  auto pts = find_path(maze, maze.start, Point(17,1), set<char>{});

  REQUIRE(pts.size() == 3); // 2 steps
}

TEST_CASE("find-path-to-b", "[find_path]") {
  auto maze = Maze::parse(istringstream{sample_input});
  auto pts = find_path(maze, maze.start, Point(11,1), set<char>{});

  REQUIRE(pts.size() == 0); // can't walk to b without the key for A

  pts = find_path(maze, maze.start, Point(11,1), set<char>{'a'});

  REQUIRE(pts.size() == 5); // with key a it takes 4 steps
}

TEST_CASE("search", "[part1]") {
  auto maze = Maze::parse(istringstream{sample_input});

  auto path = part1(maze);

  REQUIRE(path.steps == 86);
}
