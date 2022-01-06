#define CATCH_CONFIG_MAIN
#include <catch.hpp>
#include "d18.cpp"

const char* sample_input = R"(########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
)";


TEST_CASE("parse", "[Maze]") {
  auto in = istringstream{sample_input};
  auto maze = Maze::parse(in);

  REQUIRE(maze->cur_pos == Point{15,1});
}
