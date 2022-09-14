#include <cassert>
#include "d23.cpp"

void test_parse() {
  const char* sample_input = R"(#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########)";
  auto mapstate = MapState::parse(istringstream{sample_input});

  assert(mapstate.amphipods.size() == 8);
}

void test_next_states_easy() {
  const char* sample_input = R"(#############
#.A.........#
###.#B#C#D###
  #A#B#C#D#
  #########)";
  auto mapstate = MapState::parse(istringstream{sample_input});

  auto a = find_if(mapstate.amphipods.begin(), mapstate.amphipods.end(), [](auto a2) { return a2.type == 'A' && a2.pos.x == 2; });
  assert(a != mapstate.amphipods.end());
  auto ns = mapstate.next_states_for_amphipod(*a);
  assert(ns.size() == 1);
  auto a_after = find_if(ns.front().amphipods.begin(), ns.front().amphipods.end(), [&a](auto a2) { return a2.id == a->id; });
  assert(a_after != ns.front().amphipods.end());
  assert((a_after->pos == Point{3,2}));
}

void test_easy_find_goal() {
  const char* sample_input = R"(#############
#.A.........#
###.#B#C#D###
  #A#B#C#D#
  #########)";
  auto mapstate = MapState::parse(istringstream{sample_input});

  auto g = find_goal(mapstate);
  assert(g.has_value());
}

int main(int, char**) {
  test_parse();
  test_next_states_easy();
  test_easy_find_goal();
}
