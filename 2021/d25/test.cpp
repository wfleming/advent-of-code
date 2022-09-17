#include <cassert>
#include <sstream>
#include "d25.cpp"

void test_example() {
  auto i = R"(v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>)";

  auto m = Map::parse(istringstream{i});
  auto r = p1(m);
  assert(r == 58);
}

int main(int, char**) {
  test_example();

  cout << "Tests complete." << endl;
}
