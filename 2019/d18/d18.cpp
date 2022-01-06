#include <iostream>
#include <fstream>
#include <tuple>
#include <unordered_set>
#include <unordered_map>

using namespace std;

typedef tuple<int, int> Point;
//TODO - neighbors

class Maze {
  public:
    unordered_set<Point> *floor;
    unordered_map<char, Point> *doors;
    unordered_map<char, Point> *keys;
    Point cur_pos;
    unordered_set<Point> *current_keys;

    static Maze* parse(istream& in) {
      in.good();
      return new Maze();
    }

    // TODO: subsequent states. Can move through doors if have key.
};

// TODO - search a path. A*?


#ifndef IS_TEST
int main(int argc, char** argv) {
  cout << "hello world";
  return 0;
}
#endif
