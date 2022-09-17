#import <istream>
#import <memory>
#import <string>
#import <utility>
#import <vector>
#include <fstream>
#include <iostream>

using namespace std;

string trim(const string& str)
{
    size_t first = str.find_first_not_of(' ');
    if (string::npos == first) { return str; }
    size_t last = str.find_last_not_of(" \t\n");
    return str.substr(first, (last - first + 1));
}

typedef pair<size_t, size_t> pos_t;

struct Map {
  vector<string> lines{};

  Map(vector<string> ls): lines{ls} {};
  Map(const Map& other): lines{other.lines} {};

  bool operator==(const Map& other) const = default;
  bool operator!=(const Map& other) const = default;
  Map& operator=(Map other) {
    swap(lines, other.lines);
    return *this;
  }


  static Map parse(istream&& input) {
    vector<string> lines;
    string line;

    while (input.good()) {
      getline(input, line);
      if (trim(line).size() > 0) {
        lines.push_back(trim(line));
      }
    }

    return Map{lines};
  }

  size_t max_y() {
    return lines.size() - 1;
  }

  size_t max_x() {
    return lines[0].size() - 1;
  }

  char at(pos_t pos) {
    return lines[pos.second][pos.first];
  }

  pos_t next_pos(pos_t pos) {
    char c = at(pos);
    if (c == '>') {
      return pos_t{pos.first == max_x() ? 0 : pos.first + 1, pos.second};
    } else if (c == 'v') {
      return pos_t{pos.first, pos.second == max_y() ? 0 : pos.second + 1};
    } else {
      throw "Invalid next_pos call: no cucumber at pos";
    }
  }

  vector<pos_t> movable_for_tribe(char tribe) {
    vector<pos_t> rv;

    for(size_t y = 0; y <= max_y(); y++) {
      for(size_t x = 0; x <= max_x(); x++) {
        if (at(pos_t{x,y}) == tribe && at(next_pos(pos_t{x,y})) == '.') {
          rv.push_back(pos_t{x,y});
        }
      }
    }

    return rv;
  }

  void move_tribe(char tribe) {
    for (auto p : movable_for_tribe(tribe)) {
      auto n = next_pos(p);
      lines[p.second][p.first] = '.';
      lines[n.second][n.first] = tribe;
    }
  }

  Map step() {
    Map next = Map{*this};
    next.move_tribe('>');
    next.move_tribe('v');
    return next;
  }
};

size_t p1(Map m) {
  size_t r = 1;
  Map m2 = m.step();
  while(m != m2) {
    m = m2;
    m2 = m.step();
    r++;
  }

  return r;
}

#ifndef IS_TEST
int main(int argc, char** argv) {
  if (argc < 2) {
    std::cerr << "Provide input filename as argument" << std::endl;
    return 1;
  }
  auto map = Map::parse(ifstream{argv[1]});

  cout << "p1: " << p1(map) << " steps" << endl;
}
#endif
