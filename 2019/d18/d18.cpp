#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_set>
#include <unordered_map>

using namespace std;

class Point {
  public:
    int x, y;
    Point(int x, int y): x{x}, y{y} {};
    ~Point() = default;

    operator std::string() const {
      ostringstream s;
      s << '(' << x << ',' << y << ')';
      return s.str();
    }

    bool operator==(const Point& other) const {
      return other.x == x && other.y == y;
    }

    bool operator!=(const Point& other) const {
      return !(*this == other);
    }

    vector<Point> neighbors() {
      vector<Point> v{
        Point(x - 1, y),
        Point(x + 1, y),
        Point(x, y - 1),
        Point(x, y + 1),
      };
      return v;
    }
};

ostream& operator<<(ostream& os, const Point& pt) {
  return os << (string)pt;
}

template<> struct std::hash<Point> {
  size_t operator()(const Point& p) const noexcept {
    return p.x ^ (p.y << 1);
  }
};

class Maze {
  public:
    unordered_set<Point> floor;
    unordered_map<char, Point> doors;
    unordered_map<char, Point> keys;
    Point cur_pos;
    unordered_set<char> current_keys;

    static Maze parse(istream& in) {
      Maze maze = Maze();
      unsigned int y = 0;
      string line;
      while(in.good()) {
        getline(in, line);
        for(unsigned int x = 0; x < line.size(); x++) {
          char c = line[x];
          Point p = Point(x,y);
          if (c == '.') {
            maze.floor.insert(p);
          } else if (c == '@') {
            maze.floor.insert(p);
            maze.cur_pos = p;
          } else if (isupper(c)) {
            maze.floor.insert(p);
            /* maze.doors[c] = p; */
          } else if (islower(c)) {
            maze.floor.insert(p);
            /* maze.keys[c] = p; */
          }
        }
        y++;
      }
      return maze;
    }

    Maze():
      floor{unordered_set<Point>()},
      doors{unordered_map<char, Point>()},
      keys{unordered_map<char, Point>()},
      cur_pos{Point(0,0)},
      current_keys{unordered_set<char>()}
      {};

    bool can_open(char door) {
      return current_keys.contains(tolower(door));
    }

    bool is_goal() {
      for(auto it=keys.begin(); it!=keys.end(); it++) {
        if (!current_keys.contains(it->first)) { return false; }
      }
      return true;
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
