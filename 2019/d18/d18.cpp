#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <tuple>
#include <queue>
#include <set>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace std;

class Point {
  public:
    int x, y;
    Point(): x{0}, y{0} {}; // needed for unordered_map[]=
    Point(int x, int y): x{x}, y{y} {};

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

    unsigned int distance(const Point& other) const {
      return abs(x - other.x) + abs(y - other.y);
    }

    vector<Point> neighbors() const {
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
    unordered_map<Point, char> doors;
    unordered_map<Point, char> keys;
    Point start;

    static Maze parse(istream&& in) {
      Maze maze = Maze();
      unsigned int y = 0;
      string line;
      while (in.good()) {
        getline(in, line);
        for (unsigned int x = 0; x < line.size(); x++) {
          char c = line[x];
          Point p = Point(x,y);
          if (c == '.') {
            maze.floor.insert(p);
          } else if (c == '@') {
            maze.floor.insert(p);
            maze.start = p;
          } else if (isupper(c)) {
            maze.floor.insert(p);
            maze.doors[p] = c;
          } else if (islower(c)) {
            maze.floor.insert(p);
            maze.keys[p] = c;
          }
        }
        y++;
      }
      return maze;
    }

    Maze():
      // is there a more concise way to say this? I'm surprised I can't omit the
      // types and do unordered_set<>{...}. You can just do a list of args to a
      // constructor, so `floor{{}}` works. Weirdly, that's fine for `floor` and
      // `doors`, but breaks my algorithm for `keys` - not sure what it does
      // differently.
      floor{unordered_set<Point>()},
      doors{unordered_map<Point, char>()},
      keys{unordered_map<Point, char>()},
      start{Point()}
      {};
};

vector<Point> reconstruct_path(unordered_map<Point, Point> came_from, Point last_pos) {
  vector<Point> rv{last_pos};
  while (came_from.contains(last_pos)) {
    last_pos = came_from[last_pos];
    rv.push_back(last_pos);
  }
  return rv;
}

// A* search to walk between two points
vector<Point> find_path(const Maze& maze, const Point &start, const Point &goal, const set<char> held_keys) {
  unordered_map<Point, Point> came_from;
  unordered_map<Point, unsigned int> f_score;
  unordered_map<Point, unsigned int> g_score;
  auto compare = [&f_score](auto a, auto b) { return f_score[a] > f_score[b]; };
  vector<Point> open_set{start};

  g_score[start] = 0;
  f_score[start] = start.distance(goal);
  ranges::make_heap(open_set, compare);

  while (open_set.size() > 0) {
    ranges::pop_heap(open_set, compare);
    auto cur_node = open_set.back();
    open_set.pop_back();
    /* cout << "DEBUG: cur_node.f_score = " << cur_node.f_score() << " queue:"; for (auto m : open_set) { cout << m.f_score() << ", "; } cout << endl; */

    if (cur_node == goal) {
      return reconstruct_path(came_from, cur_node);
    }

    // bot can  step to neighboring point if 1) pt is just floor or 2) pt is
    // goal or 3) pt is a key we already have or 4) pt is a door we have key for
    vector<Point> next_states{};
    for (auto p : cur_node.neighbors()) {
      if (p == goal) {
        next_states.push_back(p);
      } else if (maze.doors.contains(p)) { // it's a door, check if we can open it
        if (held_keys.contains(tolower(maze.doors.at(p)))) { next_states.push_back(p); };
      } else if (maze.keys.contains(p)) { // it's a key, check if we already have it
        if (held_keys.contains(maze.keys.at(p))) { next_states.push_back(p); }
      } else if (maze.floor.contains(p)) { // check it's floor
        next_states.push_back(p);
      }
    }

    for(auto next_node : next_states) {
      /* cout << "\tDEBUG: from " << cur_node.cur_pos << " to " << next_node.cur_pos << endl; */
      unsigned int node_g_score = g_score[cur_node] + 1;

      if (!g_score.contains(next_node) || node_g_score < g_score[next_node]) {
        /* cout << "DEBUG: pushing next_node.f_score = " << next_node.f_score() << endl; */
        came_from[next_node] = cur_node;
        g_score[next_node] = node_g_score;
        f_score[next_node] = node_g_score + next_node.distance(goal);
        if (ranges::find(open_set, next_node) == open_set.end()) {
          open_set.push_back(next_node);
          ranges::push_heap(open_set, compare);
        }
      }
    }
  }

  return vector<Point>{}; // failure
}

struct Path {
  shared_ptr<Maze> maze;
  unsigned int steps;
  Point pos;
  set<char> keys;

  Path(): maze{shared_ptr<Maze>{}}, steps{0}, pos{Point(0,0)}, keys{set<char>{}} {};
  Path(const Maze& maze):
    maze{shared_ptr<Maze>{new Maze(maze)}},
    steps{0},
    pos{maze.start},
    keys{set<char>{}}
    {};

  bool operator==(const Path& other) const {
    return other.maze == maze && other.steps == steps && other.pos == pos && other.keys == keys;
  }

  operator std::string() const {
    ostringstream s;
    s << "<Path steps=" << steps
      << " pos=" << pos
      << " keys=";
    for (auto k : keys) { s << k; }
    s  << '>';
    return s.str();
  }

  // has found all keys
  bool is_complete() const {
    for (auto p : maze->keys) {
      if (!keys.contains(p.second)) {
        return false;
      }
    }
    return true;
  }

  // walk to next available keys, return new set of paths
  vector<Path> next_states() const {
    vector<Path> rv;
    auto start = pos;
    if (pos == Point(0,0)) { // initial state, start at origin
      start = maze->start;
    }

    for(auto p : maze->keys) {
      if (keys.contains(p.second)) {
        continue;
      }

      auto pts = find_path(*maze, start, p.first, keys);
      if (pts.size() > 0) {
        auto new_path = Path(*this);
        new_path.steps += (pts.size() - 1);
        new_path.pos = p.first;
        new_path.keys.insert(p.second);
        rv.push_back(new_path);
      }
    }

    return rv;
  }
};

template<> struct std::hash<set<char>> {
  size_t operator()(const set<char>& s) const noexcept {
    size_t h = 0;
    unsigned int i = 0;
    for (auto c : s) {
      h = h ^ (hash<char>{}(c) << i);
      i++;
    }
    return h;
  };
};

template<> struct std::hash<tuple<Point, set<char>, Point>> {
  size_t operator()(const tuple<Point, set<char>, Point>& t) const noexcept {
    auto h1 = hash<Point>{}(get<0>(t));
    auto h2 = hash<set<char>>{}(get<1>(t));
    auto h3 = hash<Point>{}(get<2>(t));
    return h1 ^ (h2 << 1) ^ (h3 << 2);
  }
};

Path part1(const Maze& maze) {
  auto compare = [](auto a, auto b) { return a.steps > b.steps; };
  vector<Path> open_set;
  open_set.push_back(Path(maze));
  ranges::make_heap(open_set, compare);

  // cache from (origin pos, keys held, dest point) -> step count
  unordered_map<tuple<Point, set<char>, Point>, unsigned int> cache;

  while (open_set.size() > 0) {
    ranges::pop_heap(open_set, compare);
    auto cur_path = open_set.back();
    open_set.pop_back();
    /* cout << "DEBUG: part1 loop cur=" << (string)cur_path << " queue=" << open_set.size() << " (" ; */
    /* for (unsigned long i = 0; i < min((unsigned long)10, open_set.size()); i++) { cout << open_set[i].steps << ", "; } */
    /* cout << ')' << endl; // DEBUG */

    if (cur_path.is_complete()) { // goal reached: full path to all keys
      return cur_path;
    }

    for(auto p : maze.keys) {
      // if we already hold this key, skip
      if (cur_path.keys.contains(p.second)) {
        continue;
      }

      auto cache_key = make_tuple<>(cur_path.pos, cur_path.keys, p.first);
      if (cache.contains(cache_key)) {
        /* cout << "DEBUG: cache hit for pos=" << cur_path.pos << " keys="; */
        /* for (auto c : cur_path.keys) { cout << c; } */
        /* cout << "dest=" << p.first << " (" << p.second << ")" << endl; // DEBUG */

        auto next_path = Path(cur_path);
        next_path.steps += cache.at(cache_key);
        next_path.pos = p.first;
        next_path.keys.insert(p.second);

        if (ranges::find(open_set, next_path) == open_set.end()) {
          open_set.push_back(next_path);
          ranges::push_heap(open_set, compare);
        }
      } else {
        auto pts = find_path(maze, cur_path.pos, p.first, cur_path.keys);
        if (pts.size() > 0) { // if the returned path is empty we can't walk there
          auto next_path = Path(cur_path);
          next_path.steps += pts.size() - 1;
          next_path.pos = p.first;
          next_path.keys.insert(p.second);

          cache.insert(pair{cache_key, pts.size() - 1});

          if (ranges::find(open_set, next_path) == open_set.end()) {
            open_set.push_back(next_path);
            ranges::push_heap(open_set, compare);
          }
        }
      }
    }
  }

  return Path(); // failure
}

#ifndef IS_TEST
int main(int argc, char** argv) {
  if (argc < 2) {
    std::cerr << "Provide input filename as argument" << std::endl;
    return 1;
  }
  auto maze = Maze::parse(ifstream{argv[1]});

  auto p1path = part1(maze);
  cout << "p1: " << p1path.steps << " steps";
  return 0;
}
#endif
