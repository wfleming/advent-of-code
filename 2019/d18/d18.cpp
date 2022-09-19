#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <set>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <optional>
#include <numeric>
#include <queue>

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

    /* unsigned int distance(const Point& other) const { */
    /*   return abs(x - other.x) + abs(y - other.y); */
    /* } */

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

struct Edge { unsigned long cost; set<char> needed_keys; };
class Maze {
  public:
    unordered_set<Point> floor;
    unordered_map<Point, char> doors;
    unordered_map<Point, char> keys;
    unordered_map<char, Point> key_positions{};
    vector<Point> starts;
    unordered_map<Point, unordered_map<Point, Edge>> all_paths;
    set<char> all_keys{};

    static Maze parse(istream&& in) {
      unordered_set<Point> floor;
      unordered_map<Point, char> doors, keys;
      vector<Point> start_pts;

      unsigned int y = 0;
      string line;
      while (in.good()) {
        getline(in, line);
        for (unsigned int x = 0; x < line.size(); x++) {
          char c = line[x];
          Point p = Point(x,y);
          if (c == '.') {
            floor.insert(p);
          } else if (c == '@') {
            floor.insert(p);
            start_pts.push_back(p);
          } else if (isupper(c)) {
            floor.insert(p);
            doors[p] = c;
          } else if (islower(c)) {
            floor.insert(p);
            keys[p] = c;
          }
        }
        y++;
      }
      return Maze{floor, doors, keys, start_pts};
    }


    // use djikstra to build all paths between starting point(s) and keys, and
    // between keys and different keys.
    static unordered_map<Point, unordered_map<Point, Edge>> build_all_paths(const Maze& maze) {
      unordered_map<Point, unordered_map<Point, Edge>> paths;

      vector<Point> start_pts{};
      for (auto p : maze.starts) { start_pts.push_back(p); }
      for (auto i = maze.keys.begin(); i != maze.keys.end(); i++) { start_pts.push_back(i->first); }

      for (auto pt : start_pts) {
        paths[pt] = Maze::paths_from(maze, pt);
      }

      return paths;
    }

    // map dest -> path
    static unordered_map<Point, Edge>  paths_from(const Maze& maze, const Point& origin) {
      unordered_map <Point, int> dist{};
      unordered_map<Point, Point> prev{};
      queue<Point> queue{};

      queue.push(origin);
      dist[origin] = 0;

      while (!queue.empty()) {
        auto pt = queue.front();
        queue.pop();

        for (auto n : pt.neighbors()) {
          if (!maze.is_floor(n)) { continue; }
          auto n_dist = dist[pt] + 1;
          if (!dist.contains(n) || n_dist < dist[n]) {
            dist[n] = n_dist;
            prev[n] = pt;
            queue.push(n);
          }
        }
      }

      // build that into a map of edges (dest -> edge)
      unordered_map<Point, Edge> paths;

      // build up paths to keys
      for(auto key_pair : maze.keys) {
        auto dest = key_pair.first;

        if (!prev.contains(dest)) { continue; }

        auto path_edge = dest;
        auto e = Edge{1, set<char>{}};

        while(prev.at(path_edge) != origin) {
          e.cost++;
          if (maze.doors.contains(path_edge)) { e.needed_keys.insert(tolower(maze.doors.at(path_edge))); }
          path_edge = prev.at(path_edge);
        }

        paths[dest] = e;
      }

      return paths;
    }

    Maze(unordered_set<Point> floor, unordered_map<Point, char> doors, unordered_map<Point, char> keys, vector<Point> starts):
      floor{floor},
      doors{doors},
      keys{keys},
      starts{starts}
      {
        all_paths = Maze::build_all_paths(*this);

        for (auto pair : keys) {
          all_keys.insert(pair.second);
          key_positions.insert({pair.second, pair.first});
        }
      };

    bool is_floor(const Point& pt) const {
      return floor.contains(pt);
    }
};

struct Step {
  Point from, to;
  unsigned long length;
};

struct State {
  shared_ptr<Maze> maze;
  vector<Step> steps;
  vector<Point> positions;
  set<char> keys;

  State(const Maze& maze):
    maze{shared_ptr<Maze>{new Maze(maze)}},
    steps{{}},
    positions{maze.starts},
    keys{set<char>{}}
    {};

  bool operator==(const State& other) const {
    return other.positions == positions && other.keys == keys;
  }

  operator std::string() const {
    ostringstream s;
    s << "<State steps_count=" << steps_count()
      << " pos=";
    for (auto p: positions) { s << p << ','; }
    s  << " keys=";
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
  vector<State> next_states() const {
    vector<State> rv;
    vector<char> keys_needed;
    set_difference(maze->all_keys.begin(), maze->all_keys.end(), keys.begin(), keys.end(), back_inserter(keys_needed));

    for (size_t pidx = 0; pidx < positions.size(); pidx++) {
      auto pos = positions[pidx];
      for (auto key : keys_needed) {
        if (!maze->key_positions.contains(key)) { cerr << "BUG key_positions" << endl; }
        auto dest = maze->key_positions.at(key);

        // in part 2 there may not be a path from here to there
        if (!maze->all_paths.contains(pos)) { cerr << "BUG all_paths pos" << endl; }
        if (!maze->all_paths.at(pos).contains(dest)) { continue; }

        auto edge = maze->all_paths.at(pos).at(dest);

        // reject path if it involves walking through a door we don't have the key
        // for
        bool blocked_by_door = !includes(keys.begin(), keys.end(), edge.needed_keys.begin(), edge.needed_keys.end());
        if (blocked_by_door) { continue; }

        State n = State{*this};
        n.positions[pidx] = dest;
        n.keys.insert(key);
        n.steps.push_back(Step{pos, dest, edge.cost});
        rv.push_back(n);
      }
    }

    return rv;
  }

  // how many steps this state has taken
  unsigned long steps_count() const {
    return accumulate(steps.begin(), steps.end(), 0, [](auto t, auto s) { return t + s.length; });
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

template<> struct std::hash<vector<Point>> {
  size_t operator()(const vector<Point>& pts) const noexcept {
    if (pts.empty()) { return 0; }

    size_t h = hash<Point>{}(pts.front());
    for (size_t i = 1; i < pts.size(); i++) {
      h = h ^ (hash<Point>{}(pts[i]) << i);
    }

    return h;
  };
};

template<> struct std::hash<State> {
  size_t operator()(const State& s) const noexcept {
    return hash<vector<Point>>{}(s.positions) ^ (hash<set<char>>{}(s.keys) << 2);
  };
};

optional<State> find_goal(const State& cur_state) {
  auto cmp = [](const State& s1, const State& s2) { return s1.steps_count() > s2.steps_count(); };
  priority_queue<State, vector<State>, decltype(cmp)> queue{cmp};
  unordered_map<State, unsigned long> dist;
  queue.push(cur_state);
  dist.insert({cur_state, 0});

  while (!queue.empty()) {
    auto s = queue.top();
    queue.pop();

    if (s.is_complete()) {
      return optional<State>{s};
    }

    for (auto n : s.next_states()) {
      if (!dist.contains(n) || n.steps_count() < dist.at(n)) {
        dist.insert_or_assign(n, n.steps_count());
        queue.push(n);
      }
    }
  }

  // no solution found
  return optional<State>{};
}

Maze p2maze(const Maze& maze) {
  auto old_start = maze.starts.front();
  auto new_starts = vector<Point>{
    Point{old_start.x - 1, old_start.y - 1},
    Point{old_start.x - 1, old_start.y + 1},
    Point{old_start.x + 1, old_start.y - 1},
    Point{old_start.x + 1, old_start.y + 1},
  };

  auto new_floor = unordered_set<Point>{maze.floor};
  new_floor.erase(old_start);
  new_floor.erase(Point{old_start.x, old_start.y - 1});
  new_floor.erase(Point{old_start.x, old_start.y + 1});
  new_floor.erase(Point{old_start.x - 1, old_start.y});
  new_floor.erase(Point{old_start.x + 1, old_start.y});

  return Maze{new_floor, maze.doors, maze.keys, new_starts};
}

#ifndef IS_TEST
int main(int argc, char** argv) {
  if (argc < 2) {
    cerr << "Provide input filename as argument" << endl;
    return 1;
  }
  auto maze = Maze::parse(ifstream{argv[1]});

  auto p1path = find_goal(State{maze});
  if (p1path.has_value()) {
    cout << "p1: " << p1path->steps_count() << " steps" << endl;
  } else {
    cout << "p1: failed to find goal." << endl;
  }

  auto p2path = find_goal(State{p2maze(maze)});
  if (p2path.has_value()) {
    cout << "p2: " << p2path->steps_count() << " steps" << endl;
  } else {
    cout << "p2: failed to find goal." << endl;
  }
  return 0;
}
#endif
