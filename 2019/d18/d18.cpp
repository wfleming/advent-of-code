#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
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
    shared_ptr<unordered_set<Point>> floor;
    shared_ptr<unordered_map<Point, char>> doors;
    shared_ptr<unordered_map<Point, char>> keys;
    Point cur_pos;
    char goal_key;
    set<char> held_keys;

    static Maze parse(istream& in) {
      Maze maze = Maze();
      unsigned int y = 0;
      string line;
      while (in.good()) {
        getline(in, line);
        for (unsigned int x = 0; x < line.size(); x++) {
          char c = line[x];
          Point p = Point(x,y);
          if (c == '.') {
            maze.floor->insert(p);
          } else if (c == '@') {
            maze.floor->insert(p);
            maze.cur_pos = p;
          } else if (isupper(c)) {
            maze.floor->insert(p);
            (*maze.doors)[p] = c;
          } else if (islower(c)) {
            maze.floor->insert(p);
            (*maze.keys)[p] = c;
          }
        }
        y++;
      }
      return maze;
    }

    Maze():
      // is there a more concise way to say this? I'm surprised I can't omit the
      // types and do shared_ptr<>{...}
      floor{shared_ptr<unordered_set<Point>>{new unordered_set<Point>()}},
      doors{shared_ptr<unordered_map<Point, char>>{new unordered_map<Point, char>()}},
      keys{shared_ptr<unordered_map<Point, char>>{new unordered_map<Point, char>()}},
      cur_pos{Point(0,0)},
      goal_key{'_'},
      held_keys{set<char>()}
      { };

    // similar to hash below, we only really need to check cur_pos & held_keys
    bool operator==(const Maze& other) const {
      return other.cur_pos == cur_pos && other.held_keys == held_keys;
    }

    bool operator!=(const Maze& other) const {
      return !(*this == other);
    }

    vector<char> available_keys() {
      vector<char> rv;
      for (auto p : *keys) {
        if (!held_keys.contains(p.second)) {
          rv.push_back(p.second);
        }
      }
      return rv;
    }

    bool can_open(char door) {
      return held_keys.contains(tolower(door));
    }

    bool can_step(const Point& dest) {
      if (doors->contains(dest)) {
        return can_open((*doors)[dest]);
      } else {
        return floor->contains(dest);
      }
    }

    Maze step_to(Point dest) {
      Maze copy = Maze(*this);
      if (keys->contains(dest) && !held_keys.contains((*keys)[dest])) {
        copy.held_keys.insert((*keys)[dest]);
      }
      copy.cur_pos = dest;
      return copy;
    }

    bool is_goal() {
      for (auto p : *keys) {
        if (!held_keys.contains(p.second)) { return false; }
      }
      return true;
    }

    unsigned int f_score() {
      // use the total manhattan distance to every key we *don't* have.
      int f = 0;
      for (auto p : *keys) {
        if (!held_keys.contains(p.second)) {
          f += cur_pos.distance(p.first);
        }
      }
      return f;
    }

    vector<Maze> next_states() {
      vector<Maze> rv;
      for (auto n : cur_pos.neighbors()) {
        if (can_step(n)) {
          rv.push_back(step_to(n));
        }
      }
      return rv;
    }
};

template<> struct std::hash<Maze> {
  size_t operator()(const Maze& m) const noexcept {
    // we can safely omit floor, door, keys since they're the same for every
    // instance we'll be dealing with.
    size_t h = std::hash<Point>{}(m.cur_pos);
    for(auto it = m.held_keys.begin(); it != m.held_keys.end(); it++) {
      size_t idx = distance(m.held_keys.begin(), it);
      h ^= (std::hash<char>{}(*it) << idx);
    }
    return h;
  }
};

vector<Maze> reconstruct_path(unordered_map<Maze, Maze> came_from, Maze last_node) {
  vector<Maze> rv{last_node};
  while (came_from.contains(last_node)) {
    last_node = came_from[last_node];
    rv.push_back(last_node);
  }
  return rv;
}

vector<Maze> astar(Maze start) {
  unordered_map<Maze, Maze> came_from;
  unordered_map<Maze, unsigned int> f_score;
  unordered_map<Maze, unsigned int> g_score;
  auto compare = [&f_score](auto a, auto b) { return f_score[a] > f_score[b]; }; // > so that lower f_scores are first in queue
  vector<Maze> open_set{start};

  g_score[start] = 0;
  f_score[start] = start.f_score();
  make_heap(open_set.begin(), open_set.end(), compare);

  while (open_set.size() > 0) {
    pop_heap(open_set.begin(), open_set.end(), compare);
    auto cur_node = open_set.back();
    open_set.pop_back();
    /* cout << "DEBUG: cur_node.f_score = " << cur_node.f_score() << endl; */

    if (cur_node.is_goal()) {
      return reconstruct_path(came_from, cur_node);
    }

    for(auto next_node : cur_node.next_states()) {
      unsigned int node_g_score = g_score[cur_node] + 1;

      if (!g_score.contains(next_node) || node_g_score < g_score[next_node]) {
        /* cout << "DEBUG: pushing next_node.f_score = " << next_node.f_score() << endl; */
        came_from[next_node] = cur_node;
        g_score[next_node] = node_g_score;
        f_score[next_node] = node_g_score + next_node.f_score();
        open_set.push_back(next_node);
        if (find(open_set.begin(), open_set.end(), next_node) != open_set.end()) {
          push_heap(open_set.begin(), open_set.end(), compare);
        }
      }
    }
  }

  return vector<Maze>{}; // failure
}

#ifndef IS_TEST
int main(int argc, char** argv) {
  if (argc < 2) {
    std::cerr << "Provide input filename as argument" << std::endl;
    return 1;
  }
  ifstream fh{argv[1]};
  auto maze = Maze::parse(fh);
  auto p1_path = astar(maze);
  cout << "p1: " << p1_path.size() - 1 << " steps to get all keys" << endl;
  return 0;
}
#endif
