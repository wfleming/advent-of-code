#include <algorithm>
#include <fstream>
#include <set>
#include <sstream>
#include <vector>
#include <queue>

#include "d23.hpp"

using namespace std;

Point::Point(): x{0}, y{0} {}; // needed for unordered_map[]=
Point::Point(int x, int y): x{x}, y{y} {};

Point::operator std::string() const {
    ostringstream s;
    s << '(' << x << ',' << y << ')';
  return s.str();
}

bool Point::operator==(const Point& other) const {
  return other.x == x && other.y == y;
}

bool Point::operator!=(const Point& other) const {
  return !(*this == other);
}

unsigned int Point::distance(const Point& other) const {
  return abs(x - other.x) + abs(y - other.y);
}

vector<Point> Point::neighbors() const {
  vector<Point> v{
    Point{x - 1, y},
    Point{x + 1, y},
    Point{x, y - 1},
    Point{x, y + 1},
  };
  return v;
}

ostream& operator<<(ostream& os, const Point& pt) {
  return os << (string)pt;
}

Amphipod::Amphipod(unsigned int id, char type, Point pos): id{id}, type{type}, pos{pos} {};
Amphipod::Amphipod(const Amphipod &other): id{other.id}, type{other.type}, pos{other.pos} {};

static const unordered_map<char, unsigned int> energy_costs {
  { 'A', 1 },
  { 'B', 10 },
  { 'C', 100 },
  { 'D', 1000 },
};

Step::Step(unsigned int amphipod_id, Point from, Point to, unsigned int energy):
      amphipod_id{amphipod_id},
      from{from},
      to{to},
      energy{energy} {};

Map::Map(unordered_set<Point> floor): floor{floor} {};

bool Map::is_floor(const Point& pt) const {
  return *find(floor.begin(), floor.end(), pt) == pt;
}

unordered_map<char, vector<Point>> Map::goal_rooms() {
  if (_goal_rooms.empty()) {
    vector<Point> all_room_tiles;
    // rooms are the only floor squares with walls to left & right
    copy_if(
      floor.begin(), floor.end(), all_room_tiles.begin(),
      [this](auto p) {
        auto left_pt = Point{p.x - 1, p.y}, right_pt = Point{p.x + 1, p.y};
        return !is_floor(left_pt) && !is_floor(right_pt);
      }
    );
    vector<char> types{'A', 'B', 'C', 'D'};
    vector<int> x_vals{};

    for (auto p : all_room_tiles) {
      if (find(x_vals.begin(), x_vals.end(), p.x) == x_vals.end()) {
        x_vals.push_back(p.x);
      }
    }
    sort(x_vals.begin(), x_vals.end());

    for (size_t i = 0; i < types.size(); i++) {
      vector<Point> type_goal_room_tiles{};
      copy_if(
          all_room_tiles.begin(), all_room_tiles.end(), type_goal_room_tiles.begin(),
          [&x_vals, &i](auto p) { return p.x == x_vals[i]; }
      );
      _goal_rooms[types[i]] = type_goal_room_tiles;
    }
  }

  return _goal_rooms;
}

bool Map::is_blocking_room(const Point& pt) {
  if (_room_blocking_tiles.empty()) {
    vector<int> xs{};
    int max_y = -1;
    for (auto pair : goal_rooms()) {
      for (auto pos : pair.second) {
        if (find(xs.begin(), xs.end(), pos.x) == xs.end()) {
          xs.push_back(pos.x);
        }

        if (pos.y > max_y) {
          max_y = pos.y;
        }
      }
    }

    for (auto x : xs) {
      _room_blocking_tiles.insert(Point{x, max_y + 1});
    }
  }

  return _room_blocking_tiles.contains(pt);
}

bool Map::is_hallway(const Point& pt) {
  if (!_hallway_cache.contains(pt)) {
    bool v = floor.contains(pt) &&
      none_of(
        goal_rooms().begin(), goal_rooms().end(),
        [&pt](auto p) { return *find(p.second.begin(), p.second.end(), pt) == pt; }
      );

    _hallway_cache[pt] = v;
  }

  return _hallway_cache[pt];
}

// djikstra to get efficient paths from a given floor tile to all other floor tiles
// this is intended to be cached and then re-checked as needed, so it ignores
// current positions of amphipods
unordered_map<Point, vector<Point>> paths_from(Map& map, const Point& start_point) {
  unordered_map <Point, int> dist{};
  unordered_map<Point, Point> prev{};
  queue<Point> queue{};
  queue.push(start_point);

  dist[start_point] = 0;

  // do djikstra to build the graph
  while (queue.size() > 0) {
    auto pt = queue.front();
    queue.pop();

    for (auto n : pt.neighbors()) {
      if (!map.is_floor(n)) { continue; }
      auto n_dist = dist[pt] + 1;
      if (!dist.contains(n) || n_dist < dist[n]) {
        dist[n] = n_dist;
        prev[n] = pt;
        queue.push(n);
      }
    }
  }

  // build that into a map of dest -> path
  unordered_map<Point, vector<Point>> paths;

  for (auto pair : prev) {
    auto dest = pair.first;

    if (map.is_blocking_room(dest)) {
      continue;
    } else if (map.is_hallway(start_point) && map.is_hallway(dest)) {
      continue;
    } else if (any_of(
        map.goal_rooms().begin(), map.goal_rooms().end(),
        [&dest, &start_point](auto p) {
          return *find(p.second.begin(), p.second.end(), start_point) == start_point &&
            *find(p.second.begin(), p.second.end(), dest) == dest;
        })
      ) {
      continue;
    }

    vector<Point> path{dest};
    while(prev[path.back()] != start_point) {
      path.push_back(prev[path.back()]);
    }

    paths[dest] = path;
  }

  return paths;
}

unordered_map<Point, unordered_map<Point, vector<Point>>> Map::all_paths() {
  if (_all_paths.empty()) {
    for (auto pos : floor) {
      _all_paths[pos] = paths_from(*this, pos);
    }
  }

  return _all_paths;
}

MapState::MapState(Map map, vector<Amphipod> amphipods): map{map}, amphipods{amphipods} {};
MapState::MapState(const MapState& other): map{other.map}, amphipods{vector<Amphipod>{other.amphipods}} {};

MapState MapState::parse(istream&& in) {
  unordered_set<Point> floor;
  vector<Amphipod> amphipods;

  // read in all the lines, reverse-ordered
  vector<string> lines;
  string line;
  while (in.good()) {
    getline(in, line);
    lines.insert(lines.begin(), line);
  }

  // iterate over the lines we read and construct the data
  unsigned int aid = 0;
  for (unsigned int y = 0; y < lines.size(); y++) {
    line = lines[y];
    for (unsigned int x = 0; x < line.size(); x++) {
      char c = line[x];
      Point p = Point(x,y);
      if (c == '.') {
        floor.insert(p);
      } else if (c == '@') {
        floor.insert(p);
      } else if (isupper(c)) {
        floor.insert(p);
        amphipods.push_back(Amphipod{aid, c, p});
        aid++;
      }
    }
  }

  return MapState{Map{floor}, amphipods};
}


#ifndef IS_TEST
int main(int argc, char** argv) {
  if (argc < 2) {
    std::cerr << "Error: no input filename provided." << std::endl;
    return 1;
  }
  auto state0 = MapState::parse(ifstream{argv[1]});

  /* auto p1path = part1(maze); */
  /* cout << "p1: " << p1path.steps << " steps"; */
  return 0;
}
#endif
