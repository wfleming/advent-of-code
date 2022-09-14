#include <algorithm>
#include <fstream>
#include <numeric>
#include <optional>
#include <queue>
#include <set>
#include <sstream>

#include "d23.hpp"

using namespace std;

Point::Point(): x{0}, y{0} {}
Point::Point(int x, int y): x{x}, y{y} {}

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

Amphipod::Amphipod(unsigned int id, char type, Point pos): id{id}, type{type}, pos{pos} {}

static const unordered_map<char, unsigned long> energy_costs {
  { 'A', 1 },
  { 'B', 10 },
  { 'C', 100 },
  { 'D', 1000 },
};

Step::Step(unsigned int amphipod_id, Point from, Point to, unsigned long energy):
      amphipod_id{amphipod_id},
      from{from},
      to{to},
      energy{energy} {}

Map::Map(unordered_set<Point> floor): floor{floor} {}
Map::~Map() { cout << "DEBUG: Map deconstructor" << endl; }

bool Map::is_floor(const Point& pt) const {
  return floor.contains(pt);
}

unordered_map<char, vector<Point>>& Map::goal_rooms() {
  if (_goal_rooms.empty()) {
    cout << "DEBUG: goal_rooms is empty, building it" << endl;
    vector<Point> all_room_tiles{};
    // rooms are the only floor squares with walls to left & right
    cout << "DEBUG: building goal_rooms tile count before copy=" << all_room_tiles.size() << " (all floor=" << floor.size() << ")" << endl;
    copy_if(
      floor.begin(), floor.end(), back_inserter(all_room_tiles),
      [this](auto p) {
        auto left_pt = Point{p.x - 1, p.y}, right_pt = Point{p.x + 1, p.y};
        return !is_floor(left_pt) && !is_floor(right_pt);
      }
    );
    cout << "DEBUG: building goal_rooms tile count after copy=" << all_room_tiles.size() << endl;
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
          all_room_tiles.begin(), all_room_tiles.end(), back_inserter(type_goal_room_tiles),
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
        [&pt](auto p) { return find(p.second.begin(), p.second.end(), pt) != p.second.end(); }
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
          return find(p.second.begin(), p.second.end(), start_point) != p.second.end() &&
            find(p.second.begin(), p.second.end(), dest) != p.second.end();
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

unordered_map<Point, unordered_map<Point, vector<Point>>>& Map::all_paths() {
  if (_all_paths.empty()) {
    for (auto pos : floor) {
      _all_paths[pos] = paths_from(*this, pos);
    }
  }

  return _all_paths;
}

MapState::MapState(shared_ptr<Map> map, vector<Amphipod> amphipods): map{map}, amphipods{amphipods} {}
MapState::MapState(const MapState& other): map{other.map}, amphipods{vector<Amphipod>{other.amphipods}} {}

MapState MapState::parse(istream&& in) {
  unordered_set<Point> floor;
  vector<Amphipod> amphipods;

  // read in all the lines, reverse-ordered
  vector<string> lines;
  string line;
  while (in.good()) {
    getline(in, line);
    if (line.size() > 0) {
      lines.insert(lines.begin(), line);
    }
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

  auto map = Map{floor};
  auto map_p = make_shared<Map>(map);
  return MapState{map_p, amphipods};
}

MapState& MapState::operator=(MapState other) {
  map = other.map;
  amphipods = other.amphipods;
  steps = other.steps;

  return *this;
}

bool MapState::operator==(const MapState& other) const {
  return amphipods == other.amphipods;
}

bool MapState::operator!=(const MapState& other) const {
  return !(*this == other);
}

unsigned long MapState::goal_distance() const {
  return accumulate(
    amphipods.begin(), amphipods.end(), (unsigned long)0,
    [this](auto memo, auto a) {
      auto a_type_goal_room = this->map->goal_rooms().at(a.type);
      if (find(a_type_goal_room.begin(), a_type_goal_room.end(), a.pos) != a_type_goal_room.end()) {
        return memo;
      } else {
        return memo + (a_type_goal_room.front().distance(a.pos) * energy_costs.at(a.type));
      }
    }
  );
}

bool MapState::is_goal() const {
  return all_of(
    amphipods.begin(), amphipods.end(),
    [this](auto a) {
      auto a_type_goal_room = this->map->goal_rooms().at(a.type);
      return find(a_type_goal_room.begin(), a_type_goal_room.end(), a.pos) != a_type_goal_room.end();
    }
  );
}

unsigned long MapState::energy_spent() const {
  return accumulate(steps.begin(), steps.end(), 0, [](auto memo, auto s) { return memo + s.energy; });
}

vector<Amphipod> MapState::movable_amphipods() const {
  if (steps.empty()) {
    return amphipods;
  } else {
    vector<Amphipod> rv;
    copy_if(
        amphipods.begin(), amphipods.end(), back_inserter(rv),
        [this](auto a) { return a.id != steps.back().amphipod_id; }
    );
    return rv;
  }
}

vector<MapState> MapState::next_states_for_amphipod(const Amphipod& amphipod) {
  unordered_set<Point> other_amphipod_positions;
  for (auto a : amphipods) {
    if (a.id != amphipod.id) {
      other_amphipod_positions.insert(a.pos);
    }
  }

  vector<MapState> states;

  for (auto& dest_path : map->all_paths()[amphipod.pos]) {
    // skip any path that would walk into another amphipod
    if (any_of(dest_path.second.begin(), dest_path.second.end(), [other_amphipod_positions](auto p) { return other_amphipod_positions.contains(p); })) {
      continue;
    }

    auto other_goal_room_dest = any_of(
      map->goal_rooms().begin(), map->goal_rooms().end(),
      [amphipod, dest_path](auto goal_pair) {
        return goal_pair.first != amphipod.type && find(goal_pair.second.begin(), goal_pair.second.end(), dest_path.first) != goal_pair.second.end();
      }
    );
    if (other_goal_room_dest) { continue; }


    auto cur_goal_room = map->goal_rooms().at(amphipod.type);
    auto dest_in_goal_room = (find(cur_goal_room.begin(), cur_goal_room.end(), dest_path.first) != cur_goal_room.end());
    if (dest_in_goal_room) {
      for (auto goal_room_pos : cur_goal_room) {
        // don't stop halfway into a room if we can move further
        if (goal_room_pos.y < dest_path.first.y && !other_amphipod_positions.contains(goal_room_pos)) {
          continue;
        } // don't go into our goal room if some other type amphipod is still there
        else if (any_of(amphipods.begin(), amphipods.end(), [&goal_room_pos, &amphipod](auto a){ return a.type != amphipod.type && goal_room_pos == a.pos; })) {
          continue;
        }
      }
    }

    auto new_state = MapState{*this};
    auto new_state_amphipod = find_if(new_state.amphipods.begin(), new_state.amphipods.end(), [amphipod](auto a) { return a.id == amphipod.id; });
    new_state_amphipod->pos = dest_path.first;
    new_state.steps.push_back(Step{amphipod.id, amphipod.pos, dest_path.first, energy_costs.at(amphipod.type) * dest_path.second.size()});

    states.push_back(new_state);
  }

  return states;
}

vector<MapState> MapState::next_states() {
  vector<MapState> rv{};
  for (auto a : movable_amphipods()) {
    auto a_ns = next_states_for_amphipod(a);
    copy(a_ns.begin(), a_ns.end(), back_inserter(rv));
  }
  return rv;
}


// https://en.wikipedia.org/wiki/A*_search_algorithm
optional<MapState> find_goal(const MapState& init_state) {
  unordered_map<MapState, MapState> came_from{};
  unordered_map<MapState, unsigned long> f_scores{ {init_state, 0} };
  unordered_map<MapState, unsigned long> g_scores{ {init_state, init_state.goal_distance()} };
  auto heap_comp = [&f_scores](const MapState& a, const MapState& b) { return f_scores.at(a) > f_scores.at(b); };
  vector<MapState> queue{init_state};
  make_heap(queue.begin(), queue.end(), heap_comp);

  while (!queue.empty()) {
    pop_heap(queue.begin(), queue.end(), heap_comp);
    auto n = queue.back();
    queue.pop_back();

    cout << "DEBUG: find_goal f_score=" << f_scores.at(n) << endl;
    if (n.is_goal()) {
      return optional{n};
    }

    for (auto n_next : n.next_states()) {
      auto g_score = n_next.energy_spent();
      if (!g_scores.contains(n_next) || g_score < g_scores.at(n_next)) {
        came_from.insert_or_assign(n_next, n);
        g_scores.insert_or_assign(n_next, g_score);
        f_scores.insert_or_assign(n_next, g_score + n_next.goal_distance());

        if (find(queue.begin(), queue.end(), n_next) == queue.end()) {
          queue.push_back(n_next);
          push_heap(queue.begin(), queue.end(), heap_comp);
        }
      }
    }
  }

  optional<MapState> no_result{};
  return no_result;
}


#ifndef IS_TEST
int main(int argc, char** argv) {
  if (argc < 2) {
    cerr << "Error: no input filename provided." << endl;
    return 1;
  }
  auto state0 = MapState::parse(ifstream{argv[1]});

  for(auto f : state0.map->floor) {
    cout << f << endl;
  }

  cout << "DEBUG: starting find_goal" << endl;
  auto p1_goal = find_goal(state0);
  cout << "DEBUG: finished find_goal" << endl;

  if (p1_goal) {
    cout << "p1: " << (*p1_goal).energy_spent() << " energy spent" << endl;
  } else {
    cout << "p1: FAILED TO FIND GOAL" << endl;
  }

  return 0;
}
#endif
