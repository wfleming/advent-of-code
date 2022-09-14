#include <iostream>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <memory>

using namespace std;

typedef pair<int, int> point;

struct Point {
  int x, y;
  Point(); // needed for unordered_map[]=
  Point(int x, int y);

  operator std::string() const;
  bool operator==(const Point& other) const;
  bool operator!=(const Point& other) const;
  unsigned int distance(const Point& other) const;
  vector<Point> neighbors() const;
};

template<> struct std::hash<Point> {
  size_t operator()(const Point& p) const noexcept {
    return p.x ^ (p.y << 1);
  }
};

class Amphipod {
  public:
    unsigned int id;
    char type;
    Point pos;

    Amphipod(unsigned int id, char type, Point pos);

    bool operator==(const Amphipod& other) const = default;
    bool operator!=(const Amphipod& other) const = default;
};

template<> struct std::hash<Amphipod> {
  size_t operator()(const Amphipod& a) const noexcept {
    return a.id ^ (hash<char>{}(a.type) << 1) ^ (hash<Point>{}(a.pos) << 2);
  }
};

template<> struct std::hash<vector<Amphipod>> {
  // https://stackoverflow.com/questions/20511347/a-good-hash-function-for-a-vector
  std::size_t operator()(std::vector<Amphipod> const& vec) const {
    std::size_t seed = vec.size();
    for(auto x : vec) {
      auto xh = hash<Amphipod>{}(x);
      xh = ((xh >> 16) ^ xh) * 0x45d9f3b;
      xh = ((xh >> 16) ^ xh) * 0x45d9f3b;
      xh = (xh >> 16) ^ xh;
      seed ^= xh + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};

class Step {
  public:
    unsigned int amphipod_id;
    Point from, to;
    unsigned long energy;

    Step(unsigned int amphipod_id, Point from, Point to, unsigned long energy);
};

class Map {
  public:
    unordered_set<Point> floor;

    Map(unordered_set<Point> floor);
    ~Map(); // DEBUG

    unordered_map<Point, unordered_map<Point, vector<Point>>>& all_paths();
    unordered_map<char, vector<Point>>& goal_rooms();
    bool is_floor(const Point& pt) const;
    bool is_blocking_room(const Point& pt);
    bool is_hallway(const Point& pt);

  private:
    unordered_map<Point, bool> _hallway_cache = unordered_map<Point, bool>{};
    unordered_map<Point, unordered_map<Point, vector<Point>>> _all_paths = unordered_map<Point, unordered_map<Point, vector<Point>>>{};
    unordered_set<Point> _room_blocking_tiles = unordered_set<Point>{};
    unordered_map<char, vector<Point>> _goal_rooms = unordered_map<char, vector<Point>>{};
};

class MapState {
  public:
    shared_ptr<Map> map;
    vector<Amphipod> amphipods;
    vector<Step> steps;

    MapState(shared_ptr<Map> map, vector<Amphipod> amphipods);
    MapState(const MapState& other);

    static MapState parse(istream&& in);

    MapState& operator=(MapState other);
    bool operator==(const MapState& other) const;
    bool operator!=(const MapState& other) const;

    unsigned long goal_distance() const;
    bool is_goal() const;
    unsigned long energy_spent() const;
    vector<Amphipod> movable_amphipods() const;
    vector<MapState> next_states_for_amphipod(const Amphipod& amphipod);
    vector<MapState> next_states();
};

template<> struct std::hash<MapState> {
  size_t operator()(const MapState& m) const noexcept {
    return hash<vector<Amphipod>>{}(m.amphipods);
  }
};

unordered_map<Point, vector<Point>> paths_from(Map& map, const Point& start_point);
