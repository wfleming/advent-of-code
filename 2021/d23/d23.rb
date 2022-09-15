#!/usr/bin/env ruby

require_relative "../../2015/lib/pqueue"

Point = Struct.new(:x, :y) do
  def neighbors
    [
      Point.new(x, y - 1),
      Point.new(x, y + 1),
      Point.new(x - 1, y),
      Point.new(x + 1, y),
    ]
  end

  def distance(other)
    (x - other.x).abs + (y - other.y).abs
  end
end

Amphipod = Struct.new(:id, :type, :pos)

ENERGIES = { "A" => 1, "B" => 10, "C" => 100, "D" => 1000 }
Step = Struct.new(:amphipod_id, :from, :to, :energy) do
end

Map = Struct.new(:floor) do
  attr_reader :goal_rooms

  def initialize(*args)
    super

    @steps = []
  end

  # hash of type => [positions]
  def goal_rooms
    @goal_rooms ||=
      begin
        # rooms are the only floor squares with walls to left & right
        rooms = floor.select { |f| !floor.include?(Point.new(f.x - 1, f.y)) && !floor.include?(Point.new(f.x + 1, f.y)) }
        xs = rooms.map(&:x).uniq.sort
        {
          "A" => rooms.select { |r| r.x == xs[0] },
          "B" => rooms.select { |r| r.x == xs[1] },
          "C" => rooms.select { |r| r.x == xs[2] },
          "D" => rooms.select { |r| r.x == xs[3] },
        }
      end
  end

  def blocking_room?(pos)
    @goal_rooms_max_y ||= goal_rooms.values.flatten.map(&:y).max
    @blocking_tiles ||= goal_rooms.values.flatten.map(&:x).map { |x| Point.new(x, @goal_rooms_max_y + 1) }
    @blocking_tiles.include?(pos)
  end

  def hallway?(pos)
    @hallway ||= {}
    @hallway[pos] ||= floor.include?(pos) && !goal_rooms.any? { |_,v| v.include?(pos) }
  end

  def goal_room_entrance_y
    @goal_room_entrance_y ||= goal_rooms.first.last.map(&:y).max
  end

  def goal_room_back_y
    @goal_room_back_y ||= goal_rooms.first.last.map(&:y).min
  end

  # hash of { cur_pos => { dest_pos => [path] } }
  # path includes dest (i.e. path.count == steps needed)
  def all_paths
    @all_paths ||= Hash[
      floor.map do |pos|
        [pos, paths_from(self, pos)]
      end
    ]
  end
end

MapState = Struct.new(:map, :amphipods) do
  def self.parse(str)
    floor = Set.new
    amphipods = []

    str.lines.reverse.each_with_index do |line, y|
      line.each_char.with_index do |c, x|
        if c == "."
          floor << Point.new(x,y)
        elsif /[A-Z]/ =~ c
          floor << Point.new(x,y)
          amphipods << Amphipod.new(amphipods.any? ? amphipods.last.id + 1: 0, c, Point.new(x,y))
        end
      end
    end

    self.new(Map.new(floor), amphipods)
  end

  attr_reader :steps

  def initialize(*args)
    super

    @steps = []
  end

  def to_s
    y_range = map.floor.map(&:y).minmax
    x_range = map.floor.map(&:x).minmax

    ((y_range[0] - 1)..(y_range[1] + 1)).map do |y|
      ((x_range[0] - 1)..(x_range[1] + 1)).map do |x|
        if (a = amphipods.find { |a| a.pos == Point.new(x,y) })
          a.type
        elsif map.floor.include?(Point.new(x,y))
          "."
        elsif Point.new(x,y).neighbors.any? { |p| map.floor.include?(p) }
          "#"
        elsif [Point.new(x-1, y-1), Point.new(x+1, y-1), Point.new(x+1, y+1), Point.new(x-1, y+1)].any? { |p| map.floor.include?(p) } # consider diagonals to floor
          "#"
        else
          " "
        end
      end.join("")
    end.reverse.join("\n")
  end

  def goal?
    amphipods.all? { |a| map.goal_rooms.fetch(a.type).include?(a.pos) }
  end

  # sum of distances to nearest open goal times energy to move that distance
  def goal_distance
    amphipods.sum do |a|
      if map.goal_rooms.fetch(a.type).include?(a.pos)
        0
      else
        # NB: I tried using the all_paths distances instead of manhattan
        # distances. That greatly sped up A* and found a goal faster, but while
        # it was still correct for the example it was the wrong answer on my
        # input.
        map.goal_rooms.fetch(a.type)[0].distance(a.pos) * ENERGIES.fetch(a.type)
      end
    end
  end

  def energy_spent
    steps.sum(&:energy)
  end

  def clone
    # all ivars are copied by default, so shared memos like all_paths come
    # along. But some values need special handling.
    super.tap do |c|
      c[:amphipods] = amphipods.map(&:clone)
      c.instance_variable_set(:@steps, steps.clone)
      c.instance_variable_set(:@hash, nil)
    end
  end

  def movable_amphipods
    # never move the just-moved amphipod since we move them as much as possible
    # each time
    as = steps.empty? ? amphipods : amphipods.reject { |a| a.id == steps.last.amphipod_id }
    # never consider moving an amphipod already in its goal room if not blocking
    # another amphipod
    as.reject do |a|
      cur_goal_room = map.goal_rooms.fetch(a.type)
      cur_goal_room.include?(a.pos) &&
        amphipods.none? { |a2| a2.type != a.type && cur_goal_room.include?(a2.pos) && a2.pos.y < a.pos.y }
    end
  end

  def next_states_for_amphipod(a)
    # do a flood fill of all legal reachable floor spots
    other_amphipod_pos = amphipods.reject { |a2| a2 == a }.map(&:pos).to_set
    available_paths = map.all_paths.fetch(a.pos).reject do |dest, path|
      # can't walk through an amphipod
      path.any? { |path_pos| other_amphipod_pos.include?(path_pos) } ||
        # don't walk into other's goal rooms
        map.goal_rooms.any? { |type, goal_ps| type != a.type && goal_ps.include?(dest) } ||
        # if walking into goal room, go all the way if you can. Alternatively,
        # don't walk into goal room if you'd block a different type in your own
        # goal room.
        begin
          cur_goal_room = map.goal_rooms.fetch(a.type)
          cur_goal_room.include?(dest) && (
            # open floors further back check
            cur_goal_room.any? { |p| p.y < dest.y && !other_amphipod_pos.include?(p) } ||
            # other amphipods in our room
            amphipods.any? { |a2| a2.type != a.type && cur_goal_room.include?(a2.pos) }
          )
        end
    end

    available_paths.map do |dest, path|
      self.clone.tap do |c|
        c.steps << Step.new(a.id, a.pos, dest, ENERGIES.fetch(a.type) * path.count)
        c.amphipods.find { |a2| a2.id == a.id }.pos = dest
      end
    end
  end

  def next_states
    movable_amphipods.flat_map { |a| next_states_for_amphipod(a) }
  end

  def hash
    @hash ||= amphipods.hash
  end

  def ==(other)
    amphipods == other.amphipods
  end
  alias eql? ==
end

# djikstra to get efficient paths from a given floor tile to all other floor tiles
# this is intended to be cached and then re-checked as needed, so it ignores
# current positions of amphipods
def paths_from(map, start_pos)
  dist = Hash.new(Float::INFINITY)
  prev = {}

  dist[start_pos] = 0
  queue = [start_pos]

  while queue.any?
    u = queue.shift

    u.neighbors.select { |p| map.floor.include?(p) }.each do |p|
      new_dist = dist[u] + 1
      if new_dist < dist[p]
        dist[p] = new_dist
        prev[p] = u
        queue << p
      end
    end
  end

  # transform prev hash into { dest => [path] } for each dest
  paths = {}
  prev.keys.each do |dest|
    paths[dest] = [dest]
    paths[dest] << prev[paths[dest][-1]] while prev[paths[dest][-1]] != start_pos
  end

  # cut out any always-illegal destinations
  paths.reject! do |dest, path|
    # never stop where you're blocking a room
    map.blocking_room?(dest) ||
      # never stop in a hallway if you started there
      (map.hallway?(start_pos) && map.hallway?(dest)) ||
      # never start and stop in the same goal room
      map.goal_rooms.any? { |_type, ps| ps.include?(start_pos) && ps.include?(dest) }
  end

  # paths are currently reverse of the logical dir. doesn't actually matter for
  # logic, but makes a diff for debugging and this is only calculated once, so
  # I don't mind the slight inefficiency.
  paths.transform_values(&:reverse)
end

# https://en.wikipedia.org/wiki/A*_search_algorithm
def find_goal(init_state)
  i = 0
  open_set = PQueue.new()
  open_set.push(init_state, init_state.goal_distance)

  came_from = {}
  g_scores = {init_state => 0}
  f_scores = {init_state => init_state.goal_distance}

  while open_set.any?
    n = open_set.shift
    i += 1

    if n.goal?
      puts "found goal after considering #{i} states"
      path = [n]
      while came_from.include?(n)
        n = came_from[n]
        path.unshift(n)
      end
      return path
    end

    n.next_states.each do |next_state|
      g_score = next_state.energy_spent
      if g_score < g_scores.fetch(next_state, Float::INFINITY)
        came_from[next_state] = n
        g_scores[next_state] = g_score
        f_scores[next_state] = g_score + next_state.goal_distance
        open_set.push(next_state, f_scores[next_state]) unless open_set.include?(next_state)
      end
    end
  end

  raise StandardError, "A* couldn't find path to goal"
end

def p2_map(map_str)
  ls = map_str.lines
  ls.insert(-3, "  #D#C#B#A#")
  ls.insert(-3, "  #D#B#A#C#")
  return ls.map(&:rstrip).join("\n")
end

if __FILE__ == $0
  map_str = File.read(ARGV[0])
  mapstate0 = MapState.parse(map_str)
  g = find_goal(mapstate0)[-1]
  puts "p1: spent #{g.energy_spent} energy"

  p2_mapstate0 = MapState.parse(p2_map(map_str))
  g = find_goal(p2_mapstate0)[-1]
  puts "p2: spent #{g.energy_spent} energy"
end
