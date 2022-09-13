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

Map = Struct.new(:floor, :amphipods) do
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

    Map.new(floor, amphipods)
  end

  attr_reader :goal_rooms, :steps

  def initialize(*args)
    super

    # hash of type => [positions]
    @goal_rooms =
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
    @steps = []
  end

  def to_s
    y_range = floor.map(&:y).minmax
    x_range = floor.map(&:x).minmax

    ((y_range[0] - 1)..(y_range[1] + 1)).map do |y|
      ((x_range[0] - 1)..(x_range[1] + 1)).map do |x|
        if (a = amphipods.find { |a| a.pos == Point.new(x,y) })
          a.type
        elsif floor.include?(Point.new(x,y))
          "."
        elsif Point.new(x,y).neighbors.any? { |p| floor.include?(p) }
          "#"
        elsif [Point.new(x-1, y-1), Point.new(x+1, y-1), Point.new(x+1, y+1), Point.new(x-1, y+1)].any? { |p| floor.include?(p) } # consider diagnoals to floor
          "#"
        else
          " "
        end
      end.join("")
    end.reverse.join("\n")
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

  def goal?
    amphipods.all? { |a| goal_rooms.fetch(a.type).include?(a.pos) }
  end

  # sum of distances to nearest open goal times energy to move that distance
  def goal_distance
    amphipods.sum do |a|
      if goal_rooms.fetch(a.type).include?(a.pos)
        0
      else
        goal_rooms.fetch(a.type)[0].distance(a.pos) * ENERGIES.fetch(a.type)
      end
    end
  end

  def energy_spent
    steps.sum(&:energy)
  end

  # hash of { cur_pos => { dest_pos => [path] } }
  # path includes cur_pos (i.e. path.count == steps needed)
  def all_paths
    @all_paths ||= Hash[
      floor.map do |pos|
        [pos, paths_from(self, pos)]
      end
    ]
  end

  def clone
    # all ivars are copied by default, so shared memos like all_paths come
    # along. But some values need special handling.
    super.tap do |c|
      c[:amphipods] = amphipods.map(&:clone)
      c.instance_variable_set(:@steps, steps.clone)
    end
  end

  def movable_amphipods
    # never move the just-moved amphipod since we move them as much as possible
    # each time
    steps.empty? ? amphipods : amphipods.reject { |a| a.id == steps.last.amphipod_id }
  end

  def next_states_for_amphipod(a)
    # do a flood fill of all legal reachable floor spots
    other_amphipod_pos = amphipods.reject { |a2| a2 == a }.map(&:pos).to_set
    available_paths = all_paths.fetch(a.pos).reject do |dest, path|
      # DEBUG
      # puts "#{dest} runs into other ampihpods: #{path.any? { |path_pos| other_amphipod_pos.include?(path_pos) }}"
      # puts "#{dest} walks into other goal room: #{goal_rooms.any? { |type, goal_ps| type != a.type && goal_ps.include?(dest) && !goal_ps.include?(a.pos) }}"
      # puts "#{dest} is in hallway, and we're already there: #{hallway?(a.pos) && hallway?(dest)}"
      # END DEBUG
      # can't walk through an amphipod
      path.any? { |path_pos| other_amphipod_pos.include?(path_pos) } ||
        # don't walk into other's goal rooms
        goal_rooms.any? { |type, goal_ps| type != a.type && goal_ps.include?(dest) } ||
        # if walking into goal room, go all the way if you can. Alternatively,
        # don't walk into goal room if you'd block a different type in your own
        # goal room.
        begin
          cur_goal_room = goal_rooms.fetch(a.type)
          # DEBUG
          # puts "#{dest} would be own goal: #{cur_goal_room.include?(dest)}"
          # puts "#{dest} isn't far enough back: #{cur_goal_room.any? { |p| p.y < dest.y && !other_amphipod_pos.include?(p) }}"
          # puts "#{dest} would block other type: #{amphipods.any? { |a2| a2.type != a.type && cur_goal_room.include?(a2.pos) }}"
          # END DEBUG
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
    amphipods.hash
  end

  def ==(other)
    amphipods == other.amphipods
  end
  alias eql? ==
end

# djikstra to get efficient paths from a given floor tile to all other floor tiles
# this is intended to be cached and then re-checked as needed, so it ignores
# current positions of amphipods
def paths_from(map, pos)
  dist = Hash.new(Float::INFINITY)
  prev = {}

  dist[pos] = 0
  queue = [pos]

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
    paths[dest] = [prev[dest]]
    paths[dest] << prev[paths[dest][-1]] while prev[paths[dest][-1]]
  end

  # cut out any always-illegal destinations
  paths.reject! do |dest, path|
    # never stop where you're blocking a room
    map.blocking_room?(dest) ||
      # never stop in a hallway if you started there
      (map.hallway?(pos) && map.hallway?(dest))
  end

  # paths are currently reverse of the logical dir. doesn't actually matter for
  # logic, but makes a diff for debugging and this is all cached so I don't mind
  # the slight inefficiency.
  paths.transform_values(&:reverse)
end

# https://en.wikipedia.org/wiki/A*_search_algorithm
def find_goal(map0)
  open_set = PQueue.new()
  open_set.push(map0, map0.goal_distance)

  came_from = {}
  g_scores = {map0 => 0}
  f_scores = {map0 => map0.goal_distance}

  while open_set.any?
    n = open_set.shift
    # puts "a*: queue=#{open_set.count} n.f_score=#{n.energy_spent + n.goal_distance} n.g_score=#{n.energy_spent}"
    # puts n.to_s

    if n.goal?
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
  map0 = Map.parse(map_str)
  g = find_goal(map0)[-1]
  puts "p1: spent #{g.energy_spent} energy"

  p2_map0 = Map.parse(p2_map(map_str))
  g = find_goal(map0)[-1]
  puts "p2: spent #{g.energy_spent} energy"
end
