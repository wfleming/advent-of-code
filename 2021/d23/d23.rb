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
Step = Struct.new(:amphipod, :from, :to, :energy) do
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
    floor.include?(pos) && !goal_rooms.any? { |_,v| v.include?(pos) }
  end

  def goal?
    amphipods.all? { |a| goal_rooms.fetch(a.type).include?(a.pos) }
  end

  # sum of distances to nearest open goal times energy to move that distance
  def goal_distance
    amphipods.map do |a|
      if goal_rooms.fetch(a.type).include?(a.pos)
        0
      else
        goal_rooms.fetch(a.type)[0].distance(a.pos) * ENERGIES.fetch(a.type)
      end
    end.sum
  end

  def energy_spent
    steps.sum(&:energy)
  end

  def clone
    super.tap do |c|
      c[:amphipods] = amphipods.map(&:clone)
      c.instance_variable_set(:@steps, steps.clone)
    end
  end

  def movable_amphipods
    # never move the just-moved amphipod since we move them as much as possible
    # each time
    steps.empty? ? amphipods : amphipods.reject { |a| a.id == steps.last.amphipod.id }
  end

  def next_states_for_amphipod(a)
    # do a flood fill of all legal reachable floor spots
    destinations = []
    dest_queue = [a.pos]
    while dest_queue.any?
      n = dest_queue.shift
      n_next = n.neighbors.reject do |p|
        # basic "is this spot a floor and unoccupied" check
        !floor.include?(p) || amphipods.any? { |a2| a2.pos == p } ||
          # don't walk into others goal rooms (unless you're already there)
          goal_rooms.any? { |type, ps| type != a.type && ps.include?(p) && !ps.include?(a.pos) } ||
          # don't re-walk already-found destinations
          destinations.find { |d| d[:pos] == p } || dest_queue.include?(p)
      end
      dest_queue += n_next
      n_cost = destinations.find { |d| d[:pos] == n }&.[](:steps) || 0
      # TODO - do I need to update add an already-seen dest with lower cost?
      n_next.each do |nn|
        destinations << { pos: nn, steps: n_cost + 1 }
      end
    end
    # rules to reject destinations that were valid transient steps:
    destinations = destinations.reject do |pair|
      # don't block rooms
      blocking_room?(pair[:pos])
    end.reject do |pair|
      # if walking into home room, go all the way if you can. Alternatively,
      # don't walk into home room if you'd block a different type in your own
      # home room.
      # TODO - this isn't fully correct for part 2, rooms are bigger
      goal_room_entrance = goal_rooms.fetch(a.type).max_by(&:y)
      goal_room_back = goal_rooms.fetch(a.type).min_by(&:y)
      pair[:pos] == goal_room_entrance && (
        amphipods.none? { |a2| a2.pos == goal_room_back } ||
        amphipods.any? { |a2| a2.type != a.type && a2.pos == goal_room_back }
      )
    end.reject do |pair|
      # if in the hallway right now, don't move to other hallway spots
      hallway?(a.pos) && hallway?(pair[:pos])
    end

    # puts "#{destinations.count} possible destinations for amphipod #{a}"
    # puts to_s
    destinations.map do |pair|
      self.clone.tap do |c|
        c.steps << Step.new(a, a.pos, pair[:pos], ENERGIES.fetch(a.type) * pair[:steps])
        c.amphipods[c.amphipods.find_index(a)].pos = pair[:pos]
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
  g = find_goal(map0)
  puts "p1: spent #{g[-1].energy_spent} energy"

  p2_map0 = Map.parse(p2_map(map_str))
  g = find_goal(map0)
  puts "p2: spent #{g[-1].energy_spent} energy"
end
