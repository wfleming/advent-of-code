#!/usr/bin/env ruby

require_relative "../../2015/lib/pqueue"

ENERGIES = { "A" => 1, "B" => 10, "C" => 100, "D" => 1000 }
Step = Struct.new(:from, :to, :energy)

MapState = Struct.new(:map) do
  def self.parse(str)
    self.new(str.lines.map(&:rstrip).reverse)
  end

  attr_reader :steps

  def initialize(*args)
    super

    @steps = []
  end

  def to_s
    map.reverse.join("\n")
  end

  def goal_rooms
    @goal_rooms ||=
      begin
        # rooms are the only floor squares with walls to left & right
        rooms = map.each_with_index.flat_map do |line, y|
          line.each_char.with_index.map do |c, x|
            if /[A-D.]/.match?(c) && line[x-1] == "#" && line[x+1] == "#"
              [x, y]
            end
          end
        end.compact

        xs = rooms.map(&:first).uniq.sort
        {
          "A" => rooms.select { |r| r[0] == xs[0] },
          "B" => rooms.select { |r| r[0] == xs[1] },
          "C" => rooms.select { |r| r[0] == xs[2] },
          "D" => rooms.select { |r| r[0] == xs[3] },
        }
      end
  end

  # hash of { cur_pos => { dest_pos => [path] } }
  # path includes dest (i.e. path.count == steps needed)
  def all_paths
    @all_paths ||=
      begin
        rv = Hash.new

        map.each_with_index do |line, y|
          line.each_char.with_index do |c, x|
            if /[A-D.]/.match?(c)
              rv[[x,y]] = paths_from(self, [x,y])
            end
          end
        end

        rv
      end
  end

  def goal?
    goal_rooms.all? do |type, positions|
      positions.all? { |pos| map[pos[1]][pos[0]] == type }
    end
  end

  def blocking_room?(pos)
    @goal_rooms_max_y ||= goal_rooms.values.flatten(1).map(&:last).max
    @blocking_tiles ||= goal_rooms.values.flatten(1).map(&:first).map { |x| [x, @goal_rooms_max_y + 1] }
    @blocking_tiles.include?(pos)
  end

  # if a pos is a floor. Could be an occupied floor, still floor.
  def floor?(pos)
    map[pos[1]][pos[0]].match?(/[A-D.]/)
  end

  def hallway?(pos)
    @hallway ||= {}
    @hallway[pos] ||= map[pos[1]][pos[0]].match?(/[A-D.]/) &&
      !goal_rooms.any? { |_,v| v.include?(pos) }
  end

  # sum of distances to nearest open goal times energy to move that distance
  def goal_distance
    map.each_with_index.sum do |line, y|
      line.each_char.with_index.sum do |c, x|
        if /[A-D]/.match?(c)
          goals = goal_rooms.fetch(c)
          if goals.include?([x,y])
            0
          else
            d = (goals[0][0] - x).abs + (goals[0][1] - y).abs
            d * ENERGIES.fetch(c)
          end
        else
          0
        end
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
      c[:map] = map.map(&:clone)
      c.instance_variable_set(:@steps, steps.clone)
      c.instance_variable_set(:@hash, nil)
      c.instance_variable_set(:@amphipods, nil)
    end
  end

  # array of positions
  def amphipods
    @amphipods ||= map.each_with_index.flat_map do |line, y|
      line.each_char.with_index.map do |c, x|
        [x,y] if c.match?(/[A-D]/)
      end
    end.compact
  end

  # same array of positions, filtered
  def movable_amphipods
    # never move the just-moved amphipod since we move them as much as possible
    # each time. Also don't move amphipods that are already home (and not
    # blocking another amphipod that needs to get out)
    amphipods.reject do |pos|
      type = map[pos[1]][pos[0]]
      cur_goal_room = goal_rooms.fetch(type)
      (steps.last&.to == pos) ||
        cur_goal_room.include?(pos) &&
          amphipods.none? { |pos2| map[pos2[1]][pos2[0]] != type && cur_goal_room.include?(pos2) && pos2[1] < pos[1] }
    end
  end

  def next_states_for_amphipod(a_pos)
    a_type = map[a_pos[1]][a_pos[0]]
    other_amphipod_pos = amphipods.reject { |p| p == a_pos }.to_set
    available_paths = all_paths.fetch(a_pos).reject do |dest, path|
      # can't walk through an amphipod
      path.any? { |path_pos| other_amphipod_pos.include?(path_pos) } ||
        # don't walk into other's goal rooms
        goal_rooms.any? { |type, goal_ps| type != a_type && goal_ps.include?(dest) } ||
        # if walking into goal room, go all the way if you can. Alternatively,
        # don't walk into goal room if you'd block a different type in your own
        # goal room.
        begin
          cur_goal_room = goal_rooms.fetch(a_type)
          cur_goal_room.include?(dest) && (
            # open floors further back check
            cur_goal_room.any? { |p| p[1] < dest[1] && !other_amphipod_pos.include?(p) } ||
            # other amphipods in our room
            amphipods.any? { |pos2| map[pos2[1]][pos2[0]] != a_type && cur_goal_room.include?(pos2) }
          )
        end
    end

    available_paths.map do |dest, path|
      self.clone.tap do |c|
        c.steps << Step.new(a_pos, dest, ENERGIES.fetch(a_type) * path.count)
        c.map[a_pos[1]][a_pos[0]] = "."
        c.map[dest[1]][dest[0]] = a_type
      end
    end
  end

  def next_states
    movable_amphipods.flat_map { |pos| next_states_for_amphipod(pos) }
  end

  def hash
    @hash ||= map.hash
  end

  def ==(other)
    map == other.map
  end
  alias eql? ==
end

def neighbors(pt)
  [
    [pt[0] - 1, pt[1]],
    [pt[0] + 1, pt[1]],
    [pt[0], pt[1] - 1],
    [pt[0], pt[1] + 1],
  ].reject { |p| p[0] < 0 || p[1] < 0 }
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

    neighbors(u).select { |p| map.floor?(p) }.each do |p|
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
    blocking_room?(dest) ||
      # never stop in a hallway if you started there
      (hallway?(start_pos) && hallway?(dest)) ||
      # never start and stop in the same goal room
      goal_rooms.any? { |_type, ps| ps.include?(start_pos) && ps.include?(dest) }
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

  raise StandardError, "A* couldn't find path to goal (tested #{i} states)"
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
