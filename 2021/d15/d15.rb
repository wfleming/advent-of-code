#!/usr/bin/env ruby

require_relative "../../2015/lib/pqueue"

Point = Struct.new(:x, :y) do
  def neighbors
    [
      self.class.new(x - 1, y),
      self.class.new(x + 1, y),
      self.class.new(x, y - 1),
      self.class.new(x, y + 1),
    ]
  end

  def to_s
    "(#{x}, #{y})"
  end
  alias inspect to_s
end

def parse_grid(str)
  Hash[
    str.lines.flat_map.with_index do |line, y|
      line.chomp.each_char.map.with_index do |c, x|
        [Point.new(x, y), Integer(c)]
      end
    end
  ]
end

class State
  attr_reader :grid, :cur_pos, :goal_pos

  def initialize(grid, cur_pos, goal_pos = nil)
    @grid = grid
    @cur_pos = cur_pos
    @goal_pos = goal_pos || Point.new(
      grid.keys.max_by(&:x).x,
      grid.keys.max_by(&:y).y,
    )
  end

  def to_s
    "<State #{cur_pos}>"
  end
  alias inspect to_s

  # == and hash basically only make sense if you know the states have the same
  # grid/goal_pos, thankfully that's true here even if it's not generalized,
  # and comparing all attrs is much slower, so I'm doing this.
  def ==(other)
    cur_pos == other.cur_pos
  end
  alias eql? ==

  def hash
    # also not generalizable, but safe for this case and a bit faster than
    # cur_pos.hash
    cur_pos.x * 1_000 + cur_pos.y
  end

  def goal?
    cur_pos == goal_pos
  end

  def f_score
    (goal_pos.x - cur_pos.x).abs + (goal_pos.y - cur_pos.y).abs
  end

  def g_score
    @grid[cur_pos]
  end

  def next_states
    cur_pos.neighbors.
      select { |p| grid.include?(p) }.
      map { |p| self.class.new(grid, p, goal_pos) }
  end
end

def expand_grid(grid)
  enlarged_grid = {}

  # build list of [x_offset, y_offset]
  tile_width, tile_height = grid.keys.max_by(&:x).x + 1, grid.keys.max_by(&:y).y + 1
  offsets = 25.times.map do |tile_idx|
      [(tile_idx % 5) * tile_width, (tile_idx / 5) * tile_height]
  end

  grid.each do |k, v|
    offsets.each do |x_offset, y_offset|
      new_v = ((v + y_offset + x_offset - 1) % 9) + 1
      enlarged_grid[Point.new(k.x + x_offset, k.y + y_offset)] = new_v
    end
  end

  enlarged_grid
end

def print_path(path)
  grid = path[0].grid
  hit_positions = path.map(&:cur_pos)
  x_max, y_max = *[grid.keys.max_by(&:x).x + 1, grid.keys.max_by(&:y).y + 1]
  (0..y_max).map do |y|
    (0..x_max).map do |x|
      p = Point.new(x, y)
      c = grid[p]
      hit_positions.include?(p) ? "\e[32m#{c}\e[0m" : c
    end.join
  end.join("\n")
end

# https://en.wikipedia.org/wiki/A*_search_algorithm
def astar_search(init_state)
  open_set = PQueue.new()
  open_set.push(init_state, init_state.f_score)

  came_from = {}
  g_scores = {init_state => 0}
  f_scores = {init_state => init_state.f_score}

  while open_set.any?
    n = open_set.shift

    if n.goal?
      path = [n]
      while came_from.include?(n)
        n = came_from[n]
        path.unshift(n)
      end
      return path
    end

    n.next_states.each do |next_state|
      g_score = g_scores[n] + next_state.g_score
      if g_score < g_scores.fetch(next_state, Float::INFINITY)
        came_from[next_state] = n
        g_scores[next_state] = g_score
        f_scores[next_state] = g_score + next_state.f_score
        open_set.push(next_state, f_scores[next_state]) unless open_set.include?(next_state)
      end
    end
  end

  raise StandardError, "couldn't find path to goal"
end

grid = parse_grid(File.read(ARGV[0]))

path = astar_search(State.new(grid, Point.new(0, 0)))
path_risk = path[1..].sum(&:g_score)

# puts print_path(path)
puts "p1: path found takes #{path.count - 1} steps, has risk of #{path_risk}"

grid2 = expand_grid(grid)
path2 = astar_search(State.new(grid2, Point.new(0, 0)))
path2_risk = path2[1..].sum(&:g_score)
puts "p2: path found takes #{path2.count - 1} steps, has risk of #{path2_risk}"
