#!/usr/bin/env ruby

require "set"
require "../../2015/lib/pqueue"

Point = Struct.new(:x, :y) do
  def to_s
    "(#{x}, #{y})"
  end

  def <=>(other)
    x == other.x ? y <=> other.y : x <=> other.x
  end

  def neighbors
    [
      self.class.new(x - 1, y),
      self.class.new(x + 1, y),
      self.class.new(x, y - 1),
      self.class.new(x, y + 1),
    ]
  end
end

class Maze
  def self.parse(filename)
    robot = nil
    floor = Set.new
    targets = {}

    File.readlines(filename).each_with_index do |line, y|
      line.each_char.with_index do |char, x|
        if char == "."
          floor << Point.new(x, y)
        elsif /\d/.match?(char)
          floor << Point.new(x, y)
          targets[char] = Point.new(x, y)
        end
      end
    end

    new(floor, targets)
  end

  attr_reader :floor, :targets

  def initialize(floor, targets)
    @floor = floor
    @targets = targets
  end

  # each sub-path we need to optimize
  def sub_paths
    @sub_paths ||= targets.flat_map do |id1, pt1|
      targets.reject { |id, _| id == id1 }.map { |id2, pt2| [pt1, pt2] }
    end.map(&:sort).uniq
  end

  def sub_path_distances
    @sub_path_distances ||= Hash[
      sub_paths.map do |sub_path|
        [sub_path, astar_search(MazeState.new(self, sub_path[0], sub_path[1])).count - 1]
      end
    ]
  end

  def best_overall_path
    astar_search(CompoundPath.new(self, []))[-1]
  end

  def best_path_with_return
    astar_search(CompoundPathP2.new(self, []))[-1]
  end
end

MazeState = Struct.new(:maze, :current, :goal) do
  def distance
    (current.x - goal.x).abs + (current.y - goal.y).abs
  end

  def goal?
    current == goal
  end

  def next_states
    current.neighbors.select { |pt| maze.floor.include?(pt) }.map do |np|
      self.class.new(maze, np, goal)
    end
  end

  def edge_weight
    1
  end
end

CompoundPath = Struct.new(:maze, :segments) do
  def to_s
    path = segments.map.with_index do |s, i|
      if i == 0
        "#{s[0]} walk #{s[2]} to #{s[1]},"
      else
        "walk #{s[2]} to #{s[1]}"
      end
    end.join(" ")

    "#{path} (total distance #{total_distance})"
  end

  # each segment is [start_pt, end_pt, steps_taken]
  def distance # segments left to travel
    maze.targets.count - segments.count - 1
  end

  def goal?
    distance == 0
  end

  def total_distance
    segments.sum { |s| s[2] }
  end

  def edge_weight
    segments[-1][2] || 0
  end

  def possible_next_trips
    # path starts empty, first step must start from pos of target 0
    if segments.count == 0
      return maze.sub_path_distances.select do |path, _dist|
        path.include?(maze.targets["0"])
      end.transform_keys do |path|
        path[0] == maze.targets["0"] ? path : path.reverse
      end
    end

    current_pos = segments[-1][1]
    visited = segments.flat_map { |s| s[0..1] }.uniq - [current_pos]
    maze.sub_path_distances.select do |path, _dist|
      !visited.include?(path[0]) && !visited.include?(path[1]) && path.include?(current_pos)
    end.transform_keys do |path|
      # to make connecting things easier, ensure the path is in the direction we
      # want
      path[0] == current_pos ? path : path.reverse
    end
  end

  def next_states
    possible_next_trips.to_a.map do |path, dist|
      self.class.new(maze, segments + [[*path, dist]])
    end
  end
end

class CompoundPathP2 < CompoundPath
  def distance # segments left to travel
    super + 1
  end

  def possible_next_trips
    visited_including_current = segments.flat_map { |s| s[0..1] }.uniq

    if visited_including_current.count == maze.targets.count
      # we've hit everything, we need to return to 0
      current_pos = segments[-1][1]
      maze.sub_path_distances.select do |path, _dist|
        path.include?(maze.targets["0"]) && path.include?(current_pos)
      end.transform_keys do |path|
        path[0] == current_pos ? path : path.reverse
      end
    else
      super
    end
  end
end


def astar_search(init_state)
  open_set = PQueue.new
  open_set.push(init_state, init_state.distance)

  came_from = {}

  g_scores = {init_state => 0}
  f_scores = {init_state => init_state.distance}

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
      g_score = g_scores[n] + next_state.edge_weight
      if g_score < g_scores.fetch(next_state, Float::INFINITY)
        came_from[next_state] = n
        g_scores[next_state] = g_score
        f_scores[next_state] = g_score + next_state.distance
        open_set.push(next_state, f_scores[next_state]) unless open_set.include?(next_state)
      end
    end
  end

  raise StandardError, "couldn't find path to goal"
end

maze = Maze.parse(ARGV[0])

# puts "DEBUG: sub_paths to optimize: #{maze.sub_paths}"
# puts "DEBUG: sub_path distances: #{maze.sub_path_distances.transform_keys { |k| k.map(&:to_s) }}"
puts "p1: best overall path: #{maze.best_overall_path}"
puts "p2: best overall path with return: #{maze.best_path_with_return}"
