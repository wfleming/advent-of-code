require "pqueue"

def region(p1, p2)
  (p1[0]..p2[0]).flat_map do |x|
    (p1[1]..p2[1]).map do |y|
      [x, y]
    end
  end
end

class Cave
  def self.parse(str)
    lines = str.lines
    depth = lines[0].split[-1].to_i
    target_pieces = lines[1].split(/[ ,]/)
    x = target_pieces[1].to_i
    y = target_pieces[2].to_i

    new(depth, [x, y])
  end

  attr_reader :depth, :target

  def initialize(depth, target)
    @depth = depth
    @target = target
    @geologic_index = Hash.new do |h, pt|
      if pt == [0, 0] || pt == target
        h[pt] = 0
      elsif pt[1] == 0
        h[pt] = pt[0] * 16807
      elsif pt[0] == 0
        h[pt] = pt[1] * 48271
      else
        h[pt] = erosion_level([pt[0] - 1, pt[1]]) * erosion_level([pt[0], pt[1] - 1])
      end
    end
    @erosion_level = Hash.new do |h, pt|
      h[pt] = (geologic_index(pt) + depth) % 20183
    end
  end

  def geologic_index(pt)
    @geologic_index[pt]
  end

  def erosion_level(pt)
    @erosion_level[pt]
  end

  def type(pt)
    @erosion_level[pt] % 3
  end
  alias :risk_level :type
end

def p1(cave)
  pts = region([0, 0], cave.target)
  pts.map(&cave.method(:risk_level)).sum
end

module Djikstra
  # dist is Hash of state -> distance from start state
  # prev is Hash of stateK -> stateV (best V to get to K from)
  # state is the priorit queue of all states
  Trackers = Struct.new(:dist, :prev, :states)

  def self.search(start:, goal:, neighbors_fn:, distance_fn:)
    t = Trackers.new(Hash.new(Float::INFINITY), Hash.new, [])
    t.states = PQueue.new([start]) do |a, b|
      t.dist[b] <=> t.dist[a]
    end
    t.dist[start] = 0
    closed_set = Set.new

    while t.states.size > 0
      u = t.states.pop
      closed_set << u

      break if u == goal

      neighbors_fn.call(u).each do |v|
        t.states << v if !closed_set.include?(v)
        alt = t.dist[u] + distance_fn.call(u, v)
        if alt < t.dist[v]
          t.dist[v] = alt
          t.prev[v] = u
        end
      end
    end

    self.reconstruct_path(t, goal)
  end

  def self.reconstruct_path(tracker, goal)
    current = goal
    path = [current]
    while tracker.prev.key?(current)
      current = tracker.prev[current]
      path << current
    end
    path.reverse
  end
end

class Searcher
  State = Struct.new(:mins, :pt, :equip) do
    # reverse compare by minutes for pqueue
    def <=>(other)
      other.mins <=> mins
    end
  end

  TORCH = :torch
  CLIMBING = :climbing
  NONE = :none

  attr_reader :cave

  def initialize(cave)
    @cave = cave
  end

  def manhattan_distance(state1, state2)
    (state2.pt[0] - state1.pt[0]).abs + (state2.pt[1] - state1.pt[1]).abs
  end

  # time in minutes to move between neighboring states
  def cost(state1, state2)
    d = manhattan_distance(state1, state2)
    if d != 1
      raise ArgumentError, "can't calculate cost of #{state1} -> #{state2}"
    end
    if state1.equip == state2.equip
      d
    else
      d + 7
    end
  end

  def neighbors(state)
    [
      [state.pt[0], state.pt[1] - 1],
      [state.pt[0] - 1, state.pt[1]],
      [state.pt[0] + 1, state.pt[1]],
      [state.pt[0], state.pt[1] + 1],
    ].reject do |pt|
      pt[0] < 0 || pt[1] < 0 ||
        pt[0] > cave.target[0] + 200 ||
        pt[1] > cave.target[1] + 200
    end.flat_map do |pt|
      type = cave.type(pt)
      case type
      when 0 # rocky
        [
          State.new(pt, CLIMBING),
          State.new(pt, TORCH),
        ]
      when 1 # wet
        [
          State.new(pt, CLIMBING),
          State.new(pt, NONE),
        ]
      when 2 # narrow
        [
          State.new(pt, TORCH),
          State.new(pt, NONE),
        ]
      end
    end.reject do |state|
      # you must use the torch in the target square
      state.pt == cave.target && state.equip != TORCH
    end
  end

  def goal
    @goal ||= State.new(cave.target, TORCH)
  end

  def search
    queue = PQueue.new([State.new(0, [0, 0], TORCH)])
    best = Hash.new # [pt, equip] -> mins

    while queue.size > 0
      s = queue.pop
      best_key = [s.pt, s.equip]

      next if best.key?(best_key) && best[best_key] < s.mins

      best[best_key] = s.mins
      return s if best_key == [cave.target, TORCH]

      s2 = follower_state_equip(s)
      queue.push(s2) unless queue.include?(s2)
      follower_states_pos(s).each do |s2|
        queue.push(s2) unless  queue.include?(s2)
      end
    end
  end

  # states in the same pos, but different equipment
  def follower_state_equip(s)
    case cave.type(s.pt)
    when 0 # rocky: use climbing gear or torch
      State.new(s.mins + 7, s.pt, s.equip == CLIMBING ? TORCH : CLIMBING)
    when 1 # wet: use climbing or none
      State.new(s.mins + 7, s.pt, s.equip == CLIMBING ? NONE : CLIMBING)
    when 2 # narrow: use torch or none
      State.new(s.mins + 7, s.pt, s.equip == TORCH ? NONE : TORCH)
    end
  end

  # states in neighboring pos, same equipment
  def follower_states_pos(state)
    [
      [state.pt[0], state.pt[1] - 1],
      [state.pt[0] - 1, state.pt[1]],
      [state.pt[0] + 1, state.pt[1]],
      [state.pt[0], state.pt[1] + 1],
    ].reject do |pt|
      pt[0] < 0 || pt[1] < 0 ||
        pt[0] > cave.target[0] + 100 ||
        pt[1] > cave.target[1] + 100
    end.reject do |pt|
      case cave.type(pt)
      when 0 # rocky, can't use none
        state.equip == NONE
      when 1 # wet, can't use torch
        state.equip == TORCH
      when 2 # narrow, can't use climbing
        state.equip == CLIMBING
      end
    end.map do |pt|
      State.new(state.mins + 1, pt, state.equip)
    end
  end

  def cost_of_path(path)
    (0..(path.count - 2)).map do |i|
      cost(path[i], path[i + 1])
    end.sum
  end
end

if $0 == __FILE__
  cave = Cave.parse(File.read(ARGV[0]))
  puts "p1: risk level is #{p1(cave)}"

  state = Searcher.new(cave).search

  puts "p2: final state #{state}"
end
