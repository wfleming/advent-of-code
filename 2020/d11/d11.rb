#!/usr/bin/env ruby

class Map
  include Enumerable

  attr_reader :rows

  def initialize(str)
    @rows = str.lines.map(&:strip)
  end

  def max_x
    rows[0].length - 1
  end

  def max_y
    rows.count - 1
  end

  # yields each x,y coord
  def each
    rows.each_index do |y|
      (0..(rows[0].length - 1)).each do |x|
        yield x, y
      end
    end
  end

  def clone
    self.class.new(rows.join("\n"))
  end

  def ==(other)
    rows == other.rows
  end

  def at(x,y)
    rows[y][x]
  end

  def occupied?(x,y)
    rows[y][x] == "#"
  end

  def sit!(x,y)
    rows[y][x] = "#"
  end

  def leave!(x,y)
    rows[y][x] = "L"
  end

  def occupied_count
    count { |x,y| occupied?(x,y) }
  end
end

class ImmediateNeighborCalculator
  attr_reader :max_x, :max_y

  def initialize(max_x, max_y)
    @max_x = max_x
    @max_y = max_y
  end

  def at(x,y)
    [
      [x - 1, y - 1],
      [x, y - 1],
      [x + 1, y - 1],
      [x - 1, y],
      [x + 1, y],
      [x - 1, y + 1],
      [x, y + 1],
      [x + 1, y + 1],
    ].reject { |pair|
      pair[0] < 0 || pair[1] < 0 || pair[0] > max_x || pair[1] > max_y
    }
  end
end

class LineOfSightNeighborCalculator
  LOS_VECS = [ [-1,-1], [0, -1], [1, -1], [1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0] ].map(&:freeze).freeze

  attr_reader :map

  def initialize(map)
    @map = map
  end

  def at(x, y)
    LOS_VECS.map { |vec| walk_to_chair(x, y, vec) }.compact
  end

  private

  # for los, go in a direction until you hit not-floor (or run off the map)
  def walk_to_chair(x, y, vec)
    x2 = x + vec[0]
    y2 = y + vec[1]

    while x2 >= 0 && y2 >= 0 && x2 <= map.max_x && y2 <= map.max_y
      return [x2, y2] if ["#", "L"].include?(map.at(x2, y2))

      x2 += vec[0]
      y2 += vec[1]
    end

    nil
  end
end

class Simulation
  attr_accessor :state, :threshold, :neighbors

  def initialize(state:, threshold:, neighbor_calculator:)
    @state = state
    @threshold = threshold
    @neighbors = Hash.new() do |hash, key|
      hash[key] = neighbor_calculator.at(key[0], key[1])
    end
  end

  def run_until_stable!
    state2 = next_state

    while state2 != state
      @state = state2
      state2 = next_state
    end
  end

  def next_state
    new_state = state.clone

    state.each do |x,y|
      next if state.at(x,y) == "."

      neighbors_occupied = neighbors[[x,y]].count { |x,y| state.occupied?(x,y) }

      if neighbors_occupied == 0
        new_state.sit!(x,y)
      elsif neighbors_occupied >= threshold
        new_state.leave!(x,y)
      end
    end

    new_state
  end
end

map = Map.new(File.read(ARGV[0]))

# p1
sim_p1 = Simulation.new(
  state: map.clone,
  threshold: 4,
  neighbor_calculator: ImmediateNeighborCalculator.new(map.max_x, map.max_y),
)
sim_p1.run_until_stable!
puts "p1: there are #{sim_p1.state.occupied_count} occupied seats"

# p2
sim_p2 = Simulation.new(
  state: map.clone,
  threshold: 5,
  neighbor_calculator: LineOfSightNeighborCalculator.new(map.clone),
)
sim_p2.run_until_stable!
puts "p2: there are #{sim_p2.state.occupied_count} occupied seats"
