#!/usr/bin/env ruby

class Map
  include Enumerable

  attr_reader :rows

  def initialize(str)
    @rows = str.lines.map(&:strip)
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

  def neighbors(x,y)
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
      pair[0] < 0 || pair[1] < 0 || pair[0] >= rows[0].length || pair[1] >= rows.count
    }
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
end

class Simulation
  attr_reader :state

  def initialize(state)
    @state = state
  end

  def next_state
    new_state = state.clone

    state.each do |x,y|
      next if state.at(x,y) == "."

      neighbors_occupied = state.neighbors(x,y).select { |x,y|
        state.occupied?(x,y)
      }.count

      if neighbors_occupied == 0
        new_state.sit!(x,y)
      elsif neighbors_occupied >= 4
        new_state.leave!(x,y)
      end
    end

    new_state
  end

  def run_until_stable!
    state2 = next_state

    while state2 != state
      @state = state2
      state2 = next_state
    end
  end

  def occupied_count
    state.select { |x,y| state.occupied?(x,y) }.count
  end
end

map = Map.new(File.read(ARGV[0]))
sim_p1 = Simulation.new(map)
sim_p1.run_until_stable!
puts "p1: there are #{sim_p1.occupied_count} occupied seats"
