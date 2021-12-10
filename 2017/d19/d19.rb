#!/usr/bin/env ruby

Point = Struct.new(:x, :y) do
  def +(other)
    self.class.new(x + other.x, y + other.y)
  end
end

class Maze
  def initialize(filename)
    @layout = File.readlines(filename).map(&:chomp)
    # start pos is always a lone | on first line
    @start_pos = Point.new((0..width).find { |x| @layout[0][x] == "|" }, 0)
  end

  # @layout is [y][x], this is a convenience method to make things less
  # confusing. Also turn nil into " " for consistency
  def [](pt)
    @layout[pt.y]&.[](pt.x) || " "
  end

  def height
    @layout.count
  end

  def width
    @layout[0].length
  end

  # calc [next_pos, next_heading]
  def next_from(pos, heading)
    if self[pos] == "+"
      turns = [
        Point.new(heading.y, heading.x),
        Point.new(heading.y * -1, heading.x * -1),
      ]
      raise StandardError, "Should be exactly 1 turn option" if turns.count { |h| self[pos + h] != " " } != 1
      chosen_heading = turns.find { |h| self[pos + h] != " " }

      [pos + chosen_heading, chosen_heading]
    elsif self[pos + heading] == " " # we reached the end of the maze
      nil
    else
      [pos + heading, heading]
    end
  end

  def walk_to_end
    letters = []
    steps = 1 # start at 1 since our first step is "before" the loop
    # velocity is [cur_pos, heading]. heading starts pointing down (which is y increasing)
    velocity = [@start_pos, Point.new(0, 1)]

    until (velocity = next_from(*velocity)).nil?
      steps += 1
      letters << self[velocity[0]] if /[A-Z]/.match?(self[velocity[0]])
    end

    [letters, steps]
  end
end

maze = Maze.new(ARGV[0])

walked_letters, walked_steps = *maze.walk_to_end
puts "p1: letters encountered: #{walked_letters.join}"
puts "p2: steps taken: #{walked_steps}"
