#!/usr/bin/env ruby

require "set"

# grid increases x to the left and y upwards
Point = Struct.new(:x, :y) do
  def +(other)
    self.class.new(x + other.x, y + other.y)
  end
end

Virus = Struct.new(:pos, :dir) do
  def turn_left
    # sin90 = 1, cos90 = 0
    # x = cos * x - sin * y
    # y = sin * x + cos * y
    self.class.new(pos, Point.new(0 - dir.y, dir.x))
  end

  def turn_right
    # sin270 = -1, cos270 = 0
    self.class.new(pos, Point.new(dir.y, -1 * dir.x))
  end
end

Map = Struct.new(:infected, :virus) do
  def self.parse(lines)
    # virus starts in the middle
    height, width = *[lines.count, lines[0].length]
    # reverse the lines so y is going up
    infected = lines.reverse.flat_map.with_index do |line, y|
      line.each_char.map.with_index do |c, x|
        Point.new(x, y) if c == "#"
      end
    end.compact.to_set

    new(infected, Virus.new(Point.new((width - 1) / 2, (height - 1) / 2), Point.new(0, 1)))
  end

  def step
    new_virus = virus.clone
    new_infected = infected.clone
    if infected.include?(virus.pos)
      new_virus = new_virus.turn_right
      new_infected.delete(new_virus.pos)
    else
      new_virus = new_virus.turn_left
      new_infected << new_virus.pos
    end
    new_virus.pos = new_virus.pos + new_virus.dir
    self.class.new(new_infected, new_virus)
  end

  def to_s
    x_range = (infected + [virus.pos]).minmax_by(&:x).map(&:x)
    y_range = (infected + [virus.pos]).minmax_by(&:y).map(&:y)

    (y_range[1]..y_range[0]).step(-1).map do |y|
      (x_range[0]..x_range[1]).map do |x|
        c = infected.include?(Point.new(x, y)) ? "#" : "."
        virus.pos == Point.new(x, y) ? "\e[31m#{c}\e[0m" : c
      end.join
    end.join("\n")
  end
end

def count_infections(iters, map)
  infections = 0
  iters.times do
    next_map = map.step
    infections += 1 if next_map.infected.include?(map.virus.pos) && !map.infected.include?(map.virus.pos)
    map = next_map
  end
  infections
end

if __FILE__ == $0
  map = Map.parse(File.readlines(ARGV[0]))

  bursts = 10_000
  puts "p1: in #{bursts} bursts, #{count_infections(bursts, map)} infections occurred"
end
