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

  def reverse
    self.class.new(pos, Point.new(0 - dir.x, 0 - dir.y))
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

  def step!
    if infected.include?(virus.pos)
      virus.dir = virus.turn_right.dir
      infected.delete(virus.pos)
    else
      virus.dir = virus.turn_left.dir
      infected << virus.pos
    end
    virus.pos = virus.pos + virus.dir
  end

  def infected?(pt)
    infected.include?(pt)
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

Map2 = Struct.new(:nodes, :virus) do
  def self.from_map(map)
    nodes = Hash[map.infected.map { |p| [p, :infected] }]
    new(nodes, map.virus.clone)
  end

  def infected?(pt)
    nodes[pt] == :infected
  end

  def step!
    case nodes[virus.pos]
    when :infected
      virus.dir = virus.turn_right.dir
      nodes[virus.pos] = :flagged
    when :weakened
      # no change in direction
      nodes[virus.pos] = :infected
    when :flagged
      virus.dir = virus.reverse.dir
      nodes[virus.pos] = :clean
    else
      virus.dir = virus.turn_left.dir
      nodes[virus.pos] = :weakened
    end
    virus.pos = virus.pos + virus.dir
  end
end

def count_infections(iters, map)
  infections = 0
  iters.times do |i|
    # puts "DEBUG: i=#{i} (#{((i.to_f / iters) * 100).round(2)}%)" if i % 100_000 == 0
    cur_pos = map.virus.pos
    is_infected = map.infected?(cur_pos)
    map.step!
    infections += 1 if !is_infected && map.infected?(cur_pos)
  end
  infections
end

if __FILE__ == $0
  map = Map.parse(File.readlines(ARGV[0]))

  bursts = 10_000
  puts "p1: in #{bursts} bursts, #{count_infections(bursts, map)} infections occurred"

  map2 = Map2.from_map(Map.parse(File.readlines(ARGV[0])))
  bursts = 10_000_000
  puts "p2: in #{bursts} bursts, #{count_infections(bursts, map2)} infections occurred"
end
