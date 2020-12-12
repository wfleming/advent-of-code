#!/usr/bin/env ruby

Point = Struct.new(:x, :y) do
  def distance(other)
    (other.x - x).abs + (other.y - y).abs
  end

  def +(other)
    self.class.new(x + other.x, y + other.y)
  end

  def *(scale)
    self.class.new(x * scale, y * scale)
  end
end

Ship = Struct.new(:heading, :position) do
  INSTRUCTION_PAT = /^(?<action>[NSWERLF])(?<amount>\d+)/
  ROT_DIRS = ["N", "E", "S", "W"].freeze # in the order of clockwise rotation

  def navigate_all!(lines)
    lines.each(&method(:navigate_step!))
  end

  def navigate_step!(line)
    m = INSTRUCTION_PAT.match(line)
    case m["action"]
    when "N", "S", "W", "E"
      self.position += direction_vec(m["action"]) * Integer(m["amount"])
    when "F"
      self.position += direction_vec(heading) * Integer(m["amount"])
    when "R"
      self.heading = rotate(heading, Integer(m["amount"]))
    when "L"
      self.heading = rotate(heading, 0 - Integer(m["amount"]))
    else
      raise ArgumentError, "invalid action in #{line}"
    end
  end

  def direction_vec(dir)
    case dir
    when "N"
      Point.new(0, 1)
    when "S"
      Point.new(0, -1)
    when "W"
      Point.new(-1, 0)
    when "E"
      Point.new(1, 0)
    else
      raise ArgumentError, "invalid direction #{dir}"
    end
  end

  def rotate(dir, amt)
    raise ArgumentError, "can only turn 90 degree increments" unless (amt % 90).zero?

    steps = amt / 90
    i = ROT_DIRS.find_index { |d| d == dir }
    ROT_DIRS[(i + steps) % ROT_DIRS.count]
  end
end

# p1
origin = Point.new(0, 0)
s = Ship.new("E", origin)
s.navigate_all!(File.readlines(ARGV[0]))
puts "p1: ship ended up at #{s.position}, distance=#{origin.distance(s.position)}"
