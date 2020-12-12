#!/usr/bin/env ruby

INSTRUCTION_PAT = /^(?<action>[NSWERLF])(?<amount>\d+)/

Point = Struct.new(:x, :y) do
  def distance(other)
    (other.x - x).abs + (other.y - y).abs
  end

  def +(other)
    self.class.new(x + other.x, y + other.y)
  end

  def -(other)
    self.class.new(x - other.x, y - other.y)
  end

  def *(scale)
    self.class.new(x * scale, y * scale)
  end
end

def vec_from_dir(dir)
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

Ship = Struct.new(:heading, :position) do
  ROT_DIRS = ["N", "E", "S", "W"].freeze # in the order of clockwise rotation

  def navigate_all!(lines)
    lines.each(&method(:navigate_step!))
  end

  def navigate_step!(line)
    m = INSTRUCTION_PAT.match(line)
    case m["action"]
    when "N", "S", "W", "E"
      self.position += vec_from_dir(m["action"]) * Integer(m["amount"])
    when "F"
      self.position += vec_from_dir(heading) * Integer(m["amount"])
    when "R"
      self.heading = rotate(heading, Integer(m["amount"]))
    when "L"
      self.heading = rotate(heading, 0 - Integer(m["amount"]))
    else
      raise ArgumentError, "invalid action in #{line}"
    end
  end

  def rotate(dir, amt)
    raise ArgumentError, "can only turn 90 degree increments" unless (amt % 90).zero?

    steps = amt / 90
    i = ROT_DIRS.find_index { |d| d == dir }
    ROT_DIRS[(i + steps) % ROT_DIRS.count]
  end
end

Ship2 = Struct.new(:position, :waypoint) do
  def navigate_all!(lines)
    lines.each(&method(:navigate_step!))
  end

  def navigate_step!(line)
    m = INSTRUCTION_PAT.match(line)
    case m["action"]
    when "N", "S", "W", "E"
      self.waypoint += vec_from_dir(m["action"]) * Integer(m["amount"])
    when "F"
      self.position += waypoint * Integer(m["amount"])
    when "R"
      self.waypoint = rotate(waypoint, 360 - Integer(m["amount"]))
    when "L"
      self.waypoint = rotate(waypoint, Integer(m["amount"]))
    else
      raise ArgumentError, "invalid action in #{line}"
    end
  end

  # waypoint is always treated relative to position - it's a vector, not an
  # absolute position. So we can just rotate it relative to (0,0), not the ship's
  # real position.
  # And traditionally angles sweep counter-clockwise in math, I always forget
  # that.
  def rotate(point, degrees)
    raise ArgumentError, "can only turn 90 degree increments" unless (degrees % 90).zero?
    raise ArgumentError, "can only turn 90 - 270" if degrees > 270 || degrees <= 0

    radians = degrees * Math::PI / 180
    sin = Math.sin(radians)
    cos = Math.cos(radians)
    Point.new(
      (point.x * cos - point.y * sin).round,
      (point.x * sin + point.y * cos).round,
    )
  end
end

# p1
origin = Point.new(0, 0)
s = Ship.new("E", origin)
s.navigate_all!(File.readlines(ARGV[0]))
puts "p1: ship ended up at #{s.position}, distance=#{origin.distance(s.position)}"

# p2
s = Ship2.new(origin, Point.new(10, 1))
s.navigate_all!(File.readlines(ARGV[0]))
puts "p2: ship ended up at #{s.position}, distance=#{origin.distance(s.position)}"
