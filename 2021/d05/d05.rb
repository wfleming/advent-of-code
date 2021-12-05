#!/usr/bin/env ruby

Point = Struct.new(:x, :y)
Line = Struct.new(:a, :b) do
  LINE_PAT = /(\d+),(\d+) -> (\d+),(\d+)/

  def self.all
    File.readlines(ARGV[0]).map(&method(:parse))
  end

  def self.parse(line)
    m = LINE_PAT.match(line)

    self.new(Point.new(m[1].to_i, m[2].to_i), Point.new(m[3].to_i, m[4].to_i))
  end

  def angled?
    a.x != b.x && a.y != b.y
  end

  def all_points
    # calc a step of 1, -1, or 0 for each axis
    x_step = (b.x - a.x) / [(b.x - a.x).abs, 1].max
    y_step = (b.y - a.y) / [(b.y - a.y).abs, 1].max
    pts = [a]
    while pts[-1] != b
      pts << Point.new(pts[-1].x + x_step, pts[-1].y + y_step)
    end
    pts
  end
end

def p1_overlaps(lines)
  non_angled_lines = lines.reject(&:angled?)
  points = non_angled_lines.flat_map(&:all_points)

  points.group_by(&:itself).select { |k, v| v.count > 1 }.keys
end

def p2_overlaps(lines)
  points = lines.flat_map(&:all_points)

  points.group_by(&:itself).select { |k, v| v.count > 1 }.keys
end

lines = Line.all

puts "p1: There are #{p1_overlaps(lines).count} points with overlaps"
puts "p2: There are #{p2_overlaps(lines).count} points with overlaps"
