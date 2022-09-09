#!/usr/bin/env ruby

Point = Struct.new(:a,:b,:c,:d) do
  def self.parse(line)
    self.new(*line.strip.split(",").map(&method(:Integer)))
  end

  def distance(other)
    (other.a - a).abs +
      (other.b - b).abs +
      (other.c - c).abs +
      (other.d - d).abs
  end
end

def build_constellation(c, pts)
  n = pts.find { |p| c.any? { |p2| p2.distance(p) <= 3 } }

  while n
    c << n
    pts.delete(n)
    n = pts.find { |p| c.any? { |p2| p2.distance(p) <= 3 } }
  end

  c
end

def p1_constellations(points)
  constellations = []

  while points.any?
    constellations << build_constellation([points.shift], points)
  end

  constellations
end

if __FILE__ == $0
  pts = File.readlines(ARGV[0]).map(&Point.method(:parse))

  p1 = p1_constellations(pts)
  puts "p1: there are #{p1.count} constellations"
end
