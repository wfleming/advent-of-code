#!/usr/bin/env ruby

# monkeypatch
class Range
  def intersection(other)
    first = [self.begin, other.begin].max
    last = [self.end, other.end].min
    first <= last ? first..last : nil
  end
end

Point = Struct.new(:x,:y,:z)
Region = Struct.new(:c1, :c2, :on) do
  def overlaps?(other)
    !intersection(other).nil?
  end

  def intersection(other)
    xr = (c1.x..c2.x).intersection(other.c1.x..other.c2.x)
    yr = (c1.y..c2.y).intersection(other.c1.y..other.c2.y)
    zr = (c1.z..c2.z).intersection(other.c1.z..other.c2.z)

    xr && yr && zr ? Region.new(Point.new(xr.begin, yr.begin, zr.begin), Point.new(xr.end, yr.end, zr.end), on) : nil
  end

  def split(other)
    i = intersection(other)
    return self unless i
    x_ranges = [c1.x..i.c1.x-1, i.c1.x..i.c2.x, i.c2.x+1..c2.x].reject { |r| r.end < r.begin }
    y_ranges = [c1.y..i.c1.y-1, i.c1.y..i.c2.y, i.c2.y+1..c2.y].reject { |r| r.end < r.begin }
    z_ranges = [c1.z..i.c1.z-1, i.c1.z..i.c2.z, i.c2.z+1..c2.z].reject { |r| r.end < r.begin }

    x_ranges.flat_map do |xr|
      y_ranges.flat_map do |yr|
        z_ranges.map do |zr|
          Region.new(
            Point.new(xr.begin, yr.begin, zr.begin),
            Point.new(xr.end, yr.end, zr.end),
            on
          )
        end
      end
    end
  end

  def size
    throw "Invalid region" if c2.x < c1.x || c2.y < c1.y || c2.z < c1.z
    (c2.x + 1 - c1.x) * (c2.y + 1 - c1.y) * (c2.z + 1 - c1.z)
  end
end

INSTRUCTION=/(?<state>(on|off)) x=(?<x1>-?\d+)..(?<x2>-?\d+),y=(?<y1>-?\d+)..(?<y2>-?\d+),z=(?<z1>-?\d+)..(?<z2>-?\d+)/
def initialize_reactor(str)
  reactor = [] # list of regions

  str.lines.each do |l|
    if (m = INSTRUCTION.match(l))
      x1, x2, y1, y2, z1, z2 = Integer(m[:x1]), Integer(m[:x2]), Integer(m[:y1]), Integer(m[:y2]), Integer(m[:z1]), Integer(m[:z2])

      new_region = Region.new(Point.new(x1,y1,z1), Point.new(x2,y2,z2), m[:state] == "on")
      reactor = reactor.flat_map do |reg|
        if reg.overlaps?(new_region)
          reg.split(new_region).reject { |r| r.overlaps?(new_region) }
        else
          [reg]
        end
      end
      reactor << new_region
    end
  end

  reactor
end

def count_region(reactor, constrain_region=nil)
  reactor.map do |r|
    if r.on && constrain_region
      if r.overlaps?(constrain_region)
        r.intersection(constrain_region).size
      else
        0
      end
    elsif r.on
      r.size
    else
      0
    end
  end.sum
end

if __FILE__ == $0
  reactor = initialize_reactor(File.read(ARGV[0]))
  p1_region = Region.new(Point.new(-50,-50,-50), Point.new(50,50,50), false)
  puts "p1: #{count_region(reactor, p1_region)} cubes are on"
  puts "p2: #{count_region(reactor)} cubes are on"
end
