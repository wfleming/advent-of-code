#!/usr/bin/env ruby

require "set"

Cuboid = Struct.new(:x1, :x2, :y1, :y2, :z1, :z2, :on) do
  def self.parse(line)
    m = /(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/.match(line)
    new(m[2].to_i, m[3].to_i, m[4].to_i, m[5].to_i, m[6].to_i, m[7].to_i, m[1] == "on")
  end

  def volume
    (1 + x2 - x1) * (1 + y2 - y1) * (1 + z2 - z1)
  end

  # given another cuboid, split this cuboid into smaller cuboids, 1 of which is
  # the overlap between the two cuboids
  def split(other)
    # TODO
  end

  def overlap_volume(other)
    # https://stackoverflow.com/questions/5556170/finding-shared-volume-of-two-overlapping-cuboids
    [[other.x2, x2].min - [other.x, x].max, 0].max *
      [[other.y2, y2].min - [other.y, y].max, 0].max *
      [[other.z2, z2].min - [other.z, z].max, 0].max
  end
end

# turn cubes on/off, return total number on
def run_instructions(instructions)
  on_cubes = Set.new

  instructions.each do |i|
    if i.on
      on_cubes << i
      total_on += i.volume
    else

    end
  end
end

if __FILE__ == $0
  instructions = File.readlines(ARGV[0]).map(&Cuboid.method(:parse))

  p1_instructions = instructions.filter { |i| i.x1 >= -50 && i.x2 <= 50 && i.y1 >= -50 && i.y2 <= 50 && i.z1 >= -50 && i.z2 <= 50 }

  puts "p1: #{run_instructions(p1_instructions)}"
end
