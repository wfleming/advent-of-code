#!/usr/bin/env ruby

# coordinates start (0,0) at upper left, increase right & down
TREE = "#".freeze
MAP = File.read(ARGV[0]).lines.map(&:strip).freeze

Point = Struct.new(:x,:y) do
  def +(other)
    self.class.new(self.x + other.x, self.y + other.y)
  end
end

def map_at(pt)
  y_line = MAP[pt.y]

  # x dimension repeats
  y_line[pt.x % y_line.length]
end

# step by slope until y > max y, return # of trees hit
def slide(at:, slope:)
  tree_cnt = (map_at(at) == TREE ? 1 : 0)
  next_pos = at + slope

  if next_pos.y >= MAP.count
    return tree_cnt
  else
    return tree_cnt + slide(at: next_pos, slope: slope)
  end
end

# p1
trees = slide(at: Point.new(0,0), slope: Point.new(3, 1))
puts "p1: slid through #{trees} trees"

# p2
slopes = [
  Point.new(1, 1),
  Point.new(3, 1),
  Point.new(5, 1),
  Point.new(7, 1),
  Point.new(1, 2),
]
tree_product = slopes.map do |slope|
  slide(at: Point.new(0, 0), slope: slope)
end.inject(1, :*)
puts "p2: product is #{tree_product}"
