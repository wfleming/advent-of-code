#!/usr/bin/env ruby

# coordinates start (0,0) at upper left, increase right & down
TREE = "#".freeze
MAP = File.read(ARGV[0]).lines.map(&:strip).freeze

Point = Struct.new(:x,:y) do
  def +(other)
    self.class.new(self.x + other.x, self.y + other.y)
  end
end
SLOPE = Point.new(3, 1)

def map_at(pt)
  y_line = MAP[pt.y]

  # x dimension repeats
  y_line[pt.x % y_line.length]
end

# step by SLOPE until y > max y, return # of trees hit
def slide(current_pos)
  tree_cnt = (map_at(current_pos) == TREE ? 1 : 0)
  next_pos = current_pos + SLOPE

  if next_pos.y >= MAP.count
    return tree_cnt
  else
    return tree_cnt + slide(next_pos)
  end
end

# p1
trees = slide(Point.new(0,0))
puts "p1: slid through #{trees} trees"
