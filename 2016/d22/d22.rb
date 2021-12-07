#!/usr/bin/env ruby

Pos = Struct.new(:x, :y)
Node = Struct.new(:size, :used) do
  PATTERN = %r{^/dev/grid/node-x(?<x>\d+)-y(?<y>\d+) +(?<size>\d+)T +(?<used>\d+)T}

  def self.parse_all
    Hash[
      File.readlines(ARGV[0]).map do |line|
        m = PATTERN.match(line)
        [Pos.new(m["x"].to_i, m["y"].to_i), self.new(m["size"].to_i, m["used"].to_i)]
      end
    ]
  end

  def avail
    size - used
  end
end

class State
  attr_reader :nodes, :target_data_pos

  def initialize(nodes, target_data_pos)
    @nodes = nodes
    @target_data_pos = target_data_pos
  end

  def viable_pairs
    nodes.flat_map.with_index do |a_pair, a_idx|
      a_pos, a = *a_pair
      nodes.select do |b_pos, b|
        a_pos != b_pos && a.used > 0 && b.avail >= a.used
      end.map { |b_pos, _b| [a_pos, b_pos] }
    end
  end

  # print the layout with numbers and colors
  def pretty_print
    red = ->(str) { "\e[31m#{str}\e[0m" }
    green = ->(str) { "\e[32m#{str}\e[0m" }
    blue = ->(str) { "\e[34m#{str}\e[0m" }
    str = "Nodes are used/size. #{blue.call("destination")} #{red.call("target data")} #{green.call("room for data")}\n"
    x_range = 0..nodes.keys.map(&:x).max
    y_range = 0..nodes.keys.map(&:y).max
    goal_data_size = nodes[target_data_pos].used

    str << y_range.map do |y|
      line = x_range.map do |x|
        n = nodes[Pos.new(x,y)]
        s = "#{n.used.to_s.rjust(3)}/#{n.size.to_s.rjust(3)}"
        if x == 0 && y == 0
          blue.call(s)
        elsif Pos.new(x, y) == target_data_pos
          red.call(s)
        elsif n.avail >= goal_data_size
          green.call(s)
        else
          s
        end
      end.join(" -- ")

      if y < y_range.last
        line = line + "\n" + (["   |   "] * x_range.last).join("    ")
      end

      line
    end.join("\n")

    str
  end

  def pretty_print_maze
    red = ->(str) { "\e[41m#{str}\e[0m" }
    green = ->(str) { "\e[42m#{str}\e[0m" }
    blue = ->(str) { "\e[44m#{str}\e[0m" }

    str = ". = big enough, #{blue.call("destination")}, #{red.call("G")} = data, #{green.call("_")} = empty, # = blocked\n"
    x_range = 0..nodes.keys.map(&:x).max
    y_range = 0..nodes.keys.map(&:y).max

    str << y_range.map do |y|
      x_range.map do |x|
        p = Pos.new(x, y)
        n = nodes[p]
        s =
          if n.used == 0
            "_"
          elsif n.used >= 100
            "#"
          elsif p == target_data_pos
            "G"
          elsif x == 0 && y == 0
            "D"
          else
            "."
          end

        if x == 0 && y == 0
          blue.call(s)
        elsif p == target_data_pos
          red.call(s)
        elsif n.used == 0
          green.call(s)
        else
          s
        end
      end.join("")
    end.join("\n")

    str
  end
end

nodes = Node.parse_all
initial_state = State.new(nodes, Pos.new(nodes.keys.map(&:x).max, 0))
puts "p1: there are #{initial_state.viable_pairs.count} viable pairs"

puts "p2: The state is:\n#{initial_state.pretty_print}"
puts ""
puts "p2: The maze is:\n#{initial_state.pretty_print_maze}"

# There's no code here to print the actual answer: Standard path-finding like A*
# doesn't work well here (it would find the solution eventually, I suppose, but
# it would take forever).
#
# So the approach is to print the layout as a maze and then I solved the maze by
# hand. My maze ended up looking like this (mostly using the same characters as
# the example on the site, but D is our destination for compactness, and I'm
# only showing the first few rows since the lower ones aren't relevant).
#
# D...................................G
# .....................................
# ...............######################
# .....................................
# .....................................
# .....................................
# ...................._................
# .....................................
#
# This is basically a sliding block puzzle: one empty spot, and you need to keep
# moving blocks to get the empty spot next to your goal, then you can move the
# goal to the destination.
#
# For this puzzle, that meant: move empty up 3, left 6, up 3, and right 21,
# putting it to the left of goal:
#
# ..._G
# .....
# #####
#
# Then we need to move G left 36 times to reach D, but after each left move
# (except the last), we need to move the empty one back around to the left, like
# so:
#
# ..._G  ...G_  ...G.  ...G.  ...G.  .._G.
# .....  .....  ...._  ..._.  .._..  .....
# #####  #####  #####  #####  #####  #####
#
# So for my particular input, the answer is:
# 3 + 6 + 3 + 21 + (35 * 5) + 1 = 209
