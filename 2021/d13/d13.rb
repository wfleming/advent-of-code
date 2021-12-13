#!/usr/bin/env ruby

require "set"

Point = Struct.new(:x, :y)

Fold = Struct.new(:axis, :idx) do
  def fold(points)
    if axis == "x"
      fold_left(points)
    else
      fold_up(points)
    end
  end

  def fold_left(points)
    new_pts = points.map do |pt|
      if pt.x == idx
        raise StandardError, "fold should never have points along it #{self} #{pt}"
      elsif pt.x < idx
        # puts "DEBUG: folding at #{axis}=#{idx},  left of fold. point #{pt} x -> #{idx - pt.x - 1}"
        # fold at 5 means 4 -> 6, 3 -> 7, 2 -> 8, etc.
        # then the offset is applied for the new 0
        # that's idx + (idx - pt.x) - (idx + 1), which is just idx - x - 1
        Point.new(idx - pt.x - 1, pt.y)
      else
        # if you fold at x=5, x=6 becomes the new zero point, x=7 -> 1, etc.
        # so even points to the right of the fold get a new x
        # puts "DEBUG: folding at #{axis}=#{idx}, right of fold. point #{pt} x -> #{pt.x - idx - 1}"
        Point.new(pt.x - idx - 1, pt.y)
      end
    end.to_set
  end

  def fold_up(points)
    points.map do |pt|
      if pt.y == idx
        raise StandardError, "fold should never have points along it"
      elsif pt.y > idx
        # puts "DEBUG: folding at #{axis}=#{idx}. point #{pt} y -> #{idx - (pt.y - idx)}"
        # fold at 5 means 6 -> 4, 7 -> 3, 8 -> 2, etc.
        Point.new(pt.x, idx - (pt.y - idx))
      else
        pt
      end
    end.to_set
  end
end

def separate_instructions(lines)
  pt = lines.index("")
  pts = lines[0...pt].map do |pt|
    Point.new(*pt.split(",").map(&method(:Integer)))
  end
  folds = lines[(pt + 1)..].map do |f|
    m = /fold along (x|y)=(\d+)/.match(f)
    Fold.new(m[1], m[2].to_i)
  end
  [pts.to_set, folds]
end

def render(dots, empty: " ")
  x_range = dots.minmax_by(&:x).map(&:x)
  y_range = dots.minmax_by(&:y).map(&:y)

  (y_range[0]..y_range[1]).map do |y|
    (x_range[0]..x_range[1]).map do |x|
      dots.include?(Point.new(x, y)) ? "#" : empty
    end.join
  end.join("\n")
end

if __FILE__ == $0
  lines = File.readlines(ARGV[0]).map(&:chomp)
  dots, instructions = separate_instructions(lines)

  first_fold = instructions[0].fold(dots)
  puts "p1: after first fold #{instructions[0]} there are #{first_fold.count} visible dots"

  final_dots = instructions.reduce(dots) { |dots, i| i.fold(dots) }
  # my image is mirrored, reverse the x coords
  img = render(final_dots).lines.map { |l| l.chomp.reverse }.join("\n")
  puts "p2: finished paper\n#{img}"
end
