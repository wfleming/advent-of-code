#!/usr/bin/env ruby

require "set"

Point = Struct.new(:x, :y) do
  def surrounding
    [
      Point.new(x - 1, y - 1),
      Point.new(x, y - 1),
      Point.new(x + 1, y - 1),

      Point.new(x - 1, y),
      Point.new(x, y),
      Point.new(x + 1, y),

      Point.new(x - 1, y + 1),
      Point.new(x, y + 1),
      Point.new(x + 1, y + 1),
    ]
  end
end

class Image
  def self.parse(lines)
    enhancement_algorithm = lines.shift
    lines.shift # blank line

    lit_pixels = Set.new
    lines.each_with_index do |l, y|
      l.each_char.with_index do |c, x|
        lit_pixels << Point.new(x, y) if c == "#"
      end
    end

    new(enhancement_algorithm, lit_pixels)
  end

  attr_reader :lit_pixels, :outer_pixels_lit, :x_range, :y_range

  def initialize(enhancement_algorithm, lit_pixels)
    @enhancement_algorithm = enhancement_algorithm
    @lit_pixels = lit_pixels
    @x_range = begin
      r = lit_pixels.minmax_by(&:x).map(&:x)
      r[0]..r[1]
    end
    @y_range ||= begin
      r = lit_pixels.minmax_by(&:y).map(&:y)
      r[0]..r[1]
    end
    @outer_pixels_lit = false
  end

  def char_at(pt)
    if x_range.include?(pt.x) && y_range.include?(pt.y)
      lit_pixels.include?(pt) ? "#" : "."
    else
      outer_pixels_lit ? "#" : "."
    end
  end

  def enhance!
    new_lit_pixels = lit_pixels.clone

    # to properly account for the "border" changing, we widen the x/y range
    ((y_range.first - 1)..(y_range.last + 1)).each do |y|
      ((x_range.first - 1)..(x_range.last + 1)).each do |x|
        surrounding_pat = Point.new(x,y).surrounding.map(&method(:char_at))
        enhancement_idx = surrounding_pat.map { |c| c == "#" ? "1" : "0" }.join.to_i(2)
        new_char = @enhancement_algorithm[enhancement_idx]

        # puts "enhance (#{x},#{y}): was #{char_at(Point.new(x,y))}, surrounding = #{surrounding_pat}, becomes #{new_char}"

        if new_char == "#"
          new_lit_pixels << Point.new(x,y)
        else
          new_lit_pixels.delete(Point.new(x,y))
        end
      end
    end
    @lit_pixels = new_lit_pixels
    @y_range = ((y_range.first - 1)..(y_range.last + 1))
    @x_range = ((x_range.first - 1)..(x_range.last + 1))

    # example input doesn't change outer pixels, but actual input swaps them all
    # each iteration, and the image is infinite, so...
    outer_pixels_pat = (outer_pixels_lit ? "1" : "0") * 9
    outer_pixels_enhancement_idx = outer_pixels_pat.to_i(2)
    @outer_pixels_lit = (@enhancement_algorithm[outer_pixels_enhancement_idx] == "#")

    self
  end

  def to_s
    y_range.map do |y|
      x_range.map do |x|
        char_at(Point.new(x, y))
      end.join
    end.join("\n")
  end
end

if __FILE__ == $0
  img = Image.parse(File.readlines(ARGV[0]))
  img.enhance!.enhance!
  puts "p1: image has #{img.lit_pixels.count} lit pixels"

  48.times { img.enhance! }
  puts "p2: image has #{img.lit_pixels.count} lit pixels"
end
