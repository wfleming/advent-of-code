#!/usr/bin/env ruby

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

    # at first I thought I'd just track the the lit pixels in a set, but I'm
    # concerned that could lead to accidentally considering some pixels part of
    # the "outer set" when they shouldn't be and getting the wrong value, so
    # I'll just track a hash. the set of "center pixels I need to care about"
    # won't grow that fast (for p1, here's hoping for p2)
    pixels = Hash.new
    lines.each_with_index do |l, y|
      l.each_char.with_index do |c, x|
        pixels[Point.new(x, y)] = (c == "#")
      end
    end

    new(enhancement_algorithm, pixels)
  end

  attr_reader :pixels, :outer_pixels_lit

  def initialize(enhancement_algorithm, pixels)
    @enhancement_algorithm = enhancement_algorithm
    @pixels = pixels
    @outer_pixels_lit = false
  end

  def x_range
    @x_range ||= begin
      r = pixels.minmax_by { |k, _| k.x }.map { |k, _| k.x }
      r[0]..r[1]
   end
  end

  def y_range
    @y_range ||= begin
      r = pixels.minmax_by { |k, _| k.y }.map { |k, _| k.y }
      r[0]..r[1]
   end
  end

  def char_at(pt)
    if x_range.include?(pt.x) && y_range.include?(pt.y)
      pixels[pt] ? "#" : "."
    else
      outer_pixels_lit ? "#" : "."
    end
  end

  def enhance!
    new_pixels = pixels.clone

    # to properly account for the "border" changing, we widen the x/y range
    ((y_range.first - 1)..(y_range.last + 1)).each do |y|
      ((x_range.first - 1)..(x_range.last + 1)).each do |x|
        surrounding_pat = Point.new(x,y).surrounding.map(&method(:char_at))
        enhancement_idx = surrounding_pat.map { |c| c == "#" ? "1" : "0" }.join.to_i(2)
        new_char = @enhancement_algorithm[enhancement_idx]

        # puts "enhance (#{x},#{y}): was #{char_at(Point.new(x,y))}, surrounding = #{surrounding_pat}, becomes #{new_char}"

        new_pixels[Point.new(x,y)] = (new_char == "#")
      end
    end
    @pixels = new_pixels

    # example input doesn't change outer pixels, but actual input swaps them all
    # each iteration, and the image is infinite, so...
    outer_pixels_pat = (outer_pixels_lit ? "1" : "0") * 9
    outer_pixels_enhancement_idx = outer_pixels_pat.to_i(2)
    @outer_pixels_lit = (@enhancement_algorithm[outer_pixels_enhancement_idx] == "#")

    # x, y ranges are cached for perf, but may have changes, so nil them out
    @x_range = nil
    @y_range = nil

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
  puts "p1: image has #{img.pixels.count { |_, v| v }} lit pixels"

  48.times { img.enhance! }
  puts "p2: image has #{img.pixels.count { |_, v| v }} lit pixels"
end
