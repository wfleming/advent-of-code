#!/usr/bin/env ruby

require "set"

Point = Struct.new(:x, :y) do
  def neighbors
    [
      self.class.new(x - 1, y),
      self.class.new(x + 1, y),
      self.class.new(x, y - 1),
      self.class.new(x, y + 1),
    ]
  end
end

class Map
  def initialize(lines)
    @points = Hash[
      lines.flat_map.with_index do |line, y|
        line.chomp.each_char.with_index.map do |c, x|
          [Point.new(x, y), Integer(c)]
        end
      end
    ]
  end

  # return hash of low_point_position => height
  def low_points
    @points.select do |pt, hght|
      neighb_hghts = pt.neighbors.map { |pt2| @points[pt2] }.compact
      neighb_hghts.any? && neighb_hghts.all? { |h2| hght < h2 }
    end
  end

  # map of low point => size of basin
  def basins
    Hash[
      low_points.keys.map do |center|
        painted = [].to_set
        frontier = [center]

        while frontier.any?
          node = frontier.shift
          painted << node
          frontier += node.neighbors.reject do |n|
            @points[n].nil? || @points[n] == 9 ||
              painted.include?(n) || frontier.include?(n)
          end
        end

        [center, painted.count]
      end
    ]
  end
end

map = Map.new(File.readlines(ARGV[0]))
p1_risk = map.low_points.sum { |p, h| h + 1 }
puts "p1: sum of risk levels of low points = #{p1_risk}"

p2_basins = map.basins.to_a.sort_by { |p| 0 - p[1] }.take(3)
puts "p2: basins product = #{p2_basins.reduce(1) { |memo, basin| memo * basin[1] }}"
