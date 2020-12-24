#!/usr/bin/env ruby

require "set"

# using the cube coordinates described at
# https://math.stackexchange.com/questions/2254655/hexagon-grid-coordinate-system#2643016
class Grid
  Coord = Struct.new(:x, :y, :z) do
    def east
      self.class.new(x + 1, y - 1, z)
    end

    def west
      self.class.new(x - 1, y + 1, z)
    end

    def northeast
      self.class.new(x + 1, y, z - 1)
    end

    def northwest
      self.class.new(x, y + 1, z - 1)
    end

    def southeast
      self.class.new(x, y - 1, z + 1)
    end

    def southwest
      self.class.new(x - 1, y, z + 1)
    end

    def neighbors
      [ east, west, northeast, northwest, southeast, southwest ]
    end
  end

  attr_reader :black_tiles

  def initialize
    @black_tiles = Set.new
  end

  def walk_to_tile(str)
    tile = Coord.new(0, 0, 0)
    chars = str.strip.each_char.to_a
    dir = chars.shift

    while chars.any? || !dir.nil?
      dir << chars.shift if ["n", "s"].include?(dir)
      # puts "DEBUG: walking coord=#{tile} dir=#{dir}. #{chars.count} chars left"

      case dir
      when "e"
        tile = tile.east
      when "w"
        tile = tile.west
      when "nw"
        tile = tile.northwest
      when "ne"
        tile = tile.northeast
      when "sw"
        tile = tile.southwest
      when "se"
        tile = tile.southeast
      else
        raise ArgumentError, "invalid dir #{dir}"
      end

      dir = chars.shift
    end

    tile
  end

  def flip_tile(coord)
    if black_tiles.include?(coord)
      black_tiles.delete(coord)
    else
      black_tiles.add(coord)
    end
  end

  def cycle
    coords_to_flip = black_tiles.flat_map do |black_coord|
      black_neighbor_cnt = black_coord.neighbors.count { |n| black_tiles.include?(n) }
      flip_self = black_neighbor_cnt == 0 || black_neighbor_cnt > 2

      white_neighbors = black_coord.neighbors.reject { |n| black_tiles.include?(n) }
      white_neighbors_to_flip = white_neighbors.select { |white_coord|
        white_coord.neighbors.count { |n| black_tiles.include?(n) } == 2
      }

      (flip_self ? [black_coord] : []) + white_neighbors_to_flip
    end

    coords_to_flip.uniq.each(&method(:flip_tile))
  end

  def cycle_n(n)
    n.times { cycle }
  end
end

grid = Grid.new
File.open(ARGV[0]).each_line do |line|
  dest = grid.walk_to_tile(line)
  grid.flip_tile(dest)
end
puts "p1: (day 0) there are #{grid.black_tiles.count} black tiles"

# p2 applies to the result of p1 - the p1 directions are the seed
grid.cycle_n(100)
puts "p2: after 100 days there are #{grid.black_tiles.count} black tiles"
