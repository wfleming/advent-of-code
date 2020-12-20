#!/usr/bin/env ruby

require "matrix"

class Tile
  attr_reader :id, :store, :rotation

  def initialize(id, lines, rotation = 0)
    @id = id
    @store =
      if lines.is_a?(Matrix)
        lines
      else
        Matrix.rows(lines.map { |l| l.each_char.to_a })
      end
    @rotation = rotation
  end

  def inspect
    "Tile #{id} (rotated #{rotation} deg):\n#{to_s}\n\n"
  end

  def to_s
    store.row_vectors.map { |row| row.to_a.join("") }.join("\n")
  end

  # rotate 90 degrees clockwise
  def rot_90
    self.class.new(
      id,
      Matrix.columns(store.row_vectors.map(&:to_a).reverse),
      rotation + 90
    )
  end

  def rotations
    [self, rot_90, rot_90.rot_90, rot_90.rot_90.rot_90]
  end

  def sides_including_rotations
    rotations.flat_map(&:sides)
  end

  def sides
    [top_side, right_side, bottom_side, left_side]
  end

  def top_side
    store.row(0)
  end

  def bottom_side
    store.row(-1)
  end

  def left_side
    store.column(0)
  end

  def right_side
    store.column(-1)
  end
end

class Parser
  attr_reader :str

  def initialize(str)
    @str = str
  end

  def tiles
    ts = []
    t_id = nil
    t_lines = []

    str.each_line do |l|
      l = l.strip
      if (m = /Tile (\d+):/.match(l))
        ts << Tile.new(t_id, t_lines) if t_id

        t_id = Integer(m[1])
        t_lines = []
      elsif /^[#\.]+$/ =~ l
        t_lines << l
      else
        # ignore the blank lines
      end
    end
    ts << Tile.new(t_id, t_lines) if t_id # make sure last tile gets added

    ts
  end
end

class Puzzle
  attr_reader :available_tiles

  def initialize(available_tiles)
    @available_tiles = available_tiles
  end

  # return array of 4 tiles
  def find_corners
    available_tiles.select { |tile|
      tile.sides.count { |side| find_match(tile, side) } == 2
    }.tap do |corners|
      raise "should have found 4 corners, but we found: #{corners.map(&:id)}" if corners.count != 4
    end
  end

  # return a tile or nil
  def find_match(tile, side)
    available_tiles.find { |other_tile|
      tile.id != other_tile.id && other_tile.sides_including_rotations.include?(side)
    }
  end
end

tiles = Parser.new(File.read(ARGV[0])).tiles

puzzle = Puzzle.new(tiles)
corners = puzzle.find_corners
puts "p1: corner ids = #{corners.map(&:id)}, product = #{corners.map(&:id).reduce(:*)}"
