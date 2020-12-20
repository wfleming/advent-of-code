#!/usr/bin/env ruby

require "matrix"

OPPOSITES = {
  top_side: :bottom_side,
  bottom_side: :top_side,
  left_side: :right_side,
  right_side: :left_side,
}.freeze
SIDES = OPPOSITES.keys.freeze

class Tile
  attr_reader :id, :store, :rotation, :flip

  def initialize(id, lines, rotation = 0, flip = :none)
    @id = id
    @store =
      if lines.is_a?(Matrix)
        lines
      else
        Matrix.rows(lines.map { |l| l.each_char.to_a })
      end
    @rotation = rotation
    @flip = flip
  end

  def inspect
    "Tile #{id} (rotated #{rotation} deg, flip = #{flip}):\n#{to_s}\n\n"
  end

  def to_s
    store.row_vectors.map { |row| row.to_a.join("") }.join("\n")
  end

  # rotate 90 degrees clockwise
  def rot_90
    self.class.new(
      id,
      Matrix.columns(store.row_vectors.map(&:to_a).reverse),
      rotation + 90,
      flip
    )
  end

  def rotations
    @rotations ||= [self].tap do |rots|
        3.times do rots << rots[-1].rot_90 end
      end
  end

  def flip_h
    self.class.new(
      id,
      Matrix.columns(store.column_vectors.map(&:to_a).reverse),
      rotation,
      flip == :none ? :horizontal : :both,
    )
  end

  def flip_v
    self.class.new(
      id,
      Matrix.rows(store.row_vectors.map(&:to_a).reverse),
      rotation,
      flip == :none ? :vertical : :both,
    )
  end

  def flips
    @flips ||= [
      self,
      flip_h,
      flip_v,
      flip_h.flip_v,
    ]
  end

  def all_orientations
    # some orientations are identical, so ignore those
    @all_orientations ||= rotations.flat_map(&:flips).uniq(&:store)
  end

  def sides_including_rotations
    rotations.flat_map(&:sides)
  end

  def sides_including_all_orientations
    all_orientations.flat_map(&:sides)
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

  def border_removed
    store.row_vectors[1..-2].map(&:to_a).map { |row| row[1..-2].join("") }
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
  attr_reader :available_tiles, :matched_tiles

  def initialize(available_tiles)
    @available_tiles = available_tiles.freeze
    # hash from [x,y] => tile. standard math coordinates - y increases up, x
    # increases right
    @matched_tiles = Hash.new
  end

  # return array of 4 tiles
  def find_corners
    available_tiles.select { |tile|
      tile.sides.count { |side|
        available_tiles.find { |other_tile|
          tile.id != other_tile.id &&
            other_tile.sides_including_all_orientations.include?(side)
        }
      } == 2
    }.tap do |corners|
      raise "should have found 4 corners, but we found: #{corners.map(&:id)}" if corners.count != 4
    end
  end

  class SolveState
    attr_reader :original_tiles, :all_edge_matches, :matched_tiles, :edges_to_inspect

    def initialize(original_tiles, all_edge_matches, matched_tiles, edges_to_inspect = nil)
      @original_tiles = original_tiles
      @all_edge_matches = all_edge_matches
      @matched_tiles = matched_tiles # hash of [x,y] => tile
      @edges_to_inspect = edges_to_inspect || neighbors(matched_tiles.first[0]).values # array of coords [[x,y]]
    end

    def priority
      0 - matched_tiles.count
    end

    def complete?
      matched_tiles.values.map(&:id).sort == original_tiles.map(&:id).sort
    end

    def subsequent_states
      # puts "DEBUG: looking for next state with #{matched_tiles.count}/#{original_tiles.count} matched, #{edges_to_inspect.count} edges to look at"

      edges_to_inspect.flat_map do |edge_coord|
        # side -> coord - these sides are constrained by pieces already in place
        constrained_sides = neighbors(edge_coord).select { |side, coord|
          matched_tiles.key?(coord)
        }

        # determine which tiles can go here by getting intersection of what's
        # possible from all the constratints
        placeable_tiles = constrained_sides.
          map { |source_side, coord|
            all_edge_matches[matched_tiles[coord]][OPPOSITES.fetch(source_side)]
          }.
          reduce(&:intersection).
          reject { |tile| matched_tiles.values.any? { |t2| t2.id == tile.id  } }

        placeable_tiles.map do |placed_tile|
          new_matched_tiles = matched_tiles.merge(edge_coord => placed_tile)
          new_edges_to_inspect = (edges_to_inspect + neighbors(edge_coord).values).uniq.reject { |coord| new_matched_tiles.key?(coord) }

          SolveState.new(
            original_tiles,
            all_edge_matches,
            new_matched_tiles,
            new_edges_to_inspect,
          )
        end
      end
    end

    def neighbors(coord)
      {
        top_side: [coord[0], coord[1] + 1],
        right_side: [coord[0] + 1, coord[1]],
        bottom_side: [coord[0], coord[1] - 1],
        left_side: [coord[0] - 1, coord[1]],
      }
    end
  end

  # put the tiles together
  def assemble
    # start the first tile at (0,0)
    init_orientations = available_tiles[0].all_orientations
    states = init_orientations.map do |orientation|
      SolveState.new(
        available_tiles,
        all_edge_matches,
        { [0,0] => orientation },
      )
    end

    loop do
      s = states.shift
      raise "ran out of states" if s.nil?
      return s if s.complete?
      states += s.subsequent_states
      states.sort_by!(&:priority)
    end
  end

  # a hash of tile orientation => { source_side => [tiles] }
  # there's an entry for every orientation of a tile, so e.g. there's an entry
  # for tile id 1, tile id 1 rotated 90, tile id 1 rotated 90 & flipped
  # horizontally, etc.
  def all_edge_matches
    @all_edge_matches ||=
      begin
        all_orientations = available_tiles.flat_map(&:all_orientations)
        Hash[
          all_orientations.map do |tile|
            matches_by_side = Hash[
              SIDES.map do |side|
                matched_tiles = all_orientations.select { |other_tile|
                  tile.send(side) == other_tile.send(OPPOSITES.fetch(side))
                }
                [side, matched_tiles]
              end
            ].reject { |_k, v| v.empty? }

            [tile, matches_by_side]
          end
        ]
      end
  end
end

def render_matched_tile_ids(matched_tiles)
  min_x = matched_tiles.keys.map { |coord| coord[0] }.min
  max_x = matched_tiles.keys.map { |coord| coord[0] }.max
  min_y = matched_tiles.keys.map { |coord| coord[1] }.min
  max_y = matched_tiles.keys.map { |coord| coord[1] }.max

  (min_y..max_y).map do |y|
    (min_x..max_x).map do |x|
      matched_tiles[[x, y]].id
    end.join("  ")
  end.join("\n")
end

# render the hash of { [x, y] => tile } into one big string
def render_matched_tiles(matched_tiles)
  min_x = matched_tiles.keys.map { |coord| coord[0] }.min
  max_x = matched_tiles.keys.map { |coord| coord[0] }.max
  min_y = matched_tiles.keys.map { |coord| coord[1] }.min
  max_y = matched_tiles.keys.map { |coord| coord[1] }.max

  (min_y..max_y).map do |y|
    row_tile_rows = (min_x..max_x).map do |x|
      # reverse here - i did my y coordinate system wrong, this is ugly but
      # works and I don't feel like fixing everything else
      matched_tiles[[x, y]].border_removed.reverse
    end

    row_lines = row_tile_rows.reduce(&:zip).map(&:flatten).map { |l| l.join("") }
    row_lines.join("\n")
  end.join("\n")
end

class NessieFinder
  attr_reader :cells, :nessie_locations

  def initialize(rendered_puzzle)
    @cells =
      if rendered_puzzle.is_a?(Matrix)
        rendered_puzzle
      else
        Matrix.rows(
          rendered_puzzle.lines.map { |l| l.strip.each_char.to_a }
        )
      end
    @nessie_locations = []
  end

  def to_s
    cells.row_vectors.map { |r| r.to_a.join("") }.join("\n")
  end

  def rot_90
    self.class.new(
      Matrix.columns(cells.row_vectors.map(&:to_a).reverse),
    )
  end

  def rotations
    @rotations ||= [self].tap do |rots|
        3.times do rots << rots[-1].rot_90 end
      end
  end

  def flip_h
    self.class.new(
      Matrix.columns(cells.column_vectors.map(&:to_a).reverse),
    )
  end

  def flip_v
    self.class.new(
      Matrix.rows(cells.row_vectors.map(&:to_a).reverse),
    )
  end

  def flips
    @flips ||= [
      self,
      flip_h,
      flip_v,
      flip_h.flip_v,
    ]
  end

  def all_orientations
    # some orientations could be identical, so ignore those
    @all_orientations ||= rotations.flat_map(&:flips).uniq(&:cells)
  end

  def find_nessies
    @nessie_locations = (0..(cells.row_count - 1)).flat_map do |y|
      (0..(cells.column_count - 1)).map do |x|
        [x, y] if nessie_at?([x, y])
      end
    end.compact
  end

  def mark_nessies!
    nessie_locations.each do |origin|
      nessie_coords_from(origin).each do |coord|
        cells[*coord] = "O"
      end
    end
  end

  def water_roughness
    cells.count { |c| c == "#" }
  end

  def nessie_at?(origin)
    coords = nessie_coords_from(origin)
    coords.all? { |c| c.all? { |v| v >= 0 } } &&
      coords.all? { |c| cells[c[0], c[1]] == "#" }
  end

  def nessie_coords_from(origin)
    nessie_offsets.map do |offset|
      [origin[0] + offset[0], origin[1] + offset[1]]
    end
  end

  # this is the pattern. origin is the left-most x
  NESSIE = [
    "                  x",
    "x    xx    xx    xxx",
    " x  x  x  x  x  x",
  ]
  def nessie_offsets
    @nessie_offsets ||= NESSIE.each_with_index.flat_map do |line, y|
      line.each_char.each_with_index.map do |c, x|
        [y - 1, x] if c == "x"
      end
    end.compact
  end
end

tiles = Parser.new(File.read(ARGV[0])).tiles

puzzle = Puzzle.new(tiles)
corners = puzzle.find_corners
puts "p1: corner ids = #{corners.map(&:id)}, product = #{corners.map(&:id).reduce(:*)}"

# puts "DEBUG - built all edge matches: #{puzzle.all_edge_matches.count}"
assembled_puzzle = puzzle.assemble.matched_tiles
# puts "DEBUG assembled puzzle state: #{assembled_puzzle.inspect}"
#puts "DEBUG the display by tile id:\n#{render_matched_tile_ids(assembled_puzzle)}"
# puts "DEBUG display:\n#{render_matched_tiles(assembled_puzzle)}"
finder0 = NessieFinder.new(render_matched_tiles(assembled_puzzle))
finder = finder0.all_orientations.find do |f|
  f.find_nessies
  f.nessie_locations.count > 0
end
puts "p2: found #{finder.nessie_locations.count} nessies"
finder.mark_nessies!
puts "p2: the marked picture:\n#{finder.to_s}\n"
puts "p2: water roughness is #{finder.water_roughness}"
