#!/usr/bin/env ruby

require "set"
require_relative "../d20/pqueue"

Point = Struct.new(:x, :y, :z) do
  def distance(other)
    (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
  end
end

# origin is min (x,y,z), width increases in each dimension
Cube = Struct.new(:origin, :width) do
  # 8 smaller cubes subdividing this one
  # I'm not being real precise about boundaries, so there's some overlap, but
  # it's fine for our purposes.
  def octants
    octant_width = (width.to_f / 2).ceil # round up for odd widths, which makes the octants a bit oversize but good enough for this
    @octants ||= [
      Cube.new(Point.new(origin.x, origin.y, origin.z), octant_width),
      Cube.new(Point.new(origin.x + octant_width + 1, origin.y, origin.z), octant_width),
      Cube.new(Point.new(origin.x, origin.y + octant_width + 1, origin.z), octant_width),
      Cube.new(Point.new(origin.x, origin.y, origin.z + octant_width + 1), octant_width),
      Cube.new(Point.new(origin.x + octant_width + 1, origin.y + octant_width + 1, origin.z), octant_width),
      Cube.new(Point.new(origin.x + octant_width + 1, origin.y, origin.z + octant_width + 1), octant_width),
      Cube.new(Point.new(origin.x, origin.y + octant_width + 1, origin.z + octant_width + 1), octant_width),
      Cube.new(Point.new(origin.x + octant_width + 1, origin.y + octant_width + 1, origin.z + octant_width + 1), octant_width),
    ]
  end

  def size
    (width + 1) ** 3
  end

  def points
    (0..width).flat_map do |x_offset|
      (0..width).flat_map do |y_offset|
        (0..width).map do |z_offset|
          Point.new(origin.x + x_offset, origin.y + y_offset, origin.z + z_offset)
        end
      end
    end
  end

  def intersect?(bot)
    max_corner = Point.new(origin.x + width, origin.y + width, origin.z + width)
    d = 0
    [:x, :y, :z].each do |axis|
      boxlow, boxhigh = origin.send(axis), max_corner.send(axis)
      d += (bot.pos.send(axis) - boxlow).abs + (bot.pos.send(axis) - boxhigh).abs
      d -= boxhigh - boxlow
    end
    d /= 2
    d <= bot.r
  end
end

Nanobot = Struct.new(:pos, :r) do
  PAT = /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/

  def self.parse(str)
    m = PAT.match(str)
    new(Point.new(m[1].to_i, m[2].to_i, m[3].to_i), m[4].to_i)
  end

  def in_range?(other_pos)
    pos.distance(other_pos) <= r
  end
end

class Octree
  attr_reader :cube

  def initialize(cube, bots)
    @cube = cube
    @bots = bots
  end

  def intersecting_bots
    @intersecting_bots = @bots.select { |b| cube.intersect?(b) }
  end

  def children
    @children ||= cube.octants.map { |octant| self.class.new(octant, intersecting_bots) }
  end

  def leaf?
    cube.width <= 3
  end
end

# bfs with a priority queue - we don't need to search the entire space, we want
# to always be subdividing the currently-most-bots cube until we get to a
# sufficiently small cube to just check every point
ZERO_PT = Point.new(0,0,0)
def search_octree(octree)
  # this pqueue sorts maximum first, not lowest first like the other
  # implementation I use for A* a lot
  best_pt, best_pt_bots_cnt, best_pt_d = nil, nil, nil
  queue = PQueue.new([octree]) do |a, b|
    bots_cmp = a.intersecting_bots.count <=> b.intersecting_bots.count
    if bots_cmp == 0 # if bot counts are equal, focus on the smaller cube
      b.cube.size <=> a.cube.size
    else
      bots_cmp
    end
  end

  i = 0
  while queue.length > 0
    n = queue.pop

    if i % 100 == 0
      puts "search_octree loop queue=#{queue.length}"
      puts "    n.intersecting_bots=#{n.intersecting_bots.count} n.width=#{n.cube.width} n.dist=#{n.cube.origin.distance(ZERO_PT)}"
      puts "    best_pt_bots_cnt=#{best_pt_bots_cnt} best_pt_d=#{best_pt_d}"
    end
    i += 1

    # reject any node with fewer bots than current best pt
    next if best_pt_bots_cnt&.> n.intersecting_bots.count

    # reject any node further away than current best (since what we want in
    # the end is distance to (0,0,0)) if it's *tied* with current best
    # (already rejected if it's worse than current best, and if it's better we
    # need to keep it)
    if n.intersecting_bots.count == best_pt_bots_cnt
      n_min_dist = n.cube.origin.distance(ZERO_PT) - (n.cube.width * 3)
      next if n_min_dist > best_pt_d
    end

    if n.leaf?
      n.cube.points.each do |p|
        if best_pt.nil?
          best_pt = p
          best_pt_bots_cnt = n.intersecting_bots.count { |b| b.in_range?(p) }
          best_pt_d = p.distance(ZERO_PT)
        else
          d = p.distance(ZERO_PT)
          bots_cnt = n.intersecting_bots.count { |b| b.in_range?(p) }
          if bots_cnt > best_pt_bots_cnt || (bots_cnt == best_pt_bots_cnt && d < best_pt_d)
            best_pt = p
            best_pt_bots_cnt = bots_cnt
            best_pt_d = d
          end
        end
      end
    else
      n.children.each { |c| queue << c }
    end
  end

  [best_pt, best_pt_bots_cnt, best_pt_d]
end

if __FILE__ == $0
  bots = File.readlines(ARGV[0]).map(&Nanobot.method(:parse))
  strongest_bot = bots.max_by(&:r)

  in_range_bots = bots.count { |b| strongest_bot.in_range?(b.pos) }
  puts "p1: #{in_range_bots} bots are in range of the strongest bot (#{strongest_bot})"

  x_limits = bots.map { |b| b.pos.x }.minmax
  y_limits = bots.map { |b| b.pos.y }.minmax
  z_limits = bots.map { |b| b.pos.z }.minmax

  widest = [x_limits[1] - x_limits[0], y_limits[1] - y_limits[0], z_limits[1] - z_limits[0]].max

  big_cube = Cube.new(Point.new(x_limits.min, y_limits.min, z_limits.min), widest)

  root_octree = Octree.new(big_cube, bots)
  pt, bots_cnt, d = search_octree(root_octree)

  puts "p2: maximum coverage is #{bots_cnt} bots, closest pt with max coverage is #{pt}"
  puts "p2: distance from (0,0,0) to winning point is #{d}"
end
