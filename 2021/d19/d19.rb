#!/usr/bin/env ruby

require "set"

Point = Struct.new(:x, :y, :z) do
  def to_s
    "{#{x}, #{y}, #{z}}"
  end

  def self.parse(str)
    new(*str.split(",").map(&method(:Integer)))
  end

  # all 24 possible different orientations
  def orientations
    @orientations ||= [
      Point.new(x, -y, -z),
      Point.new(y, x, -z),
      Point.new(-y, -x, -z),
      Point.new(-x, y, -z),
      Point.new(-z, y, x),
      Point.new(-y, -z, x),
      Point.new(y, z, x),
      Point.new(z, -y, x),
      Point.new(-z, -y, -x),
      Point.new(y, -z, -x),
      Point.new(-y, z, -x),
      Point.new(z, y, -x),
      Point.new(x, y, z),
      Point.new(-y, x, z),
      Point.new(y, -x, z),
      Point.new(-x, -y, z),
      Point.new(-x, -z, -y),
      Point.new(z, -x, -y),
      Point.new(-z, x, -y),
      Point.new(x, z, -y),
      Point.new(-x, z, y),
      Point.new(-z, -x, y),
      Point.new(z, x, y),
      Point.new(x, -z, y),
    ]
  end

  def +(other)
    Point.new(x + other.x, y + other.y, z + other.z)
  end

  def -(other)
    Point.new(x - other.x, y - other.y, z - other.z)
  end

  def manhattan_dist(other)
    (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
  end
end

# Beacons remain as relative to scanner even when pos is known
Scanner = Struct.new(:id, :beacons, :pos) do
  def orientations
    @orientations ||= begin
      beacon_variations = beacons.map(&:orientations)
      beacon_variations[0].each_index.map do |i|
        Scanner.new(id, beacon_variations.map { |x| x[i] }, pos)
      end
    end
  end

  def sq_distances
    @sq_distances ||= Hash.new.tap do |h|
      beacons.to_a.combination(2).map do |b1, b2|
        d = (b1.x - b2.x)**2 + (b1.y - b2.y)**2 + (b1.z - b2.z)**2
        h[d] ||= []
        h[d] += [b1,b2]
      end
    end
  end

  def absolute_beacons
    @absolute_beacons ||= Set.new(
      beacons.map { |b| pos + b }
    )
  end
end

SCANNER_PAT = /--- scanner (\d+) ---/
def parse_scanners(str)
  scanners = []
  s = nil
  str.lines.each do |l|
    if (m = SCANNER_PAT.match(l))
      scanners << s if s
      s = Scanner.new(Integer(m[1]), [], nil)
    elsif l.strip.length > 0
      s.beacons << Point.parse(l)
    end
  end
  scanners << s

  scanners
end

# given a scanner with a known pos and a scanner without a known pos, find a
# position for s2 where the two scanners see >= beacon_min of the same beacons.
# returns s2_final_orientation_and_pos if a match is found, otherwise nil
def find_scanner_offset(s1, s2, beacon_min=12)
  same_distances = s1.sq_distances.keys.intersection(s2.sq_distances.keys)
  if same_distances.count >= beacon_min / 2
    same_distances.each do |sqd|
      s1.sq_distances[sqd].each do |s1b|
        s2.sq_distances[sqd].each do |s2b|
          s2.orientations.each do |s2p|
            # for a point p that both scanners see, s1.pos + s1b == s2.pos + s2b
            # solve for s2.pos, the potential pos if s1b and s2b refer to the
            # same beacon is s2.pos = s1.pos + s1b - s2b
            s2b_rotated = s2p.beacons[s2.beacons.find_index(s2b)]
            potential_pos = s1.pos + s1b - s2b_rotated
            intersects = s2p.beacons.map do |b|
              potential_pos + b
            end.count { |b| s1.absolute_beacons.include?(b) }
            if intersects >= beacon_min
              s2p.pos = potential_pos
              return s2p
            end
          end
        end
      end
    end
  end

  nil
end

def connect_scanners_once(known, unknown)
  new_known = known.clone
  new_unknown = []

  unknown.each do |s|
    connected_scanner = known.find do |s0|
      s_identified = find_scanner_offset(s0, s)
      new_known << s_identified if s_identified
      s_identified
    end
    new_unknown << s unless connected_scanner
  end

  [new_known, new_unknown]
end

# find positions for scanners until all positions are known
def connect_scanners(scanners)
  known_scanners = [scanners.shift.tap { |s| s.pos = Point.new(0,0,0) }]

  while scanners.any?
    # puts "DEBUG: connect_scanner start loop"
    new_known, new_scanners = connect_scanners_once(known_scanners, scanners)
    # puts "    connect_scanner loop known.count=#{known_scanners.count} scanners.count=#{scanners.count} / new_known.count=#{new_known.count} new_scanners.count=#{new_scanners.count}"
    if scanners.count == new_scanners.count
      # puts "DEBUG ERROR: known=#{known_scanners.map(&:id)} unknown=#{scanners.map(&:id)}"
      raise "Full iteration found no matches"
    end

    scanners = new_scanners
    known_scanners = new_known
  end

  known_scanners
end

# for a set of scanners that have known positions, generate a list of absolute
# beacon positions
def enumerate_beacons(scanners)
  scanners.reduce(Set.new) { |beacons, s| beacons + s.absolute_beacons }
end

if __FILE__ == $0
  scanners = parse_scanners(File.read(ARGV[0]))
  matched_scanners = connect_scanners(scanners)
  puts "p1: There are #{enumerate_beacons(matched_scanners).count} beacons"

  scanner_distances = matched_scanners.combination(2).map { |s1, s2| s1.pos.manhattan_dist(s2.pos) }
  puts "p2: furthest apart scanners are #{scanner_distances.max} apart"
end
