Point = Struct.new(:x, :y)
Water = Struct.new(:point, :stable) do # stable or flowing
  def x
    point.x
  end

  def y
    point.y
  end

  def stable?
    stable
  end

  def flowing?
    !stable?
  end

  def to_s
    "<Water (#{x}, #{y}) #{stable? ? '~' : '|'}>"
  end
end

class Grid
  include Enumerable

  attr_reader :width, :height

  def initialize(width, height)
    @width = width
    @height = height
    @storage = height.times.map { [nil] * width }
  end

  def ==(other)
    other.is_a?(self.class) && other.storage == storage
  end
  alias :eql? :==

  def hash
    storage.hash
  end

  def dup
    self.class.new(width, height).tap do |other|
      other.storage = storage.map(&:dup)
    end
  end

  def [](pt)
    return nil unless in_bounds?(pt)
    storage[pt.y][pt.x]
  end

  def []=(pt, val)
    return nil unless in_bounds?(pt)
    storage[pt.y][pt.x] = val
  end

  def in_bounds?(pt)
    pt.x >= 0 && pt.x < width && pt.y >= 0 && pt.y < height
  end

  def each
    e = to_enum
    if block_given?
      e.each { |pt, v| yield pt, v }
    end
    e
  end

  def to_enum
    Enumerator.new do |yielder|
      x = y = 0
      loop do
        yielder.yield(Point.new(x, y), self[Point.new(x, y)])
        x += 1

        if x >= width
          x = 0
          y += 1
        end

        if y >= height
          raise StopIteration
        end
      end
    end
  end

  protected

  attr_accessor :storage
end

SPRING = Point.new(500, 0)

class Scan
  LINE_PAT = %r{(.)=(\d+), (.)=(\d+)..(\d+)}

  OutOfBounds = Class.new(StandardError)
  StableError = Class.new(StandardError)

  def self.parse(str)
    pts = str.lines.flat_map do |line|
      m = LINE_PAT.match(line)
      d1, d1v, _, d2_min, d2_max = m[1], m[2].to_i, m[3], m[4].to_i, m[5].to_i
      (d2_min..d2_max).map do |d2v|
        if d1 == "x"
          Point.new(d1v, d2v)
        else
          Point.new(d2v, d1v)
        end
      end
    end
    new(pts)
  end

  attr_reader :grid, :water, :min_x, :max_x, :min_y, :max_y

  def initialize(clay)
    @min_x = clay.map(&:x).min - 5
    @max_x = clay.map(&:x).max + 5
    @min_y = clay.map(&:y).min
    @max_y = clay.map(&:y).max
    @grid = Grid.new(1 + max_x - min_x, 1 + max_y - min_y)
    clay.each do |pt|
      @grid[grid_pt(pt)] = :clay
    end

    @water = []
    add_water(Water.new(Point.new(SPRING.x, min_y), false))
  end

  # translate from scan coordinates to grid coordinates
  def grid_pt(pt)
    Point.new(pt.x - min_x, pt.y - min_y)
  end

  def [](pt)
    grid[grid_pt(pt)]
  end

  def add_water(w)
    water << w
    grid[grid_pt(w)] = w
  end

  def drip_until_full
    drip while true
  rescue StableError
    # no-op
  end

  # flow all eligible squares of water
  def drip
    wstate = water_hash
    water.reject(&:stable?).sort_by { |w| -w.y }.each do |w|
      flow(w)
      enforce_out_of_bounds(w)
    end
    if wstate == water_hash
      raise StableError, "no progress made in a flow iteration"
    end
  end

  def water_hash
    water.map(&:to_s)
  end

  # flow a single drop of water by 1 time increment. "flow" means "splitting"
  def flow(w)
    # flow rules:
    # - flow down if you can
    # - otherwise, flow left and/or right if you can

    if !self[Point.new(w.x, w.y + 1)]
      scan_down(w)
    else
      scan_sideways(w)
    end
  end

  def scan_down(w)
    w2 = Water.new(Point.new(w.x, w.y + 1), false)
    until w2.y > max_y || self[w2]
      add_water(w2)
      w2 = Water.new(Point.new(w2.x, w2.y + 1), false)
    end
  end

  def stable?(point)
    case (w = self[point])
    when :clay
      true
    when Water
      w.stable?
    end
  end

  def scan_sideways(w)
    # scan left and right if we can't flow down
    left = w.x - scan_direction(w, -1)
    right = w.x + scan_direction(w, 1)

    # binding.pry if w.y == 983 && w.x == 544

    # water can only flow over a solid base
    can_flow = (left..right).map { |x| Point.new(x, w.y + 1)}.all?(&method(:stable?))
    return unless can_flow

    # if we hit walls on either side this water will be stable
    stable = [
      Point.new(left - 1, w.y),
      Point.new(right + 1, w.y),
    ].all?(&method(:stable?))

    if !stable # there's at least one edge we can flow over
      left -= 1 unless self[Point.new(left - 1, w.y)]
      right += 1 unless self[Point.new(right + 1, w.y)]
    end

    (left..right).each do |x|
      if !(w2 = self[Point.new(x, w.y)])
        add_water(Water.new(
          Point.new(x, w.y),
          stable,
        ))
      elsif w2.is_a?(Water)
        # water that flowed down may need to be marked stable after scanning
        w2.stable = stable
      end
    end
  end

  # scan in a direction, return at wall or cliff
  def scan_direction(w, dir)
    # you can flow until you hit a wall, or go over a ledge over clay
    d = 0

    while (( # don't walk past edge of map
            (w.x + (d * dir)) < max_x &&
            (w.x + (d * dir)) > min_x
          ) &&
          ( # walk over while there's nothing in the way & there is something below
           stable?(Point.new(w.x + ((d + 1) * dir), w.y + 1)) &&
           self[Point.new(w.x + ((d + 1) * dir), w.y)] != :clay
          ))
      d += 1
    end
    d
  end

  def viz
    (min_y..max_y).map do |y|
      line = "." * (max_x - min_x)
      line.each_char.each_with_index do |_, x|
        case (w = self[Point.new(min_x + x, y)])
        when :clay
          line[x] = "#"
        when Water
          line[x] = w.stable? ? "~" : "|"
        end
      end
      line + "  #{y}"
    end.join("\n")
  end

  def out_of_bounds?(w)
    w.x < min_x || w.x > max_x || w.y < min_y || w.y > max_y
  end

  def enforce_out_of_bounds(w)
    if out_of_bounds?(w)
      water.reject! { |w| out_of_bounds?(w) }
      raise OutOfBounds, w
    end
  end
end

if $0 == __FILE__
  scan = Scan.parse(File.read(ARGV[0]))
  scan.drip_until_full

  puts "p1: #{scan.water.count} squares are full of water"
  puts "p2: #{scan.water.select(&:stable?).count} squares of water are stable"
  File.open("viz", "w") { |fh| fh.write(scan.viz) }
end
