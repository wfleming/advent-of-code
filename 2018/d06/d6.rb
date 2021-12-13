Point = Struct.new(:x, :y) do
  def dist(c2)
    (self.x - c2.x).abs + (self.y - c2.y).abs
  end

  def to_s
    "(#{x}, #{y})"
  end
end

def parse_coords(input)
  input.lines.map do |line|
    m = /(\d+), (\d+)/.match(line)
    Point.new(m[1].to_i, m[2].to_i)
  end
end

class Grid
  attr_reader :coords

  def initialize(coords)
    @coords = coords
  end

  def min_x
    @min_x ||= coords.map(&:x).min
  end

  def min_y
    @min_y ||= coords.map(&:y).min
  end

  def max_x
    @max_x ||= coords.map(&:x).max
  end

  def max_y
    @max_y ||= coords.map(&:y).max
  end

  def out_of_bounds?(coord)
    coord.x < min_x || coord.x > max_x || coord.y < min_y || coord.y > max_x
  end
end

module Areas
  def self.calculate(grid)
    Hash[
      self.calculate_points(grid).map do |c, points|
        if points.include?(Float::INFINITY)
          [c, Float::INFINITY]
        else
          [c, points.length]
        end
      end
    ]
  end

  def self.calculate_points(grid)
    # coord -> list of points
    h = {}

    x_start = [grid.min_x - 1, 0].max
    y_start = [grid.min_y - 1, 0].max
    x_end = grid.max_x + 1
    y_end = grid.max_y + 1

    (x_start..x_end).each do |x|
      (y_start..y_end).each do |y|
        p = Point.new(x, y)

        closest = self.closest_to(p, grid)
        next if closest.nil?
        h[closest] ||= []
        if grid.out_of_bounds?(p)
          h[closest] << Float::INFINITY
        else
          h[closest] << p
        end
      end
    end

    h
  end

  # debugging: print out the grid
  def self.viz(grid)
    if grid.coords.count > 26
      raise ArgumentError, "can't viz this many points"
    end

    names = Hash[grid.coords.zip('a'..'z')]

    x_start = [grid.min_x - 1, 0].min
    y_start = [grid.min_y - 1, 0].min
    x_end = grid.max_x + 1
    y_end = grid.max_y + 1

    (y_start..y_end).each do |y|
      (x_start..x_end).each do |x|
        p = Point.new(x, y)

        closest = self.closest_to(p, grid)
        if closest.nil?
          $stdout << '.'
        else
          n = names[closest]
          if p.dist(closest) == 0
            $stdout << n.upcase
          else
            $stdout << n
          end
        end
      end

      $stdout << "\n"
    end
  end

  # returns nil if equally far from several coords
  def self.closest_to(point, grid)
    dists = Hash[
      grid.coords.map do |c|
        [c, point.dist(c)]
      end
    ]

    min = dists.values.min

    if dists.values.count { |d| d == min } > 1
      # equally far from several coords
      nil
    else
      dists.find { |_, d| d == min }[0]
    end
  end
end

class Safe
  attr_reader :grid, :dist_cutoff, :reporter

  def initialize(grid, dist_cutoff: 10_000, reporter: nil)
    @grid = grid
    @dist_cutoff = dist_cutoff
    @reporter = reporter
  end

  def region
    points_seen = 0

    @region ||= (min_y..max_y).flat_map do |y|
      ps = (min_x..max_x).map do |x|
        points_seen += 1

        p = Point.new(x, y)

        reporter.progress(points_seen, total_points) if reporter

        p if in_range?(p)
      end.compact
      ps
    end.compact
  end

  private

  def in_range?(p)
    grid.coords.inject(0) do |cum_dist, coord|
      cum_dist += p.dist(coord)
      return false if cum_dist >= dist_cutoff
      cum_dist
    end

    true
  end

  def x_range
    max_x - min_x
  end

  def y_range
    max_y - min_y
  end

  def total_points
    @total_points = x_range * y_range
  end

  def min_y
    [grid.min_y - dist_cutoff, 0].max
  end

  def max_y
    grid.max_y + dist_cutoff
  end

  def min_x
    [grid.min_x - dist_cutoff, 0].max
  end

  def max_x
    grid.max_x + dist_cutoff
  end
end

class P2Reporter
  def progress(points_seen, total_points)
    return unless points_seen % 100_000 == 0
    pct = ((points_seen.to_f / total_points) * 100).round(2)

    if @line_written
      print "\033[1A\033["
    else
      @line_written = true
    end

    puts "progress: #{pct}%"
  end
end

if $0 == __FILE__
  coords = parse_coords(File.read(ARGV[0]))
  g = Grid.new(coords)

  areas = Areas.calculate(g)

  largest_area = areas.values.reject { |v| v.infinite? }.max
  largest_coord = areas.find { |_, v| v == largest_area }[0]

  puts "p1: coord #{largest_coord} has largest finite area #{largest_area}"

  s = Safe.new(g, reporter: P2Reporter.new)
  puts "p2: size of safe region is #{s.region.count}"
end
