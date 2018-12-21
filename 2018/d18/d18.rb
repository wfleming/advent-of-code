require "pry" #DEBUG
require "forwardable"

Point = Struct.new(:x, :y)

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
    storage[pt.x][pt.y]
  end

  def []=(pt, val)
    storage[pt.x][pt.y] = val
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

class Forest
  extend Forwardable

  attr_reader :grid

  def_delegators :@grid, :[], :[]=, :count

  def self.parse(str)
    lines = str.lines.map(&:strip)
    h = lines.count
    w = lines[0].length
    forest = self.new(w, h)
    lines.each_with_index do |line, y|
      line.each_char.each_with_index do |c, x|
        forest[Point.new(x, y)] =
          case c
          when "."
            :open
          when "#"
            :lumberyard
          when "|"
            :trees
          else
            raise "Unexpected char '#{c}' at (#{x}, #{y})"
          end
      end
    end
    forest
  end

  def initialize(width, height)
    @grid = Grid.new(width, height)
  end

  def dup
    new(width, height).tap do |other|
      other.instance_variable_set(:@grid, @grid.dup)
    end
  end

  def min_x
    0
  end

  def max_x
    @grid.width - 1
  end

  def min_y
    0
  end

  def max_y
    @grid.height - 1
  end

  def out_of_bounds?(pt)
    pt.x > max_x || pt.x < min_x || pt.y > max_y || pt.y < min_y
  end

  def neighbors(pt)
    ((pt.x - 1)..(pt.x + 1)).flat_map do |x|
      ((pt.y - 1)..(pt.y + 1)).map do |y|
        Point.new(x, y)
      end
    end.reject { |pt2| pt2 == pt || out_of_bounds?(pt2) }
  end

  def neighbor_contents(pt)
    neighbors(pt).map { |pt2| self[pt2] }
  end

  def tick
    new_grid = @grid.dup
    new_grid.each do |pt, _|
      ns = neighbor_contents(pt)
      new_state =
        case self[pt]
        when :open
          if ns.select { |n| n == :trees }.count >= 3
            :trees
          else
            self[pt]
          end
        when :trees
          if ns.select { |n| n == :lumberyard }.count >= 3
            :lumberyard
          else
            self[pt]
          end
        when :lumberyard
          if ns.any? { |n| n == :lumberyard } && ns.any? { |n| n == :trees }
            :lumberyard
          else
            :open
          end
        else
          raise "invalid state #{self[pt.y]} at #{pt}"
        end
      new_grid[pt] = new_state
    end

    @grid = new_grid
  end

  def viz
    (min_y..max_y).map do |y|
      line = "." * (1 + max_x - min_x)
      (min_x..max_x).each do |x|
        case self[Point.new(x, y)]
        when :trees
          line[x] = "|"
        when :lumberyard
          line[x] = "#"
        end
      end
      line
    end.join("\n")
  end
end

def p1(f)
  10.times { f.tick }
  lumberyards = f.count { |_, v| v == :lumberyard }
  trees = f.count { |_, v| v == :trees }
  [lumberyards, trees]
end

def find_loop(f)
  states = [f.grid.dup]
  progress_printed = false

  100_000.times do |i|
    f.tick
    states << f.grid.dup

    break if states.uniq.count != states.count

    print "\033[1A\033[" if progress_printed
    progress_printed = true
    puts "_ finding loop: #{i}"
  end

  if states.count >= 10_000
    raise "didn't find a loop"
  end

  [states, f]
end

def p2(f)
  goal_iterations = 1_000_000_000
  states, f = *find_loop(f)
  last_idx = states.count - 1
  first_idx = states.index(states[-1])
  puts "found loop: states[#{first_idx}]=#{states[first_idx]} equals states[#{last_idx}]=#{states[last_idx]}"

  # sanity check
  loop_width = (last_idx - first_idx)
  loop_width.times { f.tick }
  match = f.grid == states[last_idx]
  puts "sanity check: did #{loop_width} more loops, state is equal #{match.inspect}"
  unless match
    raise "failed sanity check"
  end

  further_ticks = (goal_iterations - first_idx) % loop_width
  puts "need to do #{further_ticks} more ticks"
  further_ticks.times { f.tick }

  lumberyards = f.count { |_, v| v == :lumberyard }
  trees = f.count { |_, v| v == :trees }
  puts "p2: lumberyards=#{lumberyards} trees=#{trees} value=#{lumberyards * trees}"
end

if $0 == __FILE__
  f = Forest.parse(File.read(ARGV[0]))
  p1r = p1(f)
  puts "p1: after 10 minutes lumberyards=#{p1r[0]} trees=#{p1r[1]} value=#{p1r[0] * p1r[1]}"

  f = Forest.parse(File.read(ARGV[0]))
  p2(f)
end
