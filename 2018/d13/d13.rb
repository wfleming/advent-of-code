class Cart
  attr_reader :id
  attr_accessor :x, :y, :dir, :turns

  LEFT = {
    "^" => "<",
    "v" => ">",
    "<" => "v",
    ">" => "^",
  }

  RIGHT = {
    "^" => ">",
    "v" => "<",
    "<" => "^",
    ">" => "v",
  }

  def self.next_id
    @id ||= 0
    @id += 1
  end

  def initialize(x, y, dir)
    @id = self.class.next_id
    @x = x
    @y = y
    @dir = dir
    @turns = 0
  end

  def to_s
    "<Cart id=#{id} x=#{x} y=#{y} dir=#{dir} turns=#{turns}>"
  end

  def ==(other)
    x == other.x && y == other.y && dir == other.dir && turns == other.turns
  end

  def move!
    case dir
    when "^"
      self.y = y - 1
    when "v"
      self.y = y + 1
    when "<"
      self.x = x - 1
    when ">"
      self.x = x + 1
    end
  end

  def intersection_turn!
    @dir = intersection_turn_dir
    @turns += 1
  end

  # direction to face at intersection
  def intersection_turn_dir
    case @turns % 3
    when 0
      LEFT[dir]
    when 1
      dir # straight
    when 2
      RIGHT[dir]
    end
  end

  def turn_at!(char)
    case char
    when "/"
      turn_slash!
    when "\\"
      turn_backslash!
    when "+"
     intersection_turn!
    end
  end

  def turn_slash!
    self.dir =
      case dir
      when "^"
        ">"
      when "<"
        "v"
      when "v"
        "<"
      when ">"
        "^"
      end
  end

  def turn_backslash!
    self.dir =
      case dir
      when "^"
        "<"
      when ">"
        "v"
      when "v"
        ">"
      when "<"
        "^"
      end
  end
end

class Map
  attr_reader :lines

  def initialize(lines)
    @lines = lines
  end

  def [](x, y)
    lines[y][x]
  rescue NoMethodError
    raise "tried to address #{x}, #{y}: map has w=#{width}, h=#{height}"
  end

  def width
    lines.map { |l| l.length }.max
  end

  def height
    lines.count
  end
end

class State
  attr_reader :map, :time, :carts

  def initialize(map, delete_crashes: false)
    @map = map
    @time = 0
    @carts = init_carts
    @delete_crashes = delete_crashes
  end

  def tick
    @time += 1

    ids_to_del = []
    carts.sort_by { |c| [c.y, c.x] }.each do |cart|
      next if ids_to_del.include?(cart.id)
      cart.move!

      # error if we fell off track
      unless %w[- | + / \\].include?(map[cart.x, cart.y])
        raise "cart fell off track: #{cart.to_s}"
      end

      if %w[/ \\ +].include?(map[cart.x, cart.y])
        cart.turn_at!(map[cart.x, cart.y])
      end

      # need to check for crash after each move
      if crashed?(cart)
        if @delete_crashes
          ids_to_del += carts.select { |c| c.x == cart.x && c.y == cart.y }.map(&:id)
        else
          break
        end
      end
    end
    carts.reject! { |c| ids_to_del.include?(c.id) }
  end

  def viz
    lines = map.lines.map(&:dup)
    carts.each do |c|
      lines[c.y][c.x] = c.dir
    end
    lines.join("\n")
  end

  def crashed?(cart1)
    carts.any? do |cart2|
      cart1.id != cart2.id && cart1.x == cart2.x && cart1.y == cart2.y
    end
  end

  def any_crash?
    carts.map { |c| [c.x, c.y] }.uniq.count != carts.count
  end

  def init_carts
    carts = []
    map.lines.each_with_index do |line, y|
      line.each_char.each_with_index do |c, x|
        if %w[^ v < >].include?(c)
          carts << Cart.new(x, y, c)
          # fill back in the appropriate track for tracking later
          line[x] = (%w[< >].include?(c) ? "-" : "|")
        end
      end
    end
    carts
  end
end

def p1(state)
  until state.any_crash?
    state.tick
  end

  pts = state.carts.map { |c| [c.x, c.y] }.group_by
  pts.group_by { |p| p }.select { |_, v| v.count > 1 }.keys
end

def p2(state)
  until state.carts.count < 2
    state.tick
  end

  c = state.carts[0]
  [c.x, c.y]
end

if $0 == __FILE__
  lines = File.read(ARGV[0]).lines.map(&:chomp)

  state = State.new(Map.new(lines.map(&:dup)))
  crashes = p1(state)

  puts "p1: first crash is at #{crashes}"

  state = State.new(Map.new(lines.map(&:dup)), delete_crashes: true)
  puts "p2: last surviving cart is at #{p2(state)}"
end
