class Battery
  attr_reader :x, :y, :serial

  def initialize(x, y, serial)
    @x = x
    @y = y
    @serial = serial
  end

  def power
    rack_id = x + 10
    power = ((rack_id * y) + serial) * rack_id

    power / 100 % 10 - 5
  end
end

def cached_power(serial)
  Hash.new do |h, k|
    h[k] = Battery.new(k[0], k[1], serial).power
  end
end

def p1(powers)
  winner_coord = nil
  winner_power = nil

  (1..298).each do |x|
    (1..298).each do |y|
      coords = (0..2).flat_map do |dx|
        (0..2).map do |dy|
          [x + dx, y + dy]
        end
      end
      unit_power = coords.map { |coord| powers[coord] }.sum

      if winner_power.nil? || winner_power < unit_power
        winner_power = unit_power
        winner_coord = [x, y]
      end
    end
  end

  [winner_coord, winner_power]
end

# https://en.wikipedia.org/wiki/Summed-area_table
def summed_area_power(solo_powers)
  Hash.new do |h, k|
    x, y = *k

   if x < 1 || y < 1
     0
    else
      h[k] = solo_powers[[x, y]] +
        h[[x, y - 1]] +
        h[[x - 1, y]] -
        h[[x - 1, y - 1]]
    end
  end
end

class AreaCalculator
  attr_reader :summed_power

  def self.from_serial(serial)
    new(summed_area_power(cached_power(serial)))
  end

  def self.from_solo_powers(powers)
    new(summed_area_power(powers))
  end

  def initialize(summed_power)
    @summed_power = summed_power
  end

  def [](k)
    x, y, w = *k

    a = summed_power[[x - 1, y - 1]]
    b = summed_power[[x + w - 1, y - 1]]
    c = summed_power[[x - 1, y + w - 1]]
    d = summed_power[[x + w - 1, y + w - 1]]

    a + d - b - c
  end
end

def p2(solo_powers, progress: false)
  calc = AreaCalculator.from_solo_powers(solo_powers)

  winner_coord = nil
  winner_power = nil

  (1..300).each do |width|
    print "\033[1A\033[" if @progress_printed
    @progress_printed = true
    puts "_ p2: #{width}/300"

    (1..(300 - width)).each do |x|
      (1..(300 - width)).each do |y|
        unit_power = calc[[x, y, width]]

        if winner_power.nil? || winner_power < unit_power
          winner_power = unit_power
          winner_coord = [x, y, width]
        end
      end
    end
  end

  [winner_coord, winner_power]
end

if $0 == __FILE__
  serial = ARGV[0].to_i

  powers = cached_power(serial)

  winner_coord, winner_power = *p1(powers)

  puts "p1: section starting at #{winner_coord.join(",")} has power #{winner_power}"

  winner_coord, winner_power = *p2(powers, progress: true)
  puts "p2: section starting at #{winner_coord.map(&:to_s).join(",")} has power #{winner_power}"
end
