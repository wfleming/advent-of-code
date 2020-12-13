#!/usr/bin/env ruby

Input = Struct.new(:time, :buses) do
  def self.read(path)
    ls = File.readlines(path)
    new(
      Integer(ls[0]),
      ls[1].split(",").map { |i|
        if i == "x"
          nil
        else
          Integer(i)
        end
      },
    )
  end
end

def p1(input)
  bus_mod = input.buses.compact.map { |b|
    m = input.time % b
    m = m == 0 ? m : b - m
    [b, m]
  }.sort_by { |x| x[1] }

  bus_mod[0]
end

input = Input.read(ARGV[0])

# p1
p1_bus, p1_bus_time = *p1(input)
puts "p1: first bus is #{p1_bus}, you'll wait #{p1_bus_time} mins. it leaves at #{input.time + p1_bus_time}"
puts "p1: bus * wait = #{p1_bus * p1_bus_time}"

# p2
# find n such that (buses[i] % n + i) == 0 for all i
# that can be rearranged as buses[i] % n == buses[i] - i (except when i = 0,
# then it == 0)
buses_with_remainders = input.buses.each_with_index.to_a
  .reject { |p| p[0].nil? }
  .map { |p|
    if p[1] == 0
      [p[0], 0]
    else
      [p[0], p[0] - p[1]]
    end
  }

# https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
def bezout(a, b)
  old_r, r = *[a, b]
  old_s, s = *[1, 0]
  old_t, t = *[0, 1]

  while r != 0
    quotient = old_r / r
    old_r, r = *[r, old_r - (quotient * r)]
    old_s, s = *[s, old_s - (quotient * s)]
    old_t, t = *[t, old_t - (quotient * t)]
  end

  [old_s, old_t]
end

# https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Using_the_existence_construction
def reduce_modulus(bus1, bus2)
  x, y = *bezout(bus1[0], bus2[0])
  z = (bus1[0] * x * bus2[1]) + (bus2[0] * y * bus1[1])

  # puts "DEBUG reduce_modulus x=#{x}, y=#{y}, z=#{z}"
  new_remainder = z % (bus1[0] * bus2[0])
  [bus1[0] * bus2[0], new_remainder]
end

ans = buses_with_remainders.reduce { |memo, x|
  # puts "DEBUG memo = #{memo} x = #{x}"
  reduce_modulus(memo, x)
}

puts "p2: t=#{ans[1]}"
