#!/usr/bin/env ruby

def next_num(num)
  # num is a string of digits, not a real number
  num.chars.chunk(&:itself).map do |c, cs|
    "#{cs.count}#{c}"
  end.join("")
end

def iterate(num, stretches)
  stretches.times.inject(num) do |memo, _i|
    next_num(memo)
  end
end

p1 = iterate(ARGV[0], 40)
puts "p1: #{p1.length}"

p2 = iterate(p1, 10)
puts "p2: #{p2.length}"
