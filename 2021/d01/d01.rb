#!/usr/bin/env ruby

depths = File.readlines(ARGV[0]).map(&method(:Integer))

def count_increases(depths)
  depths.each_with_index.count do |d, idx|
    depths[idx + 1] && depths[idx + 1] > d
  end
end

def windowed_data(depths)
  (0..(depths.count - 3)).map do |window_start|
    depths[window_start..(window_start + 2)].sum
  end
end

p1_increases = count_increases(depths)
puts "p1: depth increased #{p1_increases} times"

p2_increases = count_increases(windowed_data(depths))
puts "p2: depth increased #{p2_increases} times"
