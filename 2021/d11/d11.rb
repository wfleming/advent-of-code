#!/usr/bin/env ruby

require "set"

def flash(octopi, y, x)
  neighbors = [
    [ y - 1, x - 1],
    [ y - 1, x],
    [ y - 1, x + 1],
    [ y, x - 1],
    [ y, x + 1],
    [ y + 1, x - 1],
    [ y + 1, x],
    [ y + 1, x + 1],
  ].reject { |p| !(0...octopi.length).include?(p[0]) || !(0...octopi[0].length).include?(p[1]) }
  neighbors.each { |p| octopi[p[0]][p[1]] += 1 }
end

def flash_ready_coords(octopi)
  octopi.flat_map.with_index do |line, y|
    line.map.with_index { |o, x| [y, x] if o > 9 }.compact
  end.to_set
end

def step(octopi)
  new_octopi = octopi.map { |line| line.map { |octopus| octopus + 1 } }

  # puts "DEBUG: after increment\n#{new_octopi.map { |l| l.join(",") }.join("\n")}\n----------"
  flashed = Set.new
  until flashed == flash_ready_coords(new_octopi)
    (flash_ready_coords(new_octopi) - flashed).each do |p|
      flash(new_octopi, p[0], p[1])
      flashed << p
      # puts "DEBUG: just flashed y=#{p[0]} x=#{p[1]}, flashed=#{flashed}\n#{new_octopi.map { |l| l.join(",") }.join("\n")}\n---------"
    end
  end

  flashed.each { |p| new_octopi[p[0]][p[1]] = 0 }

  new_octopi
end

def count_flashed(octopi)
  octopi.sum { |l| l.count { |o| o == 0 } }
end

def generations(octopi, n)
  flash_count = 0

  n.times do
    octopi = step(octopi)
    flash_count += count_flashed(octopi)
  end

  [octopi, flash_count]
end

if __FILE__ == $0
  octopi = File.readlines(ARGV[0]).map { |l| l.chomp.each_char.map(&:to_i) }

  g = 100
  puts "p1: after #{g} generations there were #{generations(octopi, g)[1]} flashes"

  t = 0
  cur_octopi = octopi
  total_octopi = octopi.sum(&:count)
  until total_octopi == count_flashed(cur_octopi)
    cur_octopi = step(cur_octopi)
    t += 1
  end
  puts "p2: flashes are synchronized at t=#{t}"
end
