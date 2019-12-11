#!/usr/bin/env ruby

MAP_INPUT = ARGV[0]

Pos = Struct.new(:x, :y) do
  def to_s
    "(#{x}, #{y})"
  end
end

def parse_asteroids
  $asteroids ||= [].tap do |list|
    File.read(MAP_INPUT).lines.each_with_index do |line, y|
      line.each_char.each_with_index do |char, x|
        if char == "#"
          list << Pos.new(x, y)
        end
      end
    end
  end
end

# test if pos lines on the line between a & b
def between(line_a, line_b, pos)
  # short-circuit if outside bounds of line
  if pos.x > [line_a.x, line_b.x].max ||
      pos.y > [line_a.y, line_b.y].max ||
      pos.x < [line_a.x, line_b.x].min ||
      pos.y < [line_a.y, line_b.y].min
    # puts "DEBUG - short circuit false"
    return false
  end

  if line_a.x == line_b.x # horizontal line
    # puts "DEBUG - horizontal line"
    pos.x == line_a.x
  elsif line_a.y == line_b.y # vertical line
    # puts "DEBUG - vertical line"
    pos.y == line_a.y
  else
    (pos.y - line_a.y) * (line_b.x - line_a.x) ==
      (pos.x - line_a.x) * (line_b.y - line_a.y)
  end
end

def asteroids_visible_from(asteroids, pos)
  # puts "DEBUG asteroids_visible_from #{pos}"
  asteroids.count do |asteroid|
    visible = asteroid != pos &&
      asteroids.filter { |a| a != asteroid && a != pos}.none? do |potential_blocker|
        between(pos, asteroid, potential_blocker)
      end
    # puts "  inner loop asteroid=#{asteroid} visible=#{visible}"
    visible
  end
end

def p1
  asteroids = parse_asteroids.dup

  best_pos = nil
  best_count = 0

  asteroids.each do |pos|
    if (c = asteroids_visible_from(asteroids, pos)) > best_count
      best_pos = pos
      best_count = c
    end
  end

  puts "p1: best_pos=#{best_pos} with #{best_count} visible"
end

def p2(base, nth = 200)
  asteroids = parse_asteroids.dup

  vaporized = 0
  degree = 0

  while true
    degree += 1
  end
end

p1

#base = Pos.new(20, 18) # calculated in p1
#p2(base)
