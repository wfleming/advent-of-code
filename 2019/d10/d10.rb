#!/usr/bin/env ruby

MAP_INPUT = ARGV[0]

Pos = Struct.new(:x, :y) do
  def to_s
    "(#{x}, #{y})"
  end
end

PosRelative = Struct.new(:pos, :angle, :dist) do
  def to_s
    "{pos=#{pos} angle=#{angle.round(3)} dist=#{dist.round(3)}}"
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

def part1
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

def dist_between(p1, p2)
  Math.sqrt((p2.x - p1.x) ** 2 + (p2.y - p1.y) ** 2)
end

def angle_between(p1, p2)
  rads = Math.atan2(p1.y - p2.y, p1.x - p2.x)
  degs = rads * 180.0 / Math::PI

  # orient & correct for direction: for us 0 is up.
  # My brain could not work this out last night. The solution at
  # https://github.com/DanaL/AdventOfCode/blob/master/2019/src/day_ten.rs#L49
  # pointed me in the correct direction. After a night's sleep and some fiddling
  # with Ruby's `atan2` I figured this out
  circle = 360.0
  ((0.75 * circle) + degs) % circle
end

def part2(base, nth = 200)
  # DEBUG
  # as = [Pos.new(8,1), Pos.new(10, 3), Pos.new(8, 6), Pos.new(3, 3)]
  # # angles should come out as 0, 90, 180, 270
  # as.each do |a|
  #   puts "angle_between(#{base}, #{a})=#{angle_between(base,a)}"
  #   puts "dist_between(#{base}, #{a})=#{dist_between(base,a)}"
  # end
  # exit
  # END DEBUG

  vaporize_order = []
  last_angle = -0.0000001 # start jusssst before 0
  asteroids = parse_asteroids.dup.filter do |p|
    p != base
  end.map do |p|
    PosRelative.new(p, angle_between(base, p), dist_between(base, p))
  end.sort_by do |p|
    [p.angle, p.dist]
  end

  while vaporize_order.count < nth && asteroids.any?
    next_vaporize = asteroids.filter do |p|
      p.angle > last_angle
    end[0]

    if next_vaporize.nil?
      last_angle = -0.000000001
    else
      # puts "DEBUG loop: nth=#{nth} vaporized=#{vaporize_order.count} left=#{asteroids.count} will vaporize #{next_vaporize} next"
      vaporize_order << next_vaporize
      last_angle = next_vaporize.angle
      asteroids.delete(next_vaporize)
    end
  end

  nth_vaporized = vaporize_order.last.pos
  puts "p2: #{vaporize_order.count}th vaporized will be #{nth_vaporized}. answer = #{nth_vaporized.x * 100 + nth_vaporized.y}"
end

# part1

#base = Pos.new(8, 3) # for the sample input
#part2(base, 10)

base = Pos.new(20, 18) # calculated in p1
part2(base, 200)
