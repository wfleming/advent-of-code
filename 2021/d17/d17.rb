#!/usr/bin/env ruby

Point = Struct.new(:x, :y) do
  def to_s; "(#{x}, #{y})"; end

  def +(other)
    self.class.new(x + other.x, y + other.y)
  end

  def next_vel
    self.class.new(
      x.zero? ? 0 : x - (x.abs / x),
      y - 1,
    )
  end
end

def parse_target_range(line)
  m = /target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)/.match(line)

  { x: m[1].to_i..m[2].to_i, y: m[3].to_i..m[4].to_i }
end

def possible_vels(target_range)
  vs = []

  # example & input consistently target a positive x and negative y,
  # so v.x can't start < 0, but v.y can
  x_range = 0..target_range[:x].max
  # theoretically higher initial y vels could work, but not with the target x
  # ranges we're working with and limited to integral x velocities
  y_range = (0 - target_range[:y].map(&:abs).max)..target_range[:y].map(&:abs).max
  x_range.each do |start_x_vel|
    y_range.each do |start_y_vel|
      pos = Point.new(0, 0)
      vel = Point.new(start_x_vel, start_y_vel)

      # because target x is always > 0 and target y < 0, we've overshot once
      # current x > highest target x or y < lowest target y
      while pos.x <= target_range[:x].max && pos.y >= target_range[:y].min
        pos = pos + vel
        vel = vel.next_vel

        if target_range[:x].include?(pos.x) && target_range[:y].include?(pos.y)
          vs << Point.new(start_x_vel, start_y_vel)
          break
        end
      end
    end
  end

  vs
end

def max_y(init_vel)
  vel = init_vel
  pos = Point.new(0, 0)
  t = 0
  max_y = 0

  while pos.y >= max_y
    pos = pos + vel
    vel = vel.next_vel
    max_y = pos.y if pos.y > max_y
  end

  max_y
end

target_range = parse_target_range(File.read(ARGV[0]))

vels = possible_vels(target_range)

max_y_vel = vels.max_by(&:y)
highest_y = max_y(max_y_vel)

puts "target range: #{target_range}"
puts "p1: best initial velocity is #{max_y_vel}, whose apex is y=#{highest_y}"
puts "p2: there are #{vels.count} workable velocity solutions"
