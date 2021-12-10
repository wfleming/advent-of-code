#!/usr/bin/env ruby

require "set"

Point = Struct.new(:x, :y, :z) do
  def self.parse(str)
    self.new(*str.split(",").map(&method(:Integer)))
  end

  def to_s
    "(#{x}, #{y}, #{z})"
  end

  def +(other)
    self.class.new(x + other.x, y + other.y, z + other.z)
  end

  def magnitude
    [x, y, z].sum(&:abs)
  end

  def distance(other)
    [x - other.x, y - other.y, z - other.z].sum(&:abs)
  end
end

Particle = Struct.new(:id, :p, :v, :a) do
  PAT = /p=<(.+)>, v=<(.+)>, a=<(.+)>/

  def self.all(filename)
    File.readlines(filename).map.with_index do |line, idx|
      self.parse(idx, line)
    end
  end

  def self.parse(id, str)
    m = PAT.match(str)
    self.new(id, Point.parse(m[1]), Point.parse(m[2]), Point.parse(m[3]))
  end

  def to_s
    "<Particle #{id} p=#{p} v=#{v} a=#{a}>"
  end

  def step
    next_v = v + a
    self.class.new(id, p + next_v, next_v, a)
  end

  def can_turn?
    !((v.x.positive? == a.x.positive? || a.x.zero?) &&
      (v.y.positive? == a.y.positive? || a.y.zero?) &&
      (v.z.positive? == a.z.positive? || a.z.zero?))
  end
end

class Simulator
  attr_reader :particles, :t

  def initialize(particles)
    @particles = particles
    @t = 0
  end

  # we're stable & can stop stimulating once all particles not getting closer to
  # any other particles
  def stable?
    @particles.each_with_index.all? do |p1, idx|
      @particles.drop(idx + 1).all? do |p2|
        !p2.can_turn? && !p2.can_turn? &&
          p1.p.distance(p2.p) <= p1.step.p.distance(p2.step.p)
      end
    end
  end

  def run
    step until stable?
  end

  def step
    # puts "DEBUG: step t=#{t} particles left=#{@particles.count}"
    @particles = @particles.map { |p| p.step }
    @particles.each_with_index do |p1, idx|
      collisions = @particles.select do |p2|
        p1 != p2 && p1.p == p2.p
      end
      @particles -= collisions
    end
    @t += 1
  end
end

if __FILE__ == $0
  particles = Particle.all(ARGV[0]).sort_by { |p| p.a.magnitude }
  lowest_acc = particles.select { |p| p.a.magnitude == particles[0].a.magnitude }
  lowest_acc = lowest_acc.sort_by { |p| p.v.magnitude }
  # theoretically if there was also a tie for initial speed I'd go to nearest
  # initial position, but it wasn't needed
  puts "p1: particle #{lowest_acc[0]} will stay closest to origin in the long run"

  s = Simulator.new(particles)
  s.run
  puts "p2: #{s.particles.count} particles left (took #{s.t} steps, if you're curious)"
end
