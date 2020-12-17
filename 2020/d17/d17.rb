#!/usr/bin/env ruby

require "set"

Point = Struct.new(:x, :y, :z) do
  def +(other)
    Point.new(x + other.x, y + other.y, z + other.z)
  end
end

class Cubes
  def self.parse(str)
    seed_active = str.lines.each_with_index.flat_map { |line, y|
      line.each_char.each_with_index.map { |c, x|
        Point.new(x, y, 0) if c == "#"
      }
    }.compact

    new(seed_active)
  end

  attr_reader :active

  def initialize(active)
    @active = Set.new(active)
  end

  def run_cycle
    to_activate = []
    to_deactivate = []

    active.each do |point|
      ns = neighbors(point)

      unless [2, 3].include?(ns.count(&method(:active?)))
        to_deactivate << point
      end

      ns.each do |n|
        to_activate << n if !active?(n) && neighbors(n).count(&method(:active?)) == 3
      end
    end

    to_activate.each(&method(:activate))
    to_deactivate.each(&method(:deactivate))
  end

  def run_cycles(n)
    n.times { run_cycle }
  end

  def neighbors(point)
    @neighbor_vectors ||= neighbor_vectors
    @neighbor_vectors.map { |v| point + v }
  end

  def active?(point)
    active.include?(point)
  end

  def activate(point)
    active << point
  end

  def deactivate(point)
    active.delete(point)
  end

  private

  def neighbor_vectors(memo = [[-1], [0], [1]])
    if memo[0].length == 3
      return memo.reject { |p| p == [0, 0, 0] }.map { |p| Point.new(*p) }
    end

    neighbor_vectors(
      memo.flat_map { |p|
        [
          [-1] + p,
          [0] + p,
          [1] + p,
        ]
      }
    )
  end
end

# p1
cubes = Cubes.parse(File.read(ARGV[0]))
cubes.run_cycles(6)
puts "p1: after 6 cycles, #{cubes.active.count} cubes are active"
