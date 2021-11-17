#!/usr/bin/env ruby

require "set"

class SmallestGroupFinder
  attr_reader :groups

  def initialize(packages, group_count)
    @packages = packages.sort.reverse
    @groups = []
    @group_count = group_count
    @group_weight = @packages.sum / group_count
  end

  def populate!
    @packages.each_with_index do |p, idx|
      remaining_packages = @packages[idx+1..].select { |p2| p2 + p <= @group_weight }
      recurse([p].to_set, remaining_packages)
    end
  end

  def recurse(group, remaining_packages)
    # since we really only need the smallest valid groups, and each recursion
    # makes longer groups, we can short-circuit and stop recursing as soon as we
    # find some valid groups
    if @groups.any? && group.count > @groups[0].count
      return
    elsif group.sum == @group_weight && (@groups.empty? || group.count == @groups[0].count)
      @groups << group
    else
      remaining_packages.each_with_index.map do |p, idx|
        next_remaining_packages = remaining_packages[idx+1..].select { |p2| group.sum + p + p2 <= @group_weight }
        recurse(group + [p], next_remaining_packages)
      end
    end
  end
end

def quantum_entanglement(packages)
  packages.inject(&:*)
end

if __FILE__ == $0
  packages = File.readlines(ARGV[0]).map(&method(:Integer))
  p1_groups = SmallestGroupFinder.new(packages, 3).tap(&:populate!).groups
  smallest_group_size = p1_groups.map(&:count).min
  best_front_group = p1_groups.select { |g| g.count == smallest_group_size }.sort_by(&method(:quantum_entanglement))[0]
  puts "p1: quantum entanglement of first group in ideal config: #{quantum_entanglement(best_front_group)}"

  p2_groups = SmallestGroupFinder.new(packages, 4).tap(&:populate!).groups
  smallest_group_size = p2_groups.map(&:count).min
  best_front_group = p2_groups.select { |g| g.count == smallest_group_size }.sort_by(&method(:quantum_entanglement))[0]
  puts "p2: quantum entanglement of first group in ideal config: #{quantum_entanglement(best_front_group)}"
end
