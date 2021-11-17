#!/usr/bin/env ruby

require "set"

def valid_groups(packages, group_count, group_weight = nil, memo = nil)
  if memo.nil?
    group_weight = packages.sum / group_count
    packages.each_with_index.flat_map do |p, idx|
      remaining_packages = packages[idx+1..].select { |p2| p2 + p <= group_weight }
      valid_groups(remaining_packages, group_count, group_weight, [p].to_set)
    end
  else
    # pairs of [group, remaining_packages]
    next_group_pairs = packages.each_with_index.map do |p, idx|
      remaining_packages = packages[idx+1..].select { |p2| memo.sum + p + p2 <= group_weight }
      [memo + [p], remaining_packages]
    end
    # since we really only need the smallest valid groups, and each recursion
    # makes longer groups, we can short-circuit and stop recursing as soon as we
    # find some valid groups
    if next_group_pairs.any? { |p| p[0].sum == group_weight }
      return next_group_pairs.map { |p| p[0] }.select { |g| g.sum == group_weight }
    else
      next_group_pairs.flat_map do |p|
        valid_groups(p[1], group_count, group_weight, p[0])
      end
    end
  end
end

def quantum_entanglement(packages)
  packages.inject(&:*)
end

if __FILE__ == $0
  # input is already sorted - we reverse it since picking bigger items first
  # will make smaller groups
  packages = File.readlines(ARGV[0]).map(&method(:Integer)).sort.reverse
  p1_groups = valid_groups(packages, 3)
  smallest_group_size = p1_groups.map(&:count).min
  best_front_group = p1_groups.select { |g| g.count == smallest_group_size }.sort_by(&method(:quantum_entanglement))[0]
  puts "p1: quantum entanglement of first group in ideal config: #{quantum_entanglement(best_front_group)}"

  p2_groups = valid_groups(packages, 4)
  smallest_group_size = p2_groups.map(&:count).min
  best_front_group = p2_groups.select { |g| g.count == smallest_group_size }.sort_by(&method(:quantum_entanglement))[0]
  puts "p2: quantum entanglement of first group in ideal config: #{quantum_entanglement(best_front_group)}"
end
