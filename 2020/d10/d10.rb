#!/usr/bin/env ruby

def read_joltages
  File.readlines(ARGV[0]).map { |l| Integer(l) }.sort.tap do |a|
    a.unshift(0) # the wall
    a << a[-1] + 3 # the device
  end
end

def joltage_diffs(joltages)
  jumps = Hash.new(0)

  # at each idx, calculate diff of next & current
  (0..(joltages.count - 2)).each do |idx|
    diff = joltages[idx + 1] - joltages[idx]
    jumps[diff] = jumps[diff] + 1
  end

  jumps
end

# Hash of joltage => immediate predecessor states
def build_reverse_vertexes(joltages)
  paths = {}

  joltages_r = joltages.reverse
  joltages_r.each_index do |i|
    next if i == joltages_r.count - 1

    dest = joltages_r[i]
    pred_states = joltages_r.drop(i + 1).take(3).select { |x| x >= dest - 3 }

    paths[dest] = pred_states
  end

  paths
end

def count_paths_to(x, reverse_vs)
  # the wall is the base case
  return 1 if x == 0

  $path_count_cache ||= {} # map x => the known answer
  return $path_count_cache[x] if $path_count_cache.key?(x)

  preds = reverse_vs[x]

  preds.map { |p| count_paths_to(p, reverse_vs) }.sum.tap do |ans|
    $path_count_cache[x] ||= ans
  end
end

joltages = read_joltages.freeze

# p1
diffs = joltage_diffs(joltages)
puts "p1: diff counts: #{diffs}. answer is #{diffs[1] * diffs[3]}"

# p2
goal = joltages[-1]
reverse_vertexes = build_reverse_vertexes(joltages)
puts "p2: there are #{count_paths_to(goal, reverse_vertexes)} paths to #{goal}"
