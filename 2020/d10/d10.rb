#!/usr/bin/env ruby

def read_adapters
  File.readlines(ARGV[0]).map { |l| Integer(l) }.sort
end

def joltage_diffs(adapters)
  jumps = Hash.new(0)

  # add the device itself to the adapters
  adapters = adapters.clone
  adapters << adapters[-1] + 3

  # at each idx, calculate diff of current & previous
  adapters.each_index do |idx|
    # wall is 0, so first one is just its own value
    diff =
      if idx == 0
        adapters[idx]
      else
        adapters[idx] - adapters[idx - 1]
      end
    jumps[diff] = jumps[diff] + 1
  end

  jumps
end

# p1
diffs = joltage_diffs(read_adapters)
puts "p1: diff counts: #{diffs}. answer is #{diffs[1] * diffs[3]}"
