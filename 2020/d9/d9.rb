#!/usr/bin/env ruby

INPUT = File.read(ARGV[0]).lines.map { |l| Integer(l) }
PREAMBLE = Integer(ARGV[1])

class Xmas
  attr_reader :nums, :preamble

  def initialize(nums:, preamble:)
    @nums = nums
    @preamble = preamble
  end

  def find_pair(target, range)
    candidates = nums[range]

    x = candidates.find { |candidate_x| candidates.include?(target - candidate_x) }
    if x
      [x, target - x]
    else
      nil
    end
  end

  def valid?(idx)
    raise ArgumentError, "must be after preamble" if idx < preamble

    find_pair(nums[idx], (idx - preamble)..(idx - 1))
  end

  def find_invalid
    i = (preamble..(nums.count - 1)).find { |idx| !valid?(idx) }

    nums[i]
  end

  def sum_range_from(idx, target)
    s = 0
    nums[idx..].take_while do |n|
      s += n
      s <= target
    end
  end

  def find_contiguous_sum(target)
    nums.each_index.each do |i|
      r = sum_range_from(i, target)

      return r if r.sum == target
    end
  end
end

# p1
xmas = Xmas.new(nums: INPUT, preamble: PREAMBLE)
p2_target = xmas.find_invalid
puts "p1: first invalid entry is #{p2_target}"

# p2
range = xmas.find_contiguous_sum(p2_target)
puts "p2: entire range is #{range}"
puts "p2: smallest=#{range.min} largest=#{range.max} answer=#{range.min + range.max}"
