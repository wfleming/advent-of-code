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
end

# p1
xmas = Xmas.new(nums: INPUT, preamble: PREAMBLE)
puts "p1: first invalid entry is #{xmas.find_invalid}"
