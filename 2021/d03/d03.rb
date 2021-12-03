#!/usr/bin/env ruby

NUMS = File.readlines(ARGV[0]).map(&:strip)

def bit_frequencies(nums)
  bit_freqs = nums[0].length.times.map { { "0" => 0, "1" => 0 } }

  nums.each do |num|
    num.each_char.with_index do |bit, idx|
      bit_freqs[idx][bit] += 1
    end
  end

  bit_freqs
end

def gamma(bit_freqs)
  bit_freqs.map do |bit_freq|
    # convert hash to pairs, sort by frequency ascending, grab last (highest
    # frequency) pair, then grab the key
    bit_freq.to_a.sort_by(&:last)[-1][0]
  end.join
end

def epsilon(bit_freqs)
  bit_freqs.map do |bit_freq|
    bit_freq.to_a.sort_by(&:last)[0][0]
  end.join
end

def filter_rating(nums, sort_index, tie_breaker)
  nums[0].length.times.each do |idx|
    # This is a little wasteful - I'm recalculating bit frequencies for every
    # position when I really only need them for the current idx. But I don't
    # feel like refactoring & the dataset is small enough that this is still
    # plenty fast, so I'm leaving it.
    bit_freqs = bit_frequencies(nums)
    target_bit =
      if bit_freqs[idx]["0"] == bit_freqs[idx]["1"]
        tie_breaker
      else
        bit_freqs[idx].to_a.sort_by(&:last)[sort_index][0]
      end

    nums = nums.select { |n| n[idx] == target_bit }

    return nums[0] if nums.count == 1
  end

  raise StandardError, "should have terminated by now, but we still have > 1 number (#{nums})"
end

def oxygen_generator_rating(nums)
  filter_rating(nums, -1, "1")
end

def co2_scrubber_rating(nums)
  filter_rating(nums, 0, "0")
end

bfs = bit_frequencies(NUMS)
gamma_b, epsilon_b = gamma(bfs), epsilon(bfs)
gamma_d, epsilon_d = Integer(gamma_b, 2), Integer(epsilon_b, 2)
puts "p1: Γ = #{gamma_d} (#{gamma_b}), Ε = #{epsilon_d} (#{epsilon_b}), power = #{gamma_d * epsilon_d}"

ogr_b, co2_b = oxygen_generator_rating(NUMS), co2_scrubber_rating(NUMS)
ogr_d, co2_d = Integer(ogr_b, 2), Integer(co2_b, 2)
puts "p2: oxygen generator rating = #{ogr_d} (#{ogr_b}), CO2 scrubber rating = #{co2_d} (#{co2_b}), life support rating = #{ogr_d * co2_d}"
