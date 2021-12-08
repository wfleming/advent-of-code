#!/usr/bin/env ruby

require "set"

DIGITS = {
  "abcefg" => 0,
  "cf" => 1,      # the only 2 segment number
  "acdeg" => 2,
  "acdfg" => 3,
  "bcdf" => 4,    # the only 4 segment number
  "abdfg" => 5,
  "abdefg" => 6,
  "acf" => 7,     # the only 3 segment number
  "abcdefg" => 8, # the only 7 segment number
  "abcdfg" => 9,
}

class Sample
  def self.parse(line)
    inputs, outputs = *line.split(" | ")
    new(inputs.split(" "), outputs.split(" "))
  end

  attr_reader :inputs, :outputs

  def initialize(inputs, outputs)
    @inputs = inputs
    @outputs = outputs
  end

  def decoded_number
    decode! unless defined?(@wires)

    @outputs.map do |o|
      DIGITS.fetch(@digit_mapping.fetch(o.each_char.sort.join(""))).to_s
    end.join("").to_i
  end

  def decode!
    # input wire => output wire possibilities
    @wires = Hash[
      "abcdefg".each_char.map do |w|
        [w, "abcdefg".each_char.to_a.to_set]
      end
    ]

    seed_uniques
    changed = true
    while changed
      wires_before = @wires.clone
      eliminate_supersets
      changed = (wires_before != @wires)
    end

    @digit_mapping = Hash[
      @inputs.map do |digit|
        possibles = DIGITS.keys.to_set & digit_permutations(digit).to_set
        if possibles.count > 1
          raise StandardError, "still > 1 possible digit match"
        end
        [digit.each_char.sort.join(""), possibles.first]
      end
    ]
  end

  # based on current wire possibilites, all possible interpretations of a digit
  def digit_permutations(digit, picked=Set.new)
    return [""] if digit == ""
    (@wires[digit[0]] - picked).flat_map do |possible|
      tail_perms = digit_permutations(digit[1..], picked + [possible])
      tail_perms.map { |t| "#{possible}#{t}".each_char.sort.join("") }
    end.uniq
  end

  def seed_uniques
    @inputs.each do |digit|
      if [2,3,4,7].include?(digit.length)
        equiv = DIGITS.keys.find { |k| k.length == digit.length }
        digit.each_char do |in_wire|
          @wires[in_wire] = @wires[in_wire] & equiv.each_char.to_a.to_set
        end
      end
    end
  end

  def eliminate_supersets
    # if any wire mapping is unique, it can be removed from other wires possibles
    @wires.each do |k1, v1|
      if v1.count == 1
        @wires.each { |k2, v2| @wires[k2] = v2 - v1 if k2 != k1 }
      end
    end

    # if any two wires have the same 2 possibilities (e.g. a => cf, b => cf),
    # then those two can also be removed from other wires
    @wires.map do |k1, v1|
      if v1.count == 2 && @wires.find { |k2, v2| k2 != k1 && v2 == v1 }
        @wires.each { |k2, v2| @wires[k2] = v2 - v1 if v2 != v1 }
      end
    end.compact
  end
end

samples = File.readlines(ARGV[0]).map(&Sample.method(:parse))

p1_ans = samples.map { |s| s.outputs.count { |o| [2,3,4,7].include?(o.length) } }.sum
puts "p1: 1,4,7,8 appear #{p1_ans} times in the outputs"

# samples.each do |s|
#   puts "#{s.inputs.join(" ")} | #{s.outputs.join(" ")} | #{s.decoded_number}"
# end
puts "p2: sum of outputs = #{samples.map(&:decoded_number).sum}"
