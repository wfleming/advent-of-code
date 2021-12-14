#!/usr/bin/env ruby

Rule = Struct.new(:pair, :insert)

def parse_polymer(lines)
  seed = lines.shift.chomp

  rules = lines.map do |line|
    if (m = /([A-Z][A-Z]) -> ([A-Z])/.match(line))
      Rule.new(m[1], m[2])
    end
  end.compact

  [seed, rules]
end

class PolymerState
  attr_reader :pairs, :chars

  def initialize(polymer)
    @pairs = (0..(polymer.length - 2)).each_with_object(Hash.new(0)) do |i, hsh|
      hsh[polymer[i,2]] += 1
    end
    @chars = polymer.each_char.each_with_object(Hash.new(0)) do |c, hsh|
      hsh[c] += 1
    end
  end

  def clone
    super.tap do |c|
      c.instance_variable_set(:@pairs, pairs.clone)
      c.instance_variable_set(:@chars, chars.clone)
    end
  end

  def length; chars.values.sum; end
  def most_common_char; chars.max_by { |_, v| v }; end
  def least_common_char; chars.min_by { |_, v| v }; end
end

def apply(state, rules)
  new_state = state.clone
  rules.each do |rule|
    cnt = state.pairs[rule.pair]
    if cnt > 0
      pair1 = "#{rule.pair[0]}#{rule.insert}"
      pair2 = "#{rule.insert}#{rule.pair[1]}"
      new_state.pairs[pair1] += cnt
      new_state.pairs[pair2] += cnt
      new_state.pairs[rule.pair] -= cnt
      new_state.pairs.delete(rule.pair) if new_state.pairs[rule.pair] == 0
      new_state.chars[rule.insert] += cnt
    end
  end
  new_state
end

def apply_n(state, rules, n)
  n.times.reduce(state) { |s, _| apply(s, rules) }
end

seed, rules = *parse_polymer(File.readlines(ARGV[0]))
state = PolymerState.new(seed)

state10 = apply_n(state, rules, 10)
puts "p1: after 10 steps, polymer has length #{state10.length}"
puts "p1: most plentiful char is #{state10.most_common_char}, least is #{state10.least_common_char}, difference is #{state10.most_common_char[1] - state10.least_common_char[1]}"

state40 = apply_n(state10, rules, 30)
puts "p1: after 40 steps, polymer has length #{state40.length}"
puts "p2: most plentiful char is #{state40.most_common_char}, least is #{state40.least_common_char}, difference is #{state40.most_common_char[1] - state40.least_common_char[1]}"
