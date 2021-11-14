#!/usr/bin/env ruby

KNOWN_ATTRS = {
  children: 3,
  cats: 7,
  samoyeds: 2,
  pomeranians: 3,
  akitas: 0,
  vizslas: 0,
  goldfish: 5,
  trees: 3,
  cars: 2,
  perfumes: 1,
}

class Sue
  attr_reader :id

  P2_GT_FIELDS = [:cats, :trees]
  P2_LT_FIELDS = [:pomeranians, :goldfish]
  P2_SPECIAL_FIELDS = P2_GT_FIELDS + P2_LT_FIELDS

  def self.parse(line)
    m = /Sue (\d+): (.+)/.match(line)
    id = m[1].to_i
    attrs = Hash[
      m[2].split(", ").map do |s|
        x = s.split(": ")
        [x[0].to_sym, x[1].to_i]
      end
    ]

    self.new(id, attrs)
  end

  def initialize(id, attrs)
    @id = id
    @attrs = attrs
    if @attrs.empty?
      raise ArgumentError, "No attrs given for Sue #{id}"
    end
  end

  def may_match?(other_attrs)
    @attrs.none? do |attr, val|
      KNOWN_ATTRS[attr] != val
    end
  end

  def may_match_p2?(other_attrs)
    if @attrs.any? { |attr, val| !P2_SPECIAL_FIELDS.include?(attr) && KNOWN_ATTRS[attr] != val }
      return false
    end

    P2_GT_FIELDS.each do |attr|
      if @attrs.include?(attr) && @attrs[attr] <= KNOWN_ATTRS[attr]
        return false
      end
    end

    P2_LT_FIELDS.each do |attr|
      if @attrs.include?(attr) && @attrs[attr] >= KNOWN_ATTRS[attr]
        return false
      end
    end

    true
  end
end

sues = File.readlines(ARGV[0]).map(&Sue.method(:parse))

matching_sues = sues.select { |s| s.may_match?(KNOWN_ATTRS) }
if matching_sues.count != 1
  raise StandardError, "Expected to only have 1 sue left, have #{matching_sues.count}"
end
puts "p1: Sue #{matching_sues[0].id} sent the gift"

matching_sues = sues.select { |s| s.may_match_p2?(KNOWN_ATTRS) }
if matching_sues.count != 1
  raise StandardError, "Expected to only have 1 sue left, have #{matching_sues.count}"
end
puts "p2: Sue #{matching_sues[0].id} sent the gift"
