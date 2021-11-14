#!/usr/bin/env ruby

Rule = Struct.new(:subject, :neighbor, :happiness_modifier) do
  PAT = /(?<subject>\w+) would (?<dir>gain|lose) (?<amt>\d+) happiness units by sitting next to (?<neighbor>\w+)\./

  def self.parse(line)
    m = PAT.match(line)
    amt = Integer(m["amt"])
    amt = 0 - amt if m["dir"] == "lose"
    self.new(m["subject"], m["neighbor"], amt)
  end
end

class Seating
  attr_reader :people

  def initialize(people, rules)
    @people = people
    @rules = rules
  end

  def happiness
    @happiness ||= @people.each_with_index.map do |subject, idx|
      neighbor_1 = @people[idx - 1]
      neighbor_2 = @people[idx == @people.length - 1 ? 0 : idx + 1]

      rule_1 = @rules[[subject, neighbor_1]]
      rule_2 = @rules[[subject, neighbor_2]]

      (rule_1&.happiness_modifier || 0) + (rule_2&.happiness_modifier || 0)
    end.sum
  end
end

rules = File.readlines(ARGV[0]).map(&Rule.method(:parse))
names = (rules.map(&:subject) + rules.map(&:neighbor)).uniq
# index rules by [subject, neighbor] for lookup
rules = Hash[
  rules.map { |r| [[r.subject, r.neighbor], r] }
]

seatings = names.permutation.map { |x| Seating.new(x, rules) }
seatings.sort_by!(&:happiness)

puts "p1: best seating has score of #{seatings.last.happiness}"

seatings_2 = (names + ["me"]).permutation.map { |x| Seating.new(x, rules) }
seatings_2.sort_by!(&:happiness)
puts "p2: best seating has score of #{seatings_2.last.happiness}"
