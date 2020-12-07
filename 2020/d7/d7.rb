#!/usr/bin/env ruby

def get_input
  File.read(ARGV[0]).lines
end

Bag = Struct.new(:adjective, :color) do
  def to_s
    "#{adjective} #{color} bag"
  end
end
InnerBag = Struct.new(:count, :bag) do
  def *(num)
    self.class.new(count * num, bag)
  end
end
Rule = Struct.new(:outer_bag, :inner_bags)

BAG_PAT = /(?<adj>\w+) (?<col>\w+) bags contain (?<contains>.+)/
CONTAIN_PAT = /(?<cnt>\d+) (?<adj>\w+) (?<col>\w+) bags?/

class Regulations
  attr_reader :rules

  def initialize(input_lines)
    @rules = parse_rules(input_lines)
  end

  # => Array of Bags
  def parse_rules(lines)
    lines.map do |l|
      m = BAG_PAT.match(l)
      raise "#{l} - didn't match top-level regex" if m.nil?
      contains =
        if /no other bags/ =~ m[:contains]
          []
        else
          m[:contains].
            split(", ").
            map(&:strip).
            reject(&:empty?).
            map do |cont_str|
              cm = CONTAIN_PAT.match(cont_str)
              raise "#{l} - didn't match inner regex for '#{cont_str}'" if cm.nil?
              InnerBag.new(Integer(cm[:cnt]), Bag.new(cm[:adj], cm[:col]))
            end
        end

      Rule.new(Bag.new(m[:adj], m[:col]), contains)
    end
  end

  # recurses, returns array of all Rules that could contain target_bag
  def rules_containing(target_bag)
    first_level = rules.filter do |r|
      r.inner_bags.any? { |ib| ib.bag == target_bag }
    end

    first_level.flat_map do |r|
      [r] + rules_containing(r.outer_bag)
    end.uniq
  end

  # recurses, returns array of InnerBag
  def bags_inside(target_bag)
    root = rules.find { |r| r.outer_bag == target_bag }

    raise "no rule for #{target_bag}" if root.nil?

    root.inner_bags.
      flat_map do |ib|
        [ib] + bags_inside(ib.bag).map do |ib2|
          # need to scale the inner ones by how many were in the layer up
          ib2 * ib.count
        end
      end
  end
end

# run the script logic

regs = Regulations.new(get_input)
target = Bag.new("shiny", "gold")

# p1
p1_rules = regs.rules_containing(target)
puts "p1: #{p1_rules.count} rules can contain #{target}"

# p2
p2_bags = regs.bags_inside(target)
p2_total = p2_bags.map(&:count).sum
puts "p2: #{p2_total} bags go inside a #{target}"
