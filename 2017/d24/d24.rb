#!/usr/bin/env ruby

def bridge_strength(bridge)
  bridge.sum(&:sum)
end

def bridge_to_s(bridge)
  bridge.map { |c| c.join("/") }.join("--")
end

def all_bridges(components, cur_bridge = [], only_longest: false)
  next_components =
    if cur_bridge.empty?
      components.select { |c| c.include?(0) }
    else
      tail =
        if cur_bridge.count == 1
          cur_bridge[-1].find { |x| x != 0 }
        elsif cur_bridge[-2].include?(cur_bridge[-1][0])
          cur_bridge[-1][1]
        else
          cur_bridge[-1][0]
        end
      (components - cur_bridge).select { |c| c.include?(tail) }
    end

  if next_components.empty?
    [cur_bridge]
  else
    # sample includes bridges that are shorter strict subsets of other bridges.
    # in practice you never need those - all ports are positive nums, so longer
    # bridges are always stronger. I included the `only_longest` param to test
    # my logic by comparing output to the sample. Not related to p2 "longest
    # overall", this is about "locally longer".
    bridges = next_components.flat_map { |c| all_bridges(components - [c], cur_bridge + [c], only_longest: only_longest) }
    only_longest ? bridges : [cur_bridge] + bridges
  end
end

components = File.readlines(ARGV[0]).map { |l| l.split("/").map(&:to_i) }

bridges = all_bridges(components)
bridges.sort_by!(&method(:bridge_strength))

puts "p1: the strongest bridge is #{bridge_to_s(bridges[-1])} with strength #{bridge_strength(bridges[-1])}"

max_length = bridges.max_by(&:count).count
longest_bridges = bridges.select { |b| b.count == max_length }

puts "p2: the longest bridges have length #{max_length}, there are #{longest_bridges.count} of them, the strongest is #{bridge_to_s(longest_bridges[-1])} with strength #{bridge_strength(longest_bridges[-1])}"
