#!/usr/bin/env ruby

require "set"

EGGNOG_LITERS = 150

# Wrapping the container makes it easy to support multiples of the same capacity
Container = Struct.new(:id, :capacity) do
  def hsh
    id
  end

  def ==(other)
    id == other.id
  end
  alias :eql? :==
end

# Rather than doing a recursive DFS where each call extends one "frontier"
# combo, this does DFS over all current frontiers so we can quickly
# eliminate/ignore duplicate combos (which are somewhat inevitable).
def find_container_combos(all_containers, combos=nil)
  if combos.nil?
    find_container_combos(all_containers, all_containers.map { |x| Set.new([x]) })
  else
    recurse = false
    next_combos = combos.flat_map do |combo|
      eggnog_stored = combo.map(&:capacity).sum
      eggnog_left = EGGNOG_LITERS - eggnog_stored
      if eggnog_stored == EGGNOG_LITERS # if we've hit the limit, keep it
        combo
      elsif (usable_containers = all_containers.select { |c| !combo.include?(c) && c.capacity <= eggnog_left }).any?
        recurse = true
        usable_containers.map { |c| combo + [c] }
      else # if no more containers we can use, this was a dead end
        nil
      end
    end.compact.uniq

    recurse ? find_container_combos(all_containers, next_combos) : next_combos
  end
end

containers = File.readlines(ARGV[0]).each_with_index.map { |c, idx| Container.new(idx, Integer(c)) }

combos = find_container_combos(containers)
puts "p1: there are #{combos.count} workable container combinations"

min_containers = combos.map(&:count).min
min_combos = combos.select { |c| c.count == min_containers }
puts "p2: there are #{min_combos.count} combos that only use #{min_containers} containers"
