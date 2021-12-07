#!/usr/bin/env ruby

require_relative "../../2015/lib/pqueue"

Generator = Struct.new(:id, :chip) do
  def to_s
    "#{id.to_s[0].upcase}G"
  end

  def inspect
    "<#{self.class} #{self.id}>"
  end

  def hash
    @_hash ||= to_s.hash
  end

  def can_move_with?(other)
    other.is_a?(self.class) || other == chip
  end
end

Chip = Struct.new(:id, :generator) do
  def to_s
    "#{id.to_s[0].upcase}M"
  end

  def inspect
    "<#{self.class} #{self.id}>"
  end

  def hash
    @_hash ||= to_s.hash
  end

  def can_move_with?(other)
    other.is_a?(self.class) || other == generator
  end
end

Elevator = Class.new(Object) do
  def to_s
    "E"
  end
end

class State
  attr_reader :items, :elevator

  def self.initial
    floors = File.readlines(ARGV[0]).map do |line|
      line.scan(/a (\w+)(-compatible)? (generator|microchip)/).map do |match_groups|
        case match_groups[-1]
        when "microchip"
          Chip.new(match_groups[0].to_sym, nil)
        when "generator"
          Generator.new(match_groups[0].to_sym, nil)
        end
      end
    end

    floors[0] << Elevator.new

    items = Hash[
      floors.flat_map.with_index do |floor_items, idx|
        floor_items.map { |item| [item, idx + 1] }
      end
    ]

    # connect generators <-> chips so we don't waste time searching later
    items.each do |item, floor|
      if item.is_a?(Chip)
        gen = items.find { |x, _xflr| x.is_a?(Generator) && x.id == item.id }[0]
        item.generator = gen
        gen.chip = item
      end
    end

    self.new(items)
  end

  def initialize(items)
    @items = items # hash of item => floor
    @elevator = items.keys.find { |x| x.is_a?(Elevator) }
  end

  def clone
    self.class.allocate.tap do |c|
      c.instance_variable_set(:@items, items.clone)
      c.instance_variable_set(:@elevator, elevator)
    end
  end

  def ==(other)
    items == other.items
  end
  alias :eql? :==

  def hash
    @_hash ||= items.hash
  end

  def to_s
    thing_cols = items.keys.sort_by(&:to_s)

    (1..4).map do |floor|
      (["F#{floor}"] + thing_cols.map do |thing|
        (items[thing] == floor ? thing.to_s.rjust(2) : ". ")
      end).join("  ")
    end.reverse.join("\n")
  end

  def valid?
    items.none? do |item, floor|
      item.is_a?(Chip) &&
        items[item.generator] != floor &&
        items.any? { |i, f| i.is_a?(Generator) && f == floor }
    end
  end

  def goal_distance
    items.map { |_item, floor| 4 - floor }.sum
  end

  def goal?
    items.all? { |_item, floor| floor == 4 }
  end

  def move!(move_items, dest_floor)
    move_items.each { |i| items[i] = dest_floor }
  end

  # all valid next states
  def next_states
    # elevator can move up or down 1
    # can take 1 - 2 items on its floor
    # can't move into a state with chip X on a floor with gen Y but not gen X
    el_floor = items[elevator]
    movable_items = items.select { |item, floor| item != elevator && floor == el_floor }.keys
    next_floors = [el_floor - 1, el_floor + 1].reject { |i| i < 1 || i > 4 }

    movable_groups = movable_items.map { |i| [i] }.to_a
    movable_items.each.with_index do |i1, i1_idx|
      movable_items.drop(i1_idx + 1).each do |i2|
        movable_groups << [i1, i2] if i1.can_move_with?(i2)
      end
    end

    next_floors.flat_map do |next_floor|
      movable_groups.map do |g|
        clone.tap { |state| state.move!([elevator] + g, next_floor) }
      end.select(&:valid?)
    end
  end
end

# https://en.wikipedia.org/wiki/A*_search_algorithm
def astar_search(init_state)
  open_set = PQueue.new()
  open_set.push(init_state, init_state.goal_distance)

  came_from = {}
  g_scores = {init_state => 0}
  f_scores = {init_state => init_state.goal_distance}

  while open_set.any?
    n = open_set.shift
    # puts "DEBUG: goal_distance=#{n.goal_distance} open_set=#{open_set.count}"

    if n.goal?
      path = [n]
      while came_from.include?(n)
        n = came_from[n]
        path.unshift(n)
      end
      return path
    end

    n.next_states.each do |next_state|
      g_score = g_scores[n] + 1
      if g_score < g_scores.fetch(next_state, Float::INFINITY)
        came_from[next_state] = n
        g_scores[next_state] = g_score
        f_scores[next_state] = g_score + next_state.goal_distance
        open_set.push(next_state, f_scores[next_state]) unless open_set.include?(next_state)
      end
    end
  end

  raise StandardError, "couldn't find path to goal"
end

path = astar_search(State.initial)
puts "p1: took #{path.count - 1} steps to get to goal"

init_state = State.initial
elerium_gen = Generator.new(:elerium, nil)
elerium_gen.chip = Chip.new(:elerium, elerium_gen)
dilithium_gen = Generator.new(:dilithium, nil)
dilithium_gen.chip = Chip.new(:dilithium, dilithium_gen)
[elerium_gen, dilithium_gen].each do |gen|
  init_state.items[gen] = 1
  init_state.items[gen.chip] = 1
end
path = astar_search(init_state)
puts "p2: Took #{path.count - 1} steps to get to goal"
