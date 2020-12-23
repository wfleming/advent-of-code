#!/usr/bin/env ruby
#
# sample input: 389125467
# my input: 394618527

class Node
  include Enumerable

  attr_reader :val
  attr_accessor :tail

  def self.construct(vals)
    vals = vals.clone
    head = nil

    while vals.any?
      head = new(val: vals.pop, tail: head)
    end

    # close the loop
    last_node = head.find { |n| n.tail.nil? }
    last_node.tail = head

    head
  end

  def initialize(val:, tail:)
    @val = val
    @tail = tail
  end

  def inspect
    last_tail = find { |n| n.tail.nil? || n.tail == self }
    str = each.map(&:val).join(",")
    if last_tail.nil?
      "#{str},NIL"
    else
      "#{str},LOOP"
    end
  end

  def to_s
    each.map(&:val).join("")
  end

  def each
    if block_given?
      enumerator.each { |node| yield(node) }
    else
      enumerator
    end
  end

  def enumerator
    Enumerator.new do |yielder|
      cur = self
      yielder.yield(cur)
      while !cur.tail.nil? && cur.tail != self
        cur = cur.tail
        yielder.yield(cur)
      end
    end
  end
end

class Game
  attr_accessor :current_cup
  attr_reader :cup_index, :cups_min, :cups_max

  def initialize(current_cup)
    @current_cup = current_cup
    @cup_index = []
    @cups_min = current_cup.val
    @cups_max = current_cup.val
    current_cup.each do |cup|
      cup_index[cup.val] = cup
      @cups_min = cup.val if cup.val < cups_min
      @cups_max = cup.val if cup.val > cups_max
    end
  end

  def move!
    # pick up 3 cups
    move_cups = current_cup.tail
    current_cup.tail = move_cups.tail.tail.tail
    move_cups.tail.tail.tail = nil

    destination_cup = nil
    destination_val = current_cup.val - 1
    while destination_cup.nil?
      if !move_cups.map(&:val).include?(destination_val)
        destination_cup = cup_index[destination_val]
      end

      destination_val -= 1
      destination_val = cups_max if destination_val < cups_min
    end

    # place the cups
    old_destination_tail = destination_cup.tail
    destination_cup.tail = move_cups
    move_cups.tail.tail.tail = old_destination_tail

    self.current_cup = current_cup.tail
  end

  def move_n!(n)
    n.times { self.move! }
  end
end

numbers = ARGV[0].each_char.map(&method(:Integer))

list = Node.construct(numbers)
puts "p1: the list is constructed: #{list.inspect}"
game = Game.new(list)
game.move_n!(100)
cup_1 = game.current_cup.find { |n| n.val == 1 }
puts "p1: after 100 moves, the list after 1 is #{cup_1.to_s[1..]}"

# construct the p2 numbers
LIST_SIZE = 1_000_000
ns_max = numbers.max
p2_numbers = numbers + (LIST_SIZE - numbers.count).times.map { |n| n + 1 + ns_max }
list = Node.construct(p2_numbers)
game = Game.new(list)
game.move_n!(10_000_000)
cup_1 = game.cup_index[1]
star_cups = [cup_1.tail.val, cup_1.tail.tail.val]
puts "p2: stars are under #{star_cups}, product is #{star_cups.reduce(&:*)}"
