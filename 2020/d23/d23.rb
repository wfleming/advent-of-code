#!/usr/bin/env ruby
#
# sample input: 389125467
# my input: 394618527

class Node
  include Enumerable

  attr_reader :val
  attr_accessor :tail

  def self.construct(vals, first_head: nil)
    if vals.count == 1
      new(val: vals[0], tail: first_head)
    else
      head = new(val: vals[0], tail: nil)
      head.tail = construct(vals[1..], first_head: first_head || head)
      head
    end
  end

  def initialize(val:, tail:)
    @val = val
    @tail = tail
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

  def maximum_val
    each.map(&:val).max
  end

  def minimum_val
    each.map(&:val).min
  end
end

class Game
  attr_accessor :current_cup

  def initialize(current_cup)
    @current_cup = current_cup
  end

  def move!
    # pick up 3 cups
    move_cups = current_cup.tail
    current_cup.tail = move_cups.tail.tail.tail
    move_cups.tail.tail.tail = nil

    destination_cup = nil
    destination_val = current_cup.val - 1

    while destination_cup.nil?
      destination_cup = current_cup.find { |node| node.val == destination_val }
      if destination_cup.nil?
        destination_val -= 1
        destination_val = current_cup.maximum_val if destination_val < current_cup.minimum_val
      end
    end

    # place the cups
    old_destination_tail = destination_cup.tail
    destination_cup.tail = move_cups
    move_cups.tail.tail.tail = old_destination_tail

    self.current_cup = current_cup.tail
  end

  def move_n!(n)
    n.times { self.move!  }
  end
end

numbers = ARGV[0].each_char.map(&method(:Integer))

list = Node.construct(numbers)
puts "the list is constructed: #{list.to_s}"
game = Game.new(list)
game.move_n!(100)
cup_1 = game.current_cup.find { |n| n.val == 1 }
puts "p1: after 100 moves, the list after 1 is #{cup_1.to_s[1..]}"

