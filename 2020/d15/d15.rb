#!/usr/bin/env ruby

SEED = File.read(ARGV[0]).split(",").map(&method(:Integer))

class Game
  attr_reader :seed, :ages, :turn, :last_said

  def initialize(seed)
    @seed = seed
    @ages = {} # number spoken => last 2 turns it was spoken
    @last_said = nil
    @turn = 0
  end

  def play_turn!
    n =
      if turn >= seed.count
        if ages[last_said].count == 1
          0
        else
          ages[last_said][1] - ages[last_said][0]
        end
      else # still in the early seed ns
        seed[turn]
      end
    @turn += 1
    ages[n] ||= []
    ages[n] << turn
    ages[n] = ages[n][-2..] if ages[n].count > 2
    @last_said = n
    # puts "DEBUG on turn #{turn}, #{n} was spoken"
  end

  def play_turns!(turn_count)
    turn_count.times { play_turn! }
  end
end

# p1
g = Game.new(SEED)
g.play_turns!(2020)
puts "p1: after 2020 turns, last number spoken was #{g.last_said}"

# p2
g = Game.new(SEED)
g.play_turns!(30_000_000)
puts "p2: after 30M turns, last number spoken was #{g.last_said}"
