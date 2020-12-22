#!/usr/bin/env ruby

require "set"

class Game
  attr_reader :p1, :p2

  def self.parse(str)
    p1 = []
    p2 = []
    accum = p1

    str.lines.each do |line|
      if /^\d/ =~ line
        accum << Integer(line)
      elsif /^Player 2:/ =~ line
        accum = p2
      end
    end

    new(p1: p1, p2: p2)
  end

  def initialize(p1:,p2:)
    @p1 = p1
    @p2 = p2
  end

  def eql?(other)
    p1 == other.p1 && p2 == other.p2
  end
  alias_method :==, :eql?
  alias_method :equal?, :eql?

  def hash
    { p1: p1, p2: p2 }.hash
  end

  def game_over?
    !winning_player.nil?
  end

  def winning_player
    if p1.empty?
      :p2
    elsif p2.empty?
      :p1
    end
  end

  def winning_deck
    if p1.empty?
      p2
    elsif p2.empty?
      p1
    end
  end

  def score(deck)
    deck.reverse.each_with_index.map do |c, idx|
      c * (idx + 1)
    end.sum
  end

  def play_round
    c1 = p1.shift
    c2 = p2.shift

    if c1 > c2
      p1 << c1 << c2
    elsif c2 > c1
      p2 << c2 << c1
    else
      raise "ties aren't possible - cards are unique"
    end
  end

  def play_full_game
    play_round until game_over?
    self
  end
end

class Game2 < Game
  def initialize(p1:, p2:)
    super(p1: p1, p2: p2)
  end

  def play_round
    c1, deck1 = p1[0], p1[1..]
    c2, deck2 = p2[0], p2[1..]

    if deck1.count >= c1 && deck2.count >= c2 # recursive combat
      # performance improvement - if p1 has the highest card for the sub-game
      # and it's high enough they can't lose it in another recursive game,
      # you can guarantee they'll always win the recursive game
      recursive_game =
        if deck1.take(c1).max > deck2.take(c2).max && deck1.take(c1).max >= (deck1.count + deck2.count)
          # this is just a stub game where p1 wins
          self.class.new(p1: [42], p2: [])
        else # otherwise simulate the whole game
          self.class.new(
             p1: deck1.take(c1),
             p2: deck2.take(c2),
          ).play_full_game
        end

      if recursive_game.winning_player == :p1
        self.class.new(p1: deck1 + [c1, c2], p2: deck2)
      elsif recursive_game.winning_player == :p2
        self.class.new(p1: deck1, p2: deck2 + [c2, c1])
      else
        raise "something's fucky"
      end
    else # normal scoring applies
      if c1 > c2
        self.class.new(p1: deck1 + [c1, c2], p2: deck2)
      elsif c2 > c1
        self.class.new(p1: deck1, p2: deck2 + [c2, c1])
      else
        raise "ties are not possible - each card value is unique"
      end
    end
  end

  # returns the won Game
  def play_full_game
    seen_states = Set.new
    cur_state = self

    until seen_states.include?(cur_state) || cur_state.game_over?
      seen_states << cur_state
      cur_state = cur_state.play_round
    end

    if seen_states.include?(cur_state) # recursion base case, force p1 to win
      cur_state.p2.clear
    end

    cur_state
  end
end

game = Game.parse(File.read(ARGV[0]))
game.play_full_game
s = game.score(game.winning_deck)
puts "p1: winner is #{game.winning_player} with score #{s}"

game = Game2.parse(File.read(ARGV[0]))
g2 = game.play_full_game
s = game.score(g2.winning_deck)
puts "p2: #{g2.winning_player} won with score #{s}"
