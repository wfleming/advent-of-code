#!/usr/bin/env ruby

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

  def game_over?
    !winning_player.nil?
  end

  def winning_player
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
      raise "we weren't told how to handle ties!"
    end
  end

  def play_full_game
    play_round until game_over?
  end
end

game = Game.parse(File.read(ARGV[0]))
game.play_full_game
s = game.score(game.winning_player)
puts "p1: score is #{s}"
