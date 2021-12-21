#!/usr/bin/env ruby

class Game
  WIN_SCORE = 1_000

  # a caching hash of [cur_pos, roll] => next_pos
  NEXT_POS = Hash.new do |hsh, k|
    pos, roll = k
    hsh[k] = ((pos + roll - 1) % 10) + 1
  end

  attr_reader :p1pos, :p2pos, :p1score, :p2score, :die_roll_count

  def initialize(p1pos:, p2pos:)
    @p1pos = p1pos
    @p2pos = p2pos
    @p1score = 0
    @p2score = 0
    @next_die_roll = 1
    @die_roll_count = 0
  end

  def die_roll
    v = @next_die_roll
    @next_die_roll = (@next_die_roll % 100) + 1
    @die_roll_count += 1
    v
  end

  def play_game
    while p1score < WIN_SCORE && p2score < WIN_SCORE
      play_turn
    end
  end

  def play_turn
    @p1pos = NEXT_POS[[@p1pos, die_roll + die_roll + die_roll]]
    @p1score += p1pos
    return if p1score >= WIN_SCORE

    @p2pos = NEXT_POS[[@p2pos, die_roll + die_roll + die_roll]]
    @p2score += p2pos
  end
end

Game2State = Struct.new(:p1pos, :p2pos, :p1score, :p2score) do
  WIN_SCORE = 21

  # the possible totals of 3 rolls, and how many ways they can happen
  ROLL_WEIGHTS = {3=>1, 4=>3, 5=>6, 6=>7, 7=>6, 8=>3, 9=>1}

  # calculate from pos => next pos, and how many ways it can happen
  def self.next_pos_weights
    @calc_next_pos_weights ||= begin
      h = {}
      (1..10).each do |start_pos|
        ROLL_WEIGHTS.each do |roll, weight|
          next_pos = ((start_pos + roll - 1) % 10) + 1
          h[start_pos] ||= Hash.new(0)
          h[start_pos][next_pos] += weight
        end
      end
      h
    end
  end

  def winner
    if p1score >= WIN_SCORE
      1
    elsif p2score >= WIN_SCORE
      2
    end
  end

  # a caching hash of state => { next_state => universe count }
  def self.next_states
    @next_states ||= Hash.new do |hsh, k|
      hsh[k] = k.next_state_weights
    end
  end

  def next_state_weights
    states = Hash.new(0)

    p1_states = self.class.next_pos_weights[p1pos].map do |next_pos, weight|
      { p1pos: next_pos, p1score: p1score + next_pos, ucount: weight }
    end

    # p2 states
    p1_states.flat_map do |p1state|
      if p1state[:p1score] >= WIN_SCORE
        states[self.class.new(p1state[:p1pos], p2pos, p1state[:p1score], p2score)] += p1state[:ucount]
      else
        self.class.next_pos_weights[p2pos].map do |next_pos, weight|
          s = self.class.new(p1state[:p1pos], next_pos, p1state[:p1score], p2score + next_pos)
          states[s] += weight * p1state[:ucount]
        end
      end
    end

    states
  end
end

def search(state, universes = 1)
  if state.winner
    { state.winner => universes }
  else
    state.class.next_states[state].reduce(Hash.new(0)) do |memo, next_state_and_universes|
      next_state, next_state_universes = *next_state_and_universes
      search(next_state, universes * next_state_universes).each { |k, v| memo[k] += v }
      memo
    end
  end
end

if __FILE__ == $0
  p1start, p2start = nil, nil
  File.readlines(ARGV[0]).each do |l|
    m = /Player (\d) starting position: (\d+)/.match(l)
    if m[1] == "1"
      p1start = m[2].to_i
    else
      p2start = m[2].to_i
    end
  end

  game = Game.new(p1pos: p1start, p2pos: p2start)
  game.play_game

  puts "p1: game finished. player 1 score = #{game.p1score}, player 2 score = #{game.p2score}, die rolled #{game.die_roll_count} times"
  puts "p1: die roll count * losing score = #{game.die_roll_count * [game.p1score, game.p2score].min}"

  g2init = Game2State.new(p1start, p2start, 0, 0)
  win_counts = search(g2init)
  puts "p2: win counts #{win_counts}"
  if win_counts[1] > win_counts[2]
    puts "p2: player 1 wins more often (#{win_counts[1]} universes)"
  else
    puts "p2: player 2 wins more often (#{win_counts[2]} universes)"
  end
end
