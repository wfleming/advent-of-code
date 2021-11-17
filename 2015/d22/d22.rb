#!/usr/bin/env ruby

require_relative "../lib/pqueue"

Player = Struct.new(:hp, :mana)
Boss = Struct.new(:hp, :dmg)

class Game
  SPELL_COSTS = {
    magic_missile: 53,
    drain: 73,
    shield: 113,
    poison: 173,
    recharge: 229,
  }

  attr_accessor :player, :boss, :effects, :next_turn, :mana_spent, :log, :hard_mode

  def initialize(player, boss, hard_mode: false)
    @player = player
    @boss = boss
    @effects = {} # effect_name => timer
    @next_turn = :player
    @mana_spent = 0 # total mana spent by player
    @log = "" # human-readable log of turns in the game so far
    @hard_mode = hard_mode
  end

  def ==(other)
    to_h == other.to_h
  end
  alias :eql? :==

  def to_h
    {player: player.to_h, boss: boss.to_h, effects: effects, next_turn: next_turn, mana_spent: mana_spent}
  end

  def clone
    super.tap do |c|
      c.player = player.clone
      c.boss = boss.clone
      # c.next_turn = next_turn
      c.effects = effects.clone
      # c.mana_spent = mana_spent
      c.log = log.clone
      # c.hard_mode = hard_mode
    end
  end

  def castable_spells
    SPELL_COSTS.keys.filter do |spell|
      SPELL_COSTS[spell] <= player.mana && effects.fetch(spell, 0) == 0
    end
  end

  def next_states
    if defeat? || clone.tap(&:pre_action_phase!).defeat?
      []
    # player can win due to poison, even without enough mana to cast
    elsif (n = clone.tap(&:pre_action_phase!)).victory?
      [n]
    elsif next_turn == :player
      # puts "DEBUG: the castable spells for #{self.to_h} are #{castable_spells}"
      next_base = clone.tap(&:pre_action_phase!)
      next_base.castable_spells.map do |spell|
        next_base.clone.tap do |game|
          game.cast_spell!(spell)
          game.post_action_phase!
        end
      end
    else
      [
        clone.tap do |game|
          game.pre_action_phase!
          dmg = game.boss.dmg
          dmg = [dmg - 7, 1].max if game.effects.fetch(:shield, 0) > 0
          game.log << "Boss attacks for #{dmg} damage\n"
          game.player.hp -= dmg
          game.post_action_phase!
        end,
      ]
    end
  end

  def cast_spell!(spell_name)
    player.mana -= SPELL_COSTS[spell_name]
    self.mana_spent += SPELL_COSTS[spell_name]

    case spell_name
    when :magic_missile
      boss.hp -= 4
      log << "Player casts #{spell_name.to_s.capitalize} for 4 damage\n"
    when :drain
      boss.hp -= 2
      player.hp += 2
      log << "Player casts #{spell_name.to_s.capitalize}; does 2 damage, gains 2 hp\n"
    when :shield
      effects[spell_name] = 6
      log << "Player casts #{spell_name.to_s.capitalize}; gains 7 armor\n"
    when :poison
      effects[spell_name] = 6
      log << "Player casts #{spell_name.to_s.capitalize}\n"
    when :recharge
      effects[spell_name] = 5
      log << "Player casts #{spell_name.to_s.capitalize}\n"
    else
      raise ArgumentErorr, "invalid spell #{spell_name}"
    end
  end

  def pre_action_phase!
    # apply effects, decrement counters
    log << "-- #{next_turn.to_s.capitalize} turn --\n"
    log << "- Player has #{player.hp} hp, #{effects.include?(:shield) ? 7 : 0} armor, #{player.mana} mana\n"
    log << "- Boss has #{boss.hp} hp\n"
    if hard_mode && next_turn == :player
      log << "- Player loses 1 hp (hard mode)\n"
      player.hp -= 1
    end

    return if player.hp <= 0
    effects.each do |key, t|
      effects[key] = t - 1
      case key
      when :poison
        boss.hp -= 3
        log << "#{key.to_s.capitalize} deals 3 damage; timer is now #{t - 1}\n"
      when :recharge
        player.mana += 101
        log << "#{key.to_s.capitalize} grants 101 mana; timer is now #{t - 1}\n"
      else
        log << "#{key.to_s.capitalize}'s timer is now #{t - 1}\n"
      end
    end
  end

  def post_action_phase!
    self.next_turn = next_turn == :player ? :boss : :player
    effects.reject! { |k, v| v == 0 }
    log << "\n"
  end

  def defeat?
    @player.hp <= 0 && @boss.hp >= 0
  end

  def victory?
    @player.hp >= 0 && @boss.hp <= 0
  end
end

def search(start)
  open_set = PQueue.new()
  open_set.push(start, start.mana_spent)

  while open_set.any?
    n = open_set.shift
    return n if n.victory?

    n.next_states.each do |next_state|
      open_set.push(next_state, next_state.mana_spent)
    end
  end

  raise StandardError, "Could not find path to goal"
end

# REAL INPUTS
init_game = Game.new(Player.new(50, 500), Boss.new(51, 9))

# SAMPLE INPUTS
# init_game = Game.new(Player.new(10, 250), Boss.new(14, 8))

winning_game = search(init_game)
puts "p1: spent #{winning_game.mana_spent} to win"

init_game = Game.new(Player.new(50, 500), Boss.new(51, 9), hard_mode: true)
winning_game = search(init_game)
puts "p2: spent #{winning_game.mana_spent} to win"
