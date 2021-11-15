#!/usr/bin/env ruby

require "delegate"

# hash wrapper that can be sorted to make building permutations easier
class Item < SimpleDelegator
  def <=>(other)
    self[:id] <=> other[:id]
  end
end

WEAPONS = [
  Item.new({id: :dagger, cost: 8, dmg: 4}),
  Item.new({id: :short_sword, cost: 10, dmg: 5}),
  Item.new({id: :warhammer, cost: 25, dmg: 6}),
  Item.new({id: :longsword, cost: 40, dmg: 7}),
  Item.new({id: :greataxe, cost: 74, dmg: 8}),
]

ARMOR = [
  Item.new({id: :leather, cost: 13, armor: 1}),
  Item.new({id: :chainmail, cost: 31, armor: 2}),
  Item.new({id: :splintmail, cost: 53, armor: 3}),
  Item.new({id: :bandedmail, cost: 75, armor: 4}),
  Item.new({id: :platemail, cost: 102, armor: 5}),
]

RINGS = [
  Item.new({id: :dmg1, cost: 25, dmg: 1 }),
  Item.new({id: :dmg2, cost: 50, dmg: 2 }),
  Item.new({id: :dmg3, cost: 100, dmg: 3 }),
  Item.new({id: :def1, cost: 20, armor: 1 }),
  Item.new({id: :def2, cost: 40, armor: 2 }),
  Item.new({id: :def3, cost: 80, armor: 3 }),
]

Combatant = Struct.new(:hp, :dmg, :armor) do
  def self.parse(str)
    ls = str.lines
    hp = ls[0].match(/Hit Points: (\d+)/)[1].to_i
    dmg = ls[1].match(/Damage: (\d+)/)[1].to_i
    armor = ls[2].match(/Armor: (\d+)/)[1].to_i
    self.new(hp, dmg, armor)
  end

  def self.from_loadout(hp, loadout)
    self.new(hp, loadout.dmg, loadout.armor)
  end
end

class Loadout
  def initialize(items)
    @items = items.reject { |i| i[:id] == :empty }
  end

  def to_s
    @items.to_s
  end

  def dmg
    @items.map { |item| item.fetch(:dmg, 0) }.sum
  end

  def armor
    @items.map { |item| item.fetch(:armor, 0) }.sum
  end

  def cost
    @items.map { |i| i[:cost] }.sum
  end
end

# return true if you win, false if boss wins
def analyze_pair(player, boss)
  you_hit = [player.dmg - boss.armor, 1].max
  boss_hit = [boss.dmg - player.armor, 1].max

  turns_till_you_die = (player.hp.to_f / boss_hit).ceil
  turns_till_boss_dies = (boss.hp.to_f / you_hit).ceil

  # player goes first, so = means you win
  turns_till_you_die >= turns_till_boss_dies
end

def all_loadouts
  empty = Item.new({id: :empty})
  armor_opts = [empty] + ARMOR
  ring_opts = ([empty] + RINGS).product([empty] + RINGS).map(&:sort).uniq.reject { |rings| rings[0] == rings[1] }

  return WEAPONS.product(armor_opts).product(ring_opts).map(&:flatten).map(&Loadout.method(:new))
end

def winning_loadouts(player_hp, boss)
  all_loadouts.select { |loadout| analyze_pair(Combatant.from_loadout(player_hp, loadout), boss) }
end

def losing_loadouts(player_hp, boss)
  all_loadouts.reject { |loadout| analyze_pair(Combatant.from_loadout(player_hp, loadout), boss) }
end

PLAYER_HP = 100
BOSS = Combatant.parse(File.read(ARGV[0]))

p1_winner = winning_loadouts(PLAYER_HP, BOSS).min { |l1, l2| l1.cost <=> l2.cost }
puts "p1: winning loadout = #{p1_winner}"
puts "p1: lowest cost to win is #{p1_winner.cost} gold"

p2_loser = losing_loadouts(PLAYER_HP, BOSS).max { |l1, l2| l1.cost <=> l2.cost }
puts "p2: losingest loadout = #{p2_loser}"
puts "p2: highest cost to lose is #{p2_loser.cost} gold"
