#!/usr/bin/env ruby

require "set"

$debug = false

def parse_armies(lines)
  id = 1
  army = nil
  groups = []
  while l = lines.shift&.strip
    next if l.empty?
    if /^[A-Za-z ]+:$/ =~ l
      army = l[0..-2]
      id = 1
    else
      groups << Group.parse(army, id, l)
      id += 1
    end
  end

  groups
end

class Group
  def self.parse(army, id, line)
    m = /(?<units>\d+) units each with (?<hp>\d+) hit points (?<attrs>\([^)]+\))? ?with an attack that does (?<dmgamt>\d+) (?<dmgtype>\w+) damage at initiative (?<initiative>\d+)/.match(line)

    weaknesses = Set.new
    immunities = Set.new

    if m[:attrs]
      m[:attrs][1..-2].split(";").each do |attrlist|
        am = /(weak|immune) to (.+)/.match(attrlist)
        if am[1] == "weak"
          weaknesses += am[2].split(",").map(&:strip).map(&:to_sym)
        else
          immunities += am[2].split(",").map(&:strip).map(&:to_sym)
        end
      end
    end

    new(
      army,
      id,
      m[:units].to_i,
      m[:hp].to_i,
      weaknesses,
      immunities,
      m[:dmgtype].to_sym,
      m[:dmgamt].to_i,
      m[:initiative].to_i,
    )
  end

  attr_reader :army, :id, :units, :hp_per_unit, :weaknesses, :immunities, :dmg_type, :dmg_amt, :initiative

  def initialize(army, id, units, hp_per_unit, weaknesses, immunities, dmg_type, dmg_amt, initiative)
    @army = army
    @id = id
    @units = units
    @hp_per_unit = hp_per_unit
    @weaknesses = weaknesses
    @immunities = immunities
    @dmg_type = dmg_type
    @dmg_amt = dmg_amt
    @initiative = initiative
  end

  def dmg_amt=(v)
    @dmg_amt = v
  end

  def to_s
    "#{army} group #{id}"
  end

  def effective_power
    units * dmg_amt
  end

  def damage_from(attacker)
    if immunities.include?(attacker.dmg_type)
      0
    elsif weaknesses.include?(attacker.dmg_type)
      2 * attacker.effective_power
    else
      attacker.effective_power
    end
  end

  def alive?
    units > 0
  end

  def take_dmg(attacker)
    units_killed = [damage_from(attacker) / hp_per_unit, units].min
    puts "  #{attacker} attacks #{self}, killing #{units_killed} units" if $debug
    @units -= units_killed
  end
end

class Battle
  def initialize(groups)
    @groups = groups
    @armies = groups.map(&:army).uniq
  end

  def winner
    if (loser = @armies.find { |army| @groups.select { |g| g.army == army }.none?(&:alive?) })
      @armies.find { |a| a != loser }
    end
  end

  def army_units(army)
    @groups.select { |g| g.army == army }.sum(&:units)
  end

  def fight_battle
    rounds = 0
    while winner.nil? && rounds < 10_000
      rounds += 1
      fight_round
    end
    puts "battle ended in stalemate" if rounds == 10_000 && $debug
  end

  def target_selection_order
    @groups.select(&:alive?).sort do |g1, g2|
      # we want higher values first in both cases, so we compare g2 <=> g1
      if g1.effective_power == g2.effective_power
        g2.initiative <=> g1.initiative
      else
        g2.effective_power <=> g1.effective_power
      end
    end
  end

  # attacker => target
  def selected_targets
    targets = {} # ruby hashes maintain insertion order for enumeration later, so we don't need to track order

    target_selection_order.map do |attacker|
      target = (@groups - targets.values).select do |g|
        g.army != attacker.army && g.alive?
      end.sort do |g1, g2|
        # again, we want larger vals first so we compare g2 <=> g1
        if g1.damage_from(attacker) == g2.damage_from(attacker)
          if g1.effective_power == g2.effective_power
            g2.initiative <=> g1.initiative
          else
            g2.effective_power <=> g1.effective_power
          end
        else
          g2.damage_from(attacker) <=> g1.damage_from(attacker)
        end
      end.first
      targets[attacker] = target if target && target.damage_from(attacker) > 0
    end

    targets
  end

  def fight_round
    puts "start round:" if $debug
    targets = selected_targets
    attack_order = targets.keys.sort_by { |g| 0 - g.initiative }
    attack_order.each do |attacker|
      puts "  #{attacker} will deal #{targets[attacker]} #{targets[attacker].damage_from(attacker)} damage" if attacker.alive? && $debug
      targets[attacker].take_dmg(attacker) if attacker.alive?
    end
    puts "\n" if $debug
  end
end

def apply_boost(groups, boost)
  groups.map do |g|
    g2 = g.clone
    g2.dmg_amt += boost if g.army == "Immune System"
    g2
  end
end

def find_boost(tmpl_groups)
  boost = 0
  until Battle.new(apply_boost(tmpl_groups, boost)).tap(&:fight_battle).winner == "Immune System"
    boost += 1
  end
  boost
end

if __FILE__ == $0
  init_groups = parse_armies(File.readlines(ARGV[0]))

  battle = Battle.new(init_groups.map(&:clone))
  battle.fight_battle

  puts "p1: battle is won by #{battle.winner}, with #{battle.army_units(battle.winner)} remaining units"

  boost = find_boost(init_groups)
  # boost = 84
  battle2 = Battle.new(apply_boost(init_groups, boost))
  battle2.fight_battle

  puts "p2: minimum boost #{boost} leaves #{battle2.winner} the winner with #{battle2.army_units(battle2.winner)} remaining units"
end
