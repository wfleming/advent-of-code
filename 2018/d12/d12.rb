require "set"

class State
  attr_reader :plants

  def self.from_str(str)
    plants = str.each_char.each_with_index.select do |c, idx|
      c == "#"
    end.map { |p| p[1] }
    new(SortedSet.new(plants))
  end

  def initialize(plants)
    @plants = plants
  end

  def to_s(range = nil)
    range ||= pot_ids
    range.map do |i|
      plants.include?(i) ? "#" : "."
    end.join("")
  end

  def dup
    self.class.new(plants.dup)
  end

  def pot_ids
    arr = plants.to_a
    arr[0]..arr[-1]
  end

  def delete(pot)
    plants.delete(pot)
  end

  def <<(pot)
    plants << pot
  end

  def [](pot)
    plants.include?(pot) ? "#" : "."
  end

  # string rep of pot_id  & context of 2 on either side
  def take5(pot_id)
    ((pot_id - 2)..(pot_id + 2)).map do |id|
      self[id]
    end.compact.join("")
  end
end

class Pots
  def self.parse(input)
    lines = input.lines
    state = lines.shift.gsub("initial state: ", "").strip
    lines.shift
    rules = Hash[*lines.flat_map do |line|
      line.strip.split(" => ")
    end]

    new(State.from_str(state), rules)
  end

  attr_reader :state, :rules, :generation

  def initialize(state, rules)
    @state = state
    @rules = rules
    @generation = 0
  end

  def step
    next_gen = state.dup

    sids = state.pot_ids
    puts "plants=#{state.plants}" if $debug
    consider_ids = (sids.first - 5)..(sids.last + 5)
    consider_ids.each do |pot|
      puts "pot=#{pot} pattern=#{state.take5(pot)}" if $debug
      plant = apply_rules(state.take5(pot))
      if plant == "#"
        next_gen << pot
      else
        next_gen.delete(pot)
      end
      puts "  plant=#{plant} plants=#{state.plants}" if $debug
    end

    @generation += 1
    @state = next_gen
  end

  def to_s(range = -3..35)
    "#{@generation}: #{state.to_s(range)}"
  end

  def apply_rules(candidate)
    rules.each do |pat, result|
      if candidate == pat
        puts "  match #{pat}, return #{result}" if $debug
        return result
      end
    end

    # in sample input, not all rules present: default is no plant
    puts "  no match, returning ." if $debug
    "."
  end
end

def p1(pots)
  20.times { pots.step }

  pots.state.plants.to_a.sum
end

def p2(pots)
  total_gens = 50_000_000_000
  iterations_until_abandon = 10_000
  deltas = []

  iterations_until_abandon.times do
    s = pots.state.plants.to_a.sum
    pots.step
    deltas << pots.state.plants.to_a.sum - s

    if deltas.length > 100 && deltas[-100..-1].uniq.count == 1
      break
    end
  end

  if deltas[-100..-1].uniq.count == 1
    gen_change = deltas[-1]
    puts "stabilized at delta #{gen_change} per generation around generation #{pots.generation}"
    gens_left = total_gens - pots.generation
    puts "current sum is #{pots.state.plants.to_a.sum}, #{gens_left} generations left, for additional #{gens_left * gen_change} plants"
    pots.state.plants.to_a.sum + (gens_left * gen_change)
  else
    raise "couldn't find a stable increase after #{iterations_until_abandon} generations"
  end
end

if $0 == __FILE__
  pots = Pots.parse(File.read(ARGV[0]))

  puts "p1: after 20 gens, sum of pot ids with plants is #{p1(pots.dup)}"
  puts "p2: after 50B gens, sum of pot ids with plants is #{p2(pots.dup)}"
end
