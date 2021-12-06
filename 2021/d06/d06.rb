#!/usr/bin/env ruby

def step_1(fish)
  new_fish = []
  fish.map do |f|
    if f == 0
      new_fish << 8
      6
    else
      f - 1
    end
  end + new_fish
end

# Returns a hash that will self-populate & cache keys to calculate how many fish
# a given fish turns into after given days. Hash keys are [fish, days]
def count_cache
  Hash.new do |hsh, key|
    fish, days = *key

    if days <= 0
      hsh[key] = 1
    elsif fish == 0
      hsh[key] = 1 + hsh[[8, days]]
    elsif fish > 6
      hsh[key] = hsh[[fish - 2, days - 2]]
    elsif days >= 7
      hsh[key] = hsh[[fish, days - 7]] + hsh[[fish + 2, days - 7]]
    else # days < 7
      hsh[key] = hsh[[fish - 1, days - 1]]
    end
  end
end

# take an array of fish and return how many will exist after given number of
# days
def count_fish(fish, days)
  calc_cache = count_cache
  fish.map { |f| calc_cache[[f, days]] }.sum
end

init_fish = File.read(ARGV[0]).split(",").map(&method(:Integer))

fish = 80.times.reduce(init_fish) { |fish, _i| step_1(fish) }
puts "p1: after 80 days, there are #{fish.count} fish"
puts "p1 (p2 impl): after 80 days, there are #{count_fish(init_fish, 80)} fish"

puts "p2: after 256 days, there are #{count_fish(init_fish, 256)} fish"
