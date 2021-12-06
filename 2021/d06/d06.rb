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

    count = 1

    while days > 0
      if fish > 6
        fish -= 2
        days -= 2
      end

      if days >= 7 # every 7 days, fish will have looped and produced offspring at parent + 2
        days -= 7
        count += hsh[[fish + 2, days]]
      else # once less than 7 days left, step singly
        days -= 1
        fish -= 1
        if fish < 0
          count += hsh[[8, days]]
          # we can just break and skip the last few loops, the fish won't spawn
          # again
          break
        end
      end
    end

    hsh[key] = count
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
puts "p1 (p2 imp): after 80 days, there are #{count_fish(init_fish, 80)} fish"

puts "p2: after 256 days, there are #{count_fish(init_fish, 256)} fish"
