#!/usr/bin/env ruby

class Calculator
  def initialize(initial_crabs)
    @initial_crabs = initial_crabs
    @bounds = initial_crabs.min..initial_crabs.max
  end

  # a hash of [start_pos, final_pos] => cost
  def move_costs
    @move_costs ||=
      begin
        Hash[
          @bounds.flat_map do |final_pos|
            @initial_crabs.map do |crab|
              [[crab, final_pos], (final_pos - crab).abs]
            end
          end
        ]
      end
  end

  # return hash of { position: x, fuel: y }
  def best_position
    winner = { position: nil, fuel: nil }

    @bounds.each do |final_pos|
      fuel_cost = @initial_crabs.map { |c| move_costs[[c, final_pos]] }.sum

      if winner[:fuel].nil? || fuel_cost < winner[:fuel]
        winner[:position] = final_pos
        winner[:fuel] = fuel_cost
      end
    end

    winner
  end
end

class Calculator2 < Calculator
  def move_costs
    @move_costs ||=
      begin
        Hash[
          @bounds.flat_map do |final_pos|
            @initial_crabs.map do |crab|
              cost = (1..(final_pos - crab).abs).sum
              [[crab, final_pos], cost]
            end
          end
        ]
      end
  end
end

crabs = File.read(ARGV[0]).split(",").map(&method(:Integer))
p1_ans = Calculator.new(crabs).best_position
puts "p1: Best position is #{p1_ans[:position]}, costs #{p1_ans[:fuel]} fuel"

p2_ans = Calculator2.new(crabs).best_position
puts "p2: Best position is #{p2_ans[:position]}, costs #{p2_ans[:fuel]} fuel"
