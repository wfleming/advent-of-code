#!/usr/bin/env ruby

require "minitest/autorun"
require_relative "d23"

INPUT_1 = <<~STR
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
STR

describe Map do
  describe "parsing and stringifying" do
    it "parses and then stringifies a map" do
      map = Map.parse(INPUT_1)
      _(map.to_s.lines.map(&:rstrip).join("\n")).must_equal(INPUT_1.lines.map(&:rstrip).join("\n"))
      _(map.goal?).must_equal(false)
    end

    it "calculates goal rooms" do
      map = Map.parse(INPUT_1)
      _(map.goal_rooms["A"]).must_equal([Point.new(3,1), Point.new(3,2)])
      _(map.goal_rooms["B"]).must_equal([Point.new(5,1), Point.new(5,2)])
      _(map.goal_rooms["C"]).must_equal([Point.new(7,1), Point.new(7,2)])
      _(map.goal_rooms["D"]).must_equal([Point.new(9,1), Point.new(9,2)])
    end
  end

  describe :goal? do
    it "is true" do
      input = <<~STR
      #############
      #...........#
      ###A#B#C#D###
        #A#B#C#D#
        #########
      STR
      map = Map.parse(input)
      _(map.goal?).must_equal(true)
      _(map.goal_distance).must_equal(0)
    end
  end

  describe :equality_and_hashing do
    it "is equal based on amphipods" do
      map0 = Map.parse(INPUT_1)
      map1 = Map.parse(INPUT_1)
      _(map0 == map1).must_equal(true)

      h = {}
      h[map0] = 1
      h[map1] = 1
      _(h.count).must_equal(1)
    end
  end

  describe :next_steps do
    it "it would move all the way home" do
      input = <<~STR
      #############
      #.A........A#
      ###.#B#C#D###
        #.#B#C#D#
        #########
      STR
      map0 = Map.parse(input)
      a = map0.amphipods.find { |a| a.type == "A" && a.pos.x == 2 }
      ns = map0.next_states_for_amphipod(a)
      # because of the hallway rule and "don't stop halfway home" rule, there's actually only one option
      _(ns.count).must_equal(1)
      _(ns[0].amphipods.find { |a2| a2.id == a.id }.pos == Point.new(3,1)).must_equal(true)
    end
  end
end

describe :p2_map do
  it "updates the str" do
    _(p2_map(INPUT_1)).must_equal(<<~STR)
    #############
    #...........#
    ###B#C#B#D###
      #D#C#B#A#
      #D#B#A#C#
      #A#D#C#A#
      #########
    STR
  end
end

describe :find_goal do
  it "finds the goal when already very very close" do
    input = <<~STR
    #############
    #.A.........#
    ###.#B#C#D###
      #A#B#C#D#
      #########
    STR
    map0 = Map.parse(input)
    a = map0.amphipods.find { |a| a.type == "A" && a.pos.x == 2 }
    ns = map0.next_states_for_amphipod(a)
    g = find_goal(map0)
    _(g).wont_be_nil
    _(g.steps.count).must_equal(1) # A should move straight home
    _(g.energy_spent).must_equal(2) # A should move straight home
  end

  it "finds the goal from the example" do
    map0 = Map.parse(INPUT_1)
    g = find_goal(map0)
    _(g[-1].energy_spent).must_equal(12521)
  end

  it "finds the p2 goal from the example" do
    map0 = Map.parse(p2_map(INPUT_1))
    g = find_goal(map0)
    _(g[-1].energy_spent).must_equal(44169)
  end
end
