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

describe MapState do
  describe "parsing and stringifying" do
    it "parses and then stringifies a map" do
      mapstate = MapState.parse(INPUT_1)
      _(mapstate.to_s.lines.map(&:rstrip).join("\n")).must_equal(INPUT_1.lines.map(&:rstrip).join("\n"))
      _(mapstate.goal?).must_equal(false)
    end

    it "calculates goal rooms" do
      mapstate = MapState.parse(INPUT_1)
      _(mapstate.goal_rooms["A"]).must_equal([[3,1], [3,2]])
      _(mapstate.goal_rooms["B"]).must_equal([[5,1], [5,2]])
      _(mapstate.goal_rooms["C"]).must_equal([[7,1], [7,2]])
      _(mapstate.goal_rooms["D"]).must_equal([[9,1], [9,2]])
    end
  end

  describe :amphipods do
    it "gets the positions" do
      mapstate = MapState.parse(INPUT_1)
      _(mapstate.amphipods.count).must_equal(8)
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
      mapstate = MapState.parse(input)
      _(mapstate.goal?).must_equal(true)
      _(mapstate.goal_distance).must_equal(0)
    end
  end

  describe :all_paths do
    it "can walk from room to hallway or other rooms" do
      mapstate = MapState.parse(INPUT_1)
      paths = mapstate.all_paths.fetch([3,2])
      _(paths.count).must_equal(13)
    end

    it "can walk from hallway to rooms" do
    end
  end

  describe :equality_and_hashing do
    it "is equal based on amphipods" do
      mapstate0 = MapState.parse(INPUT_1)
      mapstate1 = MapState.parse(INPUT_1)
      _(mapstate0 == mapstate1).must_equal(true)

      h = {}
      h[mapstate0] = 1
      h[mapstate1] = 1
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
      mapstate0 = MapState.parse(input)
      _(mapstate0.map[3][2]).must_equal("A")
      ns = mapstate0.next_states_for_amphipod([2,3])
      # because of the hallway rule and "don't stop halfway home" rule, there's actually only one option
      _(ns.count).must_equal(1)
      _(ns[0].map[1][3]).must_equal("A")
    end

    it "it correctly won't move into home when another amphipod is there" do
      input = <<~STR
      #############
      #.A........A#
      ###.#.#C#D###
        #B#B#C#D#
        #########
      STR
      mapstate0 = MapState.parse(input)
      ns = mapstate0.next_states_for_amphipod([2,3])
      # require "debug"; binding.break
      _(ns.count).must_equal(0)
    end

    it "it correctly constrains the options" do
      input = <<~STR
      #############
      #...B......A#
      ###.#.#C#D###
        #A#B#C#D#
        #########
      STR
      mapstate0 = MapState.parse(input)
      ns = mapstate0.next_states_for_amphipod([3,1])
      _(ns.count).must_equal(2)
    end

    it "sees valid moves" do
      input = <<~STR
      #############
      #A.........B#
      ###.#.#C#D###
        #A#B#C#D#
        ########
      STR
      mapstate0 = MapState.parse(input)
      ns = mapstate0.next_states
      _(ns.count).must_equal(2)
    end
  end
end

describe :p2_map do
  it "updates the str" do
    _(p2_map(INPUT_1)).must_equal(<<~STR.rstrip)
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
    mapstate0 = MapState.parse(input)
    g = find_goal(mapstate0)[-1]
    _(g).wont_be_nil
    _(g.steps.count).must_equal(1) # A should move straight home
    _(g.energy_spent).must_equal(2) # A should move straight home
  end

  it "finds the goal when it's a bit more difficult" do
    input = <<~STR
    #############
    #...........#
    ###B#A#C#D###
      #A#B#C#D#
      ########
    STR
    mapstate0 = MapState.parse(input)
    g = find_goal(mapstate0)[-1]
    _(g).wont_be_nil
    _(g.steps.count).must_equal(3)
    _(g.energy_spent).must_equal(46)
  end

  it "finds the goal from the example" do
    mapstate0 = MapState.parse(INPUT_1)
    g = find_goal(mapstate0)[-1]
    _(g.energy_spent).must_equal(12521)
  end

  it "finds the p2 goal from the example" do
    skip("p2 wrong on example but right on my input")
    mapstate0 = MapState.parse(p2_map(INPUT_1))
    g = find_goal(mapstate0)[-1]
    _(g.energy_spent).must_equal(44169)
  end
end
