#!/usr/bin/env ruby

require "minitest/autorun"
require_relative "d24"

describe Group do
  describe :parse do
    it "parses simple unit" do
      s = "1514 units each with 8968 hit points (weak to cold) with an attack that does 57 bludgeoning damage at initiative 9"
      g = Group.parse("foo", 1, s)
      _(g.units).must_equal(1514)
      _(g.hp_per_unit).must_equal(8968)
      _(g.weaknesses).must_equal([:cold].to_set)
      _(g.immunities).must_equal([].to_set)
      _(g.dmg_type).must_equal(:bludgeoning)
      _(g.dmg_amt).must_equal(57)
      _(g.initiative).must_equal(9)
    end

    it "parses unit without weakness or immunities" do
      s = "1514 units each with 8968 hit points with an attack that does 57 bludgeoning damage at initiative 9"
      g = Group.parse("foo", 1, s)
      _(g.units).must_equal(1514)
      _(g.hp_per_unit).must_equal(8968)
      _(g.weaknesses).must_equal([].to_set)
      _(g.immunities).must_equal([].to_set)
      _(g.dmg_type).must_equal(:bludgeoning)
      _(g.dmg_amt).must_equal(57)
      _(g.initiative).must_equal(9)
    end

    it "parses unit with weaknesses & immunity" do
      s = "2890 units each with 11418 hit points (weak to fire; immune to bludgeoning, slashing) with an attack that does 30 cold damage at initiative 8"
      g = Group.parse("foo", 1, s)
      _(g.units).must_equal(2890)
      _(g.hp_per_unit).must_equal(11418)
      _(g.weaknesses).must_equal([:fire].to_set)
      _(g.immunities).must_equal([:bludgeoning, :slashing].to_set)
      _(g.dmg_type).must_equal(:cold)
      _(g.dmg_amt).must_equal(30)
      _(g.initiative).must_equal(8)
    end
  end
end

