#!/usr/bin/env ruby

require "minitest/autorun"
require_relative "d23"

describe Nanobot do
  it "parses" do
    bot = Nanobot.parse("pos=<1,2,3>, r=4")
    _(bot.pos).must_equal(Point.new(1,2,3))
    _(bot.r).must_equal(4)
  end

  describe :in_range? do
    it "says 2 bots are correctly in range" do
      b1 = Nanobot.new(Point.new(0,0,0), 4)
      b2 = Nanobot.new(Point.new(1,0,0), 1)

      _(b1.in_range?(b1.pos)).must_equal(true) # self is explicitly counted, according to example
      _(b1.in_range?(b2.pos)).must_equal(true)
    end

    it "says 2 bots are correctly out of range" do
      b1 = Nanobot.new(Point.new(0,0,0), 4)
      b2 = Nanobot.new(Point.new(0,5,0), 3)

      _(b1.in_range?(b2.pos)).must_equal(false)
    end
  end
end

describe Cube do
  describe :intersect? do
    it "intersects a bot inside" do
      c = Cube.new(Point.new(0,0,0), 8)
      _(c.intersect?(Nanobot.new(Point.new(2,2,2), 1))).must_equal(true)
    end

    it "intersects a bot outside with sufficient radius" do
      c = Cube.new(Point.new(0,0,0), 8)
      _(c.intersect?(Nanobot.new(Point.new(8,10,10), 4))).must_equal(true)
    end

    it "does not intersects a bot outside with insufficient radius" do
      c = Cube.new(Point.new(0,0,0), 8)
      _(c.intersect?(Nanobot.new(Point.new(10,10,10), 1))).must_equal(false)
    end

    it "negative pos is correct" do
      c = Cube.new(Point.new(-10,-8,-8), 5)

      _(c.intersect?(Nanobot.new(Point.new(-8,-6,-6), 1))).must_equal(true)
      _(c.intersect?(Nanobot.new(Point.new(-8,-6,-9), 3))).must_equal(true)
      _(c.intersect?(Nanobot.new(Point.new(-8,-6,-10), 3))).must_equal(true)

      _(c.intersect?(Nanobot.new(Point.new(-30,-20,-10), 5))).must_equal(false)
      _(c.intersect?(Nanobot.new(Point.new(0,0,30), 5))).must_equal(false)
    end

    # earlier impl gave wrong result for part of pt2 example
    it "sample: wrong intersect" do
      c = Cube.new(Point.new(10,13,13), 3)
      # 13, 16, 16 is max corner
      # from (16,12,12)
      # closest: step 3 to x=13, step 1 to y=13. That's 4, need one more step
      _(c.intersect?(Nanobot.new(Point.new(16,12,12),4))).must_equal(false)
    end
  end
end
