#!/usr/bin/env ruby

require "minitest/autorun"
# require "minitest/spec"
require_relative "d22"

describe Virus do
  it "turns right correctly" do
    _(Virus.new(nil, Point.new(0, 1)).turn_right.dir).must_equal(Point.new(1, 0))
    _(Virus.new(nil, Point.new(1, 0)).turn_right.dir).must_equal(Point.new(0, -1))
    _(Virus.new(nil, Point.new(0, -1)).turn_right.dir).must_equal(Point.new(-1, 0))
    _(Virus.new(nil, Point.new(-1, 0)).turn_right.dir).must_equal(Point.new(0, 1))
  end

  it "turns left correctly" do
    _(Virus.new(nil, Point.new(0, 1)).turn_left.dir).must_equal(Point.new(-1, 0))
    _(Virus.new(nil, Point.new(-1, 0)).turn_left.dir).must_equal(Point.new(0, -1))
    _(Virus.new(nil, Point.new(0, -1)).turn_left.dir).must_equal(Point.new(1, 0))
    _(Virus.new(nil, Point.new(1, 0)).turn_left.dir).must_equal(Point.new(0, 1))
  end
end

describe Map do
  describe :parse do
    it "parses the sample" do
      m = Map.parse(
        <<~STR.chomp.lines
          ..#
          #..
          ...
        STR
      )

      _(m.infected).must_equal(Set.new([Point.new(0, 1), Point.new(2, 2)]))
      _(m.virus.pos).must_equal(Point.new(1, 1))
      _(m.virus.dir).must_equal(Point.new(0, 1))

      puts "DEBUG"
      puts m.to_s
      puts "END DEBUG"
    end
  end
end
