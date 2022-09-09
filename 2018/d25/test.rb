#!/usr/bin/env ruby

require "minitest/autorun"
require "minitest/spec"
require_relative "d25"

def parse(str)
  str.lines.map(&Point.method(:parse))
end

describe "#p1_constellations" do
  it "handles example 1" do
    pts = parse(<<~STR)
       0,0,0,0
       3,0,0,0
       0,3,0,0
       0,0,3,0
       0,0,0,3
       0,0,0,6
       9,0,0,0
      12,0,0,0
    STR
    cs = p1_constellations(pts)
    _(cs.count).must_equal(2)
  end

  it "handles example 2" do
    pts = parse(<<~STR)
      -1,2,2,0
      0,0,2,-2
      0,0,0,-2
      -1,2,0,0
      -2,-2,-2,2
      3,0,2,-1
      -1,3,2,2
      -1,0,-1,0
      0,2,1,-2
      3,0,0,0
    STR
    cs = p1_constellations(pts)
    _(cs.count).must_equal(4)
  end

  it "handles example 3" do
    pts = parse(<<~STR)
      1,-1,0,1
      2,0,-1,0
      3,2,-1,0
      0,0,3,1
      0,0,-1,-1
      2,3,-2,0
      -2,2,0,0
      2,-2,0,-1
      1,-1,0,-1
      3,2,0,2
    STR
    cs = p1_constellations(pts)
    _(cs.count).must_equal(3)
  end

  it "handles example 4" do
    pts = parse(<<~STR)
      1,-1,-1,-2
      -2,-2,0,1
      0,2,1,3
      -2,3,-2,1
      0,2,3,-2
      -1,-1,1,-2
      0,-2,-1,0
      -2,2,3,-1
      1,2,2,0
      -1,-2,0,-2
    STR
    cs = p1_constellations(pts)
    _(cs.count).must_equal(8)
  end
end
