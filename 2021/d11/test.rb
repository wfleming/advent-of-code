#!/usr/bin/env ruby

require "minitest/autorun"
require "minitest/spec"
require_relative "d11"

def parse(str)
  str.lines.map { |l| l.strip.each_char.map(&:to_i) }
end

def o_str(octopi)
  octopi.map(&:join).join("\n")
end

describe "stepping" do
  it "handles 1 in swarm of 9s" do
    o0 = parse(<<~STR)
      11111
      19991
      19191
      19991
      11111
    STR

    actual = step(o0)
    expected = parse(<<~STR)
      34543
      40004
      50005
      40004
      34543
    STR
    _(actual).must_equal(expected)
  end

  it "steps sample 1 generation" do
    o0 = parse(<<~STR)
      5483143223
      2745854711
      5264556173
      6141336146
      6357385478
      4167524645
      2176841721
      6882881134
      4846848554
      5283751526
    STR

    actual = step(o0)
    expected = parse(<<~STR)
      6594254334
      3856965822
      6375667284
      7252447257
      7468496589
      5278635756
      3287952832
      7993992245
      5957959665
      6394862637
    STR
    _(actual).must_equal(expected)
  end

  it "steps sample 2nd generation" do
    o0 = parse(<<~STR)
      6594254334
      3856965822
      6375667284
      7252447257
      7468496589
      5278635756
      3287952832
      7993992245
      5957959665
      6394862637
    STR

    actual = step(o0)
    # last col, below 9 ends up too high?
    expected = parse(<<~STR)
      8807476555
      5089087054
      8597889608
      8485769600
      8700908800
      6600088989
      6800005943
      0000007456
      9000000876
      8700006848
    STR
    _(o_str(actual)).must_equal(o_str(expected))
  end
end
