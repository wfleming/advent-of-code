#!/usr/bin/env ruby

require "minitest/autorun"
require "minitest/spec"
require_relative "d13"

describe :separate_instructions do
  it "parses the sample" do
    lines = [
      "6,10",
      "0,14",
      "9,10",
      "0,3",
      "10,4",
      "4,11",
      "6,0",
      "6,12",
      "4,1",
      "0,13",
      "10,12",
      "3,4",
      "3,0",
      "8,4",
      "1,10",
      "2,14",
      "8,10",
      "9,0",
      "",
      "fold along y=7",
      "fold along x=5",
    ]

    dots, instructions = separate_instructions(lines)

    _(instructions).must_equal([
      Fold.new("y", 7),
      Fold.new("x", 5)
    ])

    _(render(dots, empty: ".")).must_equal(
      <<~STR.chomp
      ...#..#..#.
      ....#......
      ...........
      #..........
      ...#....#.#
      ...........
      ...........
      ...........
      ...........
      ...........
      .#....#.##.
      ....#......
      ......#...#
      #..........
      #.#........
      STR
    )
  end
end

describe Fold do
  it "does first fold of sample" do
    dots = [
      Point.new(6,10),
      Point.new(0,14),
      Point.new(9,10),
      Point.new(0,3),
      Point.new(10,4),
      Point.new(4,11),
      Point.new(6,0),
      Point.new(6,12),
      Point.new(4,1),
      Point.new(0,13),
      Point.new(10,12),
      Point.new(3,4),
      Point.new(3,0),
      Point.new(8,4),
      Point.new(1,10),
      Point.new(2,14),
      Point.new(8,10),
      Point.new(9,0),
    ].to_set

    dots2 = Fold.new("y", 7).fold(dots)

    dots2.each do |d|
      puts "Point.new(#{d.x}, #{d.y})"
    end

    _(render(dots2, empty: ".")).must_equal(
      # last 2 blank lines of sample aren't included because I'm only tracking
      # dots
      <<~STR.chomp
        #.##..#..#.
        #...#......
        ......#...#
        #...#......
        .#.#..#.###
      STR
    )
  end

  it "does second fold of sample" do
    dots = [
      Point.new(0, 0),
      Point.new(0, 1),
      Point.new(0, 3),
      Point.new(1, 4),
      Point.new(10, 2),
      Point.new(10, 4),
      Point.new(2, 0),
      Point.new(3, 0),
      Point.new(3, 4),
      Point.new(4, 1),
      Point.new(4, 3),
      Point.new(6, 0),
      Point.new(6, 2),
      Point.new(6, 4),
      Point.new(8, 4),
      Point.new(9, 0),
      Point.new(9, 4),
    ].to_set

    # double check I transcribed those points correctly first
    _(render(dots, empty: ".")).must_equal(
      # last 2 blank lines of sample aren't included because I'm only tracking
      # dots
      <<~STR.chomp
        #.##..#..#.
        #...#......
        ......#...#
        #...#......
        .#.#..#.###
      STR
    )

    dots2 = Fold.new("x", 5).fold(dots)
    _(render(dots2, empty: ".")).must_equal(
      # last 2 blank lines of sample aren't included because I'm only tracking
      # dots
      <<~STR.chomp
        #####
        #...#
        #...#
        #...#
        #####
      STR
    )
  end
end
