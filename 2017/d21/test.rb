#!/usr/bin/env ruby

require "minitest/autorun"
require "minitest/spec"

require_relative "d21"

describe :flip_h do
  it "flips 2x2 square" do
    _(flip_h("#.\n##")).must_equal(".#\n##")
  end

  it "flips 3x3 square" do
    _(flip_h("#..\n.##\n..#")).must_equal("..#\n##.\n#..")
  end
end

describe :flip_v do
  it "flips 2x2 square" do
    _(flip_v("#.\n##")).must_equal("##\n#.")
  end

  it "flips 3x3 square" do
    _(flip_v("#..\n.##\n..#")).must_equal("..#\n.##\n#..")
  end
end

describe :rot_90 do
  it "rotates a 2x2 square" do
    _(rot_90("12\n34")).must_equal("31\n42")
  end

  it "rotates a 2x2 square back to original" do
    orig = "12\n34"
    t = rot_90(rot_90(rot_90(rot_90(orig))))
    _(t).must_equal(orig)
  end

  it "rotates a 3x3 square" do
    _(rot_90("123\n456\n789")).must_equal("741\n852\n963")
  end

  it "rotates a 3x3 square back to original" do
    orig = "123\n456\n789"
    t = rot_90(rot_90(rot_90(rot_90(orig))))
    _(t).must_equal(orig)
  end
end

describe PatternSet do
  it "stores all variations of simple 2x2" do
    ps = PatternSet.parse(["../.# => ##./#../..."])
    # 1 for self, 2 flips, 3 rotations
    # but for this one both flips are equivalent to a rotation
    _(ps.variations.count).must_equal(4)
  end

  it "stores all variations of initial 3x3" do
    ps = PatternSet.parse([".#./..#/### => .##./.#../...."])
    # 1 for self, 2 flips, 3 rotations, all are unique
    _(ps.variations.count).must_equal(6)
  end
  it "stores all variations of simple 2x2" do
    ps = PatternSet.parse(["../.# => ##./#../..."])
    # 1 for self, 2 flips, 3 rotations
    # but for this one both flips are equivalent to a rotation
    _(ps.variations.count).must_equal(4)
  end
end

describe Transformer do
  describe :chunks do
    it "chunks an even sized image" do
      img = "1234\n4567\n89ab\ncdef"
      t = Transformer.new(img, nil)
      _(t.chunks).must_equal(["12\n45", "34\n67", "89\ncd", "ab\nef"])
    end
  end

  describe :output do
    it "transforms a picture" do
      img = ".#.\n..#\n###"
      pats = [
        "../.# => ##./#../...",
        ".#./..#/### => #..#/..../..../#..#",
      ]
      pat_set = PatternSet.parse(pats)
      t = Transformer.new(img, pat_set)
      _(t.output).must_equal("#..#\n....\n....\n#..#")

      t2 = Transformer.new(t.output, pat_set)
      _(t2.output).must_equal("##.##.\n#..#..\n......\n##.##.\n#..#..\n......")
    end
  end
end
