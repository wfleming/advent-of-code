#!/usr/bin/env ruby

require "minitest/autorun"
require_relative "d22"

describe Cuboid do
  describe :volume do
    it "is correct for a 1x1x1 cube" do
      c = Cuboid.new(1,1,2,2,3,3,true)
      _(c.volume).must_equal(1)
    end

    it "is correct for a 2x2x2 cube" do
      c = Cuboid.new(1,2,2,3,-3,-2,true)
      _(c.volume).must_equal(8)
    end

    it "is correct for a 3x3x3 cube" do
      c = Cuboid.new(10,12,10,12,10,12,true)
      _(c.volume).must_equal(27)
    end
  end

  describe :volume_overlap do
  end
end
