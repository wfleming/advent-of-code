#!/usr/bin/env ruby

require "minitest/spec"
require "minitest/autorun"

require_relative "d20"

describe Particle do
  describe :can_turn? do
    it "cannot turn when signs are equal" do
      _(Particle.new(
        1,
        Point.new(0, 0, 0),
        Point.new(1, -1, 3),
        Point.new(1, -8, 7),
      ).can_turn?).must_equal(false)
    end

    it "cannot turn when acceleration is 0" do
      _(Particle.new(
        1,
        Point.new(0, 0, 0),
        Point.new(1, -1, -3),
        Point.new(1, 0, -7),
      ).can_turn?).must_equal(false)
    end

    it "can turn when signs are not equal" do
      _(Particle.new(
        1,
        Point.new(0, 0, 0),
        Point.new(1, -1, 3),
        Point.new(1, 8, 0),
      ).can_turn?).must_equal(true)
    end
  end
end
