require "minitest/autorun"

require "d11"

describe Battery do
  describe "power" do
    it "calculates power" do
      b = Battery.new(3, 5, 8)
      b.power.must_equal(4)

      Battery.new(122, 79, 57).power.must_equal(-5)
      Battery.new(217, 196, 39).power.must_equal(0)
      Battery.new(101, 153, 71).power.must_equal(4)
    end
  end
end

describe "#p1" do
  it "calculates based on the sample serial" do
    winner_coord, winner_power = *p1(cached_power(18))

    winner_coord.must_equal([33, 45])
    winner_power.must_equal(29)
  end
end

describe "AreaCalculator" do
  it "gets expected value" do
    calc = AreaCalculator.from_serial(18)

    calc[[90, 269, 16]].must_equal 113
  end
end
