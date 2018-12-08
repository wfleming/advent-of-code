require "minitest/autorun"

require "d6"

TEST_COORDS = parse_coords(<<~COORDS)
1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
COORDS

describe Areas do
  it "calculates" do
    h = Areas.calculate(Grid.new(TEST_COORDS))

    h.must_equal({
      Point.new(1, 1) => Float::INFINITY,
      Point.new(1, 6) => Float::INFINITY,
      Point.new(8, 3) => Float::INFINITY,
      Point.new(3, 4) => 9,
      Point.new(5, 5) => 17,
      Point.new(8, 9) => Float::INFINITY,
    })
  end
end

describe Safe do
  it "finds region" do
    g = Grid.new(TEST_COORDS)
    s = Safe.new(g, dist_cutoff: 32)

    s.region.count.must_equal 16
  end
end
