require "minitest/autorun"

require "d22"

describe Cave do
  it "calculates erosion & geologic index" do
    c = Cave.new(510, [10, 10])

    c.geologic_index([0, 0]).must_equal 0
    c.erosion_level([0, 0]).must_equal 510
    c.type([0, 0]).must_equal 0

    c.erosion_level([1, 0]).must_equal 17317
    c.type([1, 0]).must_equal 1

    c.erosion_level([0, 1]).must_equal 8415
    c.type([0, 1]).must_equal 0

    c.erosion_level([1, 1]).must_equal 1805
    c.type([1, 1]).must_equal 2

    c.geologic_index([10, 10]).must_equal 0
    c.erosion_level([10, 10]).must_equal 510
    c.type([10, 10]).must_equal 0
  end
end

describe "#p1" do
  it "gets the right answer" do
    c = Cave.new(510, [10, 10])
    p1(c).must_equal 114
  end
end

describe "Searcher" do
  it "searches out the most effective path" do
    c = Cave.new(510, [10, 10])
    s = Searcher.new(c)

    state = s.search
    state.mins.must_equal 45
  end
end
