require "minitest/autorun"

require "d18"

SAMPLE = <<~STR
.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.
STR

describe Grid do
  it "can be created and modified" do
    g = Grid.new(2, 2)
    g[Point.new(0, 0)] = 1
    g[Point.new(0, 0)].must_equal 1
    g[Point.new(1, 0)].must_be_nil
    g[Point.new(0, 1)].must_be_nil
    g[Point.new(1, 1)].must_be_nil
  end

  it "can be duped" do
    g = Grid.new(2, 2)
    g[Point.new(0, 0)] = 1

    g2 = g.dup
    g2[Point.new(0, 0)] = 2

    g[Point.new(0, 0)].must_equal 1
    g2[Point.new(0, 0)].must_equal 2
  end

  it "can be enumerated" do
    g = Grid.new(2, 2)
    e = g.each
    g[Point.new(0, 0)] = 1
    e.count.must_equal 4
    e.to_a.must_equal [
      [Point.new(0, 0), 1],
      [Point.new(1, 0), nil],
      [Point.new(0, 1), nil],
      [Point.new(1, 1), nil],
    ]
  end

  it "supports ==" do
    g = Grid.new(2, 2)
    g[Point.new(0, 0)] = 1

    g2 = Grid.new(2, 2)
    g2[Point.new(0, 0)] = 1
    g.must_equal g2

    g2[Point.new(0, 1)] = 1
    g.wont_equal g2
  end
end

describe Forest do
  it "parses and changes" do
    f = Forest.parse(SAMPLE)

    f.viz.must_equal SAMPLE.strip

    f.tick

    f.viz.must_equal <<~EXPECTED.strip
    .......##.
    ......|###
    .|..|...#.
    ..|#||...#
    ..##||.|#|
    ...#||||..
    ||...|||..
    |||||.||.|
    ||||||||||
    ....||..|.
    EXPECTED
  end
end

describe "#p1" do
  it "is correct" do
    f = Forest.parse(SAMPLE)
    r = p1(f)

    r.must_equal [31, 37]
  end
end
