require "minitest/autorun"

require "d13"

SIMPLE = <<~STR
|
v
|
|
|
^
|
STR

SAMPLE = <<~STR
/->-\\
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/
STR

P2_SAMPLE = <<~STR
/>-<\\
|   |
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/
STR

describe State do
  it "figures out cart positions" do
    map = Map.new(SAMPLE.lines.map(&:chomp))
    state = State.new(map)

    state.carts.count.must_equal 2
    state.carts[0].must_equal Cart.new(2, 0, ">")
    state.carts[1].must_equal Cart.new(9, 3, "v")

    # test map filling
    state.map[2, 0].must_equal "-"
    state.map[9, 3].must_equal "|"
  end
end

describe "p1" do
  it "runs straght line until crash" do
    map = Map.new(SIMPLE.lines.map(&:strip))
    state = State.new(map)

    p1(state)

    state.time.must_equal 2
  end

  it "runs full map until crash" do
    map = Map.new(SAMPLE.lines.map(&:strip))
    state = State.new(map)

    crashes = p1(state)

    crashes.must_equal [[7, 3]]
  end
end

describe "p2" do
  it "runs until end" do
    map = Map.new(P2_SAMPLE.lines.map(&:chomp))
    state = State.new(map, delete_crashes: true)

    loc = p2(state)

    loc.must_equal [6, 4]
  end
end
