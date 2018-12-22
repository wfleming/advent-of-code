require "minitest/autorun"

require "d17"

SAMPLE = <<~STR
x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
STR

describe Scan do
  it "parses" do
    s = Scan.parse(<<~STR)
    x=501, y=2..3
    y=7, x=10..11
    STR

    s[Point.new(501, 2)].must_equal :clay
    s[Point.new(501, 3)].must_equal :clay
    s[Point.new(10, 7)].must_equal :clay
    s[Point.new(11, 7)].must_equal :clay
  end

  it "flows" do
    s = Scan.parse(SAMPLE)

    s.drip_until_full
    # puts "\n\n#{s.viz}\n\n"

    s.water.count.must_equal 57
    s.water.select(&:stable?).count.must_equal 29
  end
end
