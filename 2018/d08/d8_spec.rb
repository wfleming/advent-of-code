require "minitest/autorun"

require "d8"

TEST_NUMS = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".split.map(&:to_i)

describe TreeBuilder do
  it "works" do
    tb = TreeBuilder.new(TEST_NUMS.dup)
    tb.tree.must_equal Node.new(
      [
        Node.new(
          [],
          [10, 11, 12],
        ),
        Node.new(
          [
            Node.new(
              [],
              [99],
            ),
          ],
          [2]
        ),
      ],
      [1, 1, 2]
    )
  end
end

describe "#p1_checksum" do
  it "works" do
    t = TreeBuilder.new(TEST_NUMS.dup).tree
    p1_checksum(t).must_equal 138
  end
end

describe "Node#value" do
  it "finds the root node value from test data" do
    t = TreeBuilder.new(TEST_NUMS.dup).tree
    t.value.must_equal 66
  end

  it "calcs sum of metadata for node without children" do
    n = Node.new(
      [],
      [10, 11, 12],
    )

    n.value.must_equal 33
  end
end
