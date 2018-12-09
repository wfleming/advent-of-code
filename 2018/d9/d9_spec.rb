require "minitest/autorun"

require "d9"

describe Node do
  it "behaves correctly" do
    n = Node.create_list(0)

    # [0]
    n.val.must_equal(0)
    n.left.must_equal(n)
    n.right.must_equal(n)

    # [1, 0]
    n2 = n.insert_before(1)
    n.val.must_equal(0)
    n.left.must_equal(n2)
    n.right.must_equal(n2)
    n2.val.must_equal(1)
    n2.left.must_equal(n)
    n2.right.must_equal(n)

    # [2, 1, 0]
    n3 = n2.insert_before(2)
    n2.left.must_equal(n3)
    n3.val.must_equal(2)
    n3.left.must_equal(n)
    n.right.must_equal(n3)

    # [1, 0]
    nr = n3.remove
    nr.must_equal n2
  end
end

describe Game do
  describe "#take_turn" do
    it "steps through expected states" do
      g = Game.new(9, 25)

      g.take_turn
      g.current_player.must_equal 1
      g.current_marble.to_a.must_equal [1, 0]

      g.take_turn
      g.current_player.must_equal 2
      g.current_marble.to_a.must_equal [2, 1, 0]

      g.take_turn
      g.current_player.must_equal 3
      g.current_marble.to_a.must_equal [3, 0, 2, 1]

      g.take_turn
      g.current_marble.to_a.must_equal [4, 2, 1, 3, 0]
    end

    it "does the right thing on high-score marbles" do
      g = Game.new(9, 25)

      22.times { g.take_turn}

      g.current_marble.val.must_equal 22
      g.active_marble.must_equal 22
      g.current_player.must_equal 4
      g.current_marble.to_a.must_equal [22, 11, 1, 12, 6, 13, 3, 14, 7, 15, 0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5]

      g.take_turn
      g.current_marble.val.must_equal 19
      g.active_marble.must_equal 23
      g.current_player.must_equal 5
      g.current_marble.to_a.must_equal [19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15, 0, 16, 8, 17, 4, 18]
      g.scores[5].must_equal 32 # 23 from marble just played, 9 from marble removed
    end
  end

  describe "#run" do
    it "runs the whole game" do
      g = Game.new(9, 25)
      g.run

      g.scores[5].must_equal 32
    end
  end
end
