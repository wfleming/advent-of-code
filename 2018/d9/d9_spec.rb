require "minitest/autorun"

require "d9"

describe Game do
  describe "#take_turn" do
    it "steps through expected states" do
      g = Game.new(9, 25)

      g.take_turn
      g.current_marble.must_equal 1
      g.current_player.must_equal 1
      g.marbles.must_equal [0, 1]

      g.take_turn
      g.current_marble.must_equal 2
      g.current_player.must_equal 2
      g.marbles.must_equal [0, 2, 1]

      g.take_turn
      g.current_marble.must_equal 3
      g.current_player.must_equal 3
      g.marbles.must_equal [0, 2, 1, 3]

      g.take_turn
      g.current_marble.must_equal 4
      g.current_player.must_equal 4
      g.marbles.must_equal [0, 4, 2, 1, 3]
    end

    it "does the right thing on high-score marbles" do
      g = Game.new(9, 25)

      22.times { g.take_turn}

      g.current_marble.must_equal 22
      g.current_player.must_equal 4
      g.marbles.must_equal [0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15]

      g.take_turn
      g.current_marble.must_equal 19
      g.active_marble.must_equal 23
      g.current_player.must_equal 5
      g.marbles.must_equal [0, 16, 8, 17, 4, 18, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15]
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
