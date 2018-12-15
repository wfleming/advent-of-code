require "minitest/autorun"

require "d14"

$state_score_buffer = 10_000

describe State do
  it "ticks correctly" do
    s = State.new(SEED_SCORES, ELF_COUNT)

    s.tick
    s.score_count.must_equal(4)
    s.scores[0, s.score_count].must_equal([3, 7, 1, 0])
    s.elves.must_equal([0, 1])

    s.tick
    s.score_count.must_equal(6)
    s.scores[0, s.score_count].must_equal([3, 7, 1, 0, 1, 0])
    s.elves.must_equal([4, 3])
  end
end

describe "find_seq" do
  it "finds sequences" do
    a = [3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1, 5, 8, 9, 1, 6, 7, 7, 9, 2]
    state = State.new(a, ELF_COUNT)
    find_seq(state, "5158916779").must_equal 9
  end
end

describe "p1" do
  it "is right after 9 recipes" do
    s = State.new(SEED_SCORES, ELF_COUNT)
    res = p1(s, 9).join("")

    res.must_equal "5158916779"
  end

  it "is right after 2018 recipes" do
    s = State.new(SEED_SCORES, ELF_COUNT)
    res = p1(s, 2018).join("")

    res.must_equal "5941429882"
  end
end

describe "p2" do
  it "is right for 51589" do
    s = State.new(SEED_SCORES, ELF_COUNT)
    p2(s, "51589").must_equal 9
  end

  it "is right for 59414" do
    s = State.new(SEED_SCORES, ELF_COUNT)
    p2(s, "59414").must_equal 2018
  end
end
