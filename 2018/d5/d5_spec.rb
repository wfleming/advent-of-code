require "minitest/autorun"

require "d5"

describe "#collapse" do
  it "collapses a string" do
    collapse("dabAcCaCBAcCcaDA").must_equal "dabCBAcaDA"
  end
end

describe "#most_efficient" do
  it "finds the winner" do
    most_efficient("dabAcCaCBAcCcaDA").must_equal ['c', 4]
  end
end
